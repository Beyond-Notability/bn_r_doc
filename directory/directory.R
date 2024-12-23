### notes ####

# for more about the methods, see: https://beyond-notability.github.io/bn_notes/notes/narratives-from-data.html

# code is lightly commented but some familiarity with R is assumed.
# {glue} is the key R package for inserting data into sentence templates: https://glue.tidyverse.org/


#### libraries-functions-etc ####

# R packages

library(janitor)
library(glue)
library(tidytext)
library(tidyverse)

# for SPARQL queries
# this should be the only package not available from the official R CRAN repository
# remotes::install_github("aourednik/SPARQLchunks")
suppressPackageStartupMessages(library(SPARQLchunks) ) 


## wikibase endpoint URL and prefixes for queries

bn_endpoint <- "https://beyond-notability.wikibase.cloud/query/sparql"

bn_prefixes <- 
  "PREFIX bnwd: <https://beyond-notability.wikibase.cloud/entity/>
PREFIX bnwds: <https://beyond-notability.wikibase.cloud/entity/statement/>
PREFIX bnwdv: <https://beyond-notability.wikibase.cloud/value/>
PREFIX bnwdt: <https://beyond-notability.wikibase.cloud/prop/direct/>
PREFIX bnp: <https://beyond-notability.wikibase.cloud/prop/>
PREFIX bnps: <https://beyond-notability.wikibase.cloud/prop/statement/>
PREFIX bnpq: <https://beyond-notability.wikibase.cloud/prop/qualifier/> 
PREFIX bnpsv: <https://beyond-notability.wikibase.cloud/prop/statement/value/>
PREFIX bnpqv: <https://beyond-notability.wikibase.cloud/prop/qualifier/value/>
PREFIX bnwdref: <https://beyond-notability.wikibase.cloud/reference/>
PREFIX bnpr: <https://beyond-notability.wikibase.cloud/prop/reference/>
PREFIX bnprv: <https://beyond-notability.wikibase.cloud/prop/reference/value/>
"


## wikibase query functions

# a standard query using bn_prefixes and bn_endpoint. sparql will be 'query string *without prefixes*'
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql
  )) |>
    sparql2df(endpoint=bn_endpoint) 
}


# use dplyr::across to extract IDs from URLs for 1 or more cols
# across_cols can be any tidy-select kind of thing; use c() for multiple cols
make_bn_ids <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~str_extract(., "([^/]*$)")))
}


# make date and year columns 
# requires a column named date, in wikibase date format. 
# turns <unknown value> dates into NA before parsing
# but could still fail occasionally so an NA filter might be needed afterwards
make_date_year <-function(data){
  data  |>
    mutate(date = if_else(str_detect(date, "^_:t"), NA, date))  |>
    mutate(date = parse_date_time(date, "ymdHMS"))  |>
    mutate(year = year(date))
}



# add date property labels, inside a mutate 
# (primarily for qualifier dates; sometimes this is easier than getting labels in the sparql query)
date_property_labels <- function(v) {
  case_when(
    {{v}}=="P1" ~ "point in time",
    {{v}}=="P27" ~ "start time",
    {{v}}=="P28" ~ "end time",
    {{v}}=="P53" ~ "earliest date",
    {{v}}=="P51" ~ "latest date"
  )
}




# function to normalise surnames.
# step 1 embracing and name injection https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html 
# step 2 reuse new name https://stackoverflow.com/questions/67142718/embracing-operator-inside-mutate-function
# requires {stringi}
bn_std_surnames <- function(df, col){
  val <- deparse(substitute(col))
  col_std <- paste0(val, "_std")
  
  df |>
    # lowercase
    mutate(!!col_std := str_to_lower({{col}})) |>
    # handle accented letters. 
    mutate(!!col_std := stringi::stri_trans_general(.data[[col_std]], id = "Latin-ASCII"))  |>
    # replace hyphens with a space
    mutate(!!col_std := str_replace_all(.data[[col_std]], "-", " "))  |>
    # remove other punct
    mutate(!!col_std := str_replace_all(.data[[col_std]], "[:punct:]", ""))  |>
    # in case it's created any extra spaces anywhere 
    mutate(!!col_std := str_replace_all(.data[[col_std]], "  +", " "))
}



## gluey functions

# indefinite articles; a or an or nothing
prefix_an <- function(x) {
  case_when(
    # if the string x already starts with the word a/an, don't prefix anything
    str_detect(x, "^an?\b") ~ glue("{x}"),
    # if it starts with a vowel, prefix with "an"; 
    str_detect(x, "^[AEIOUaeiou]") ~ glue("an {x}"),
    # otherwise, as long as it's not *NA*, prefix with "a"; 
    !is.na(x) ~ glue("a {x}"),
    # that should leave only NAs: keep them unchanged
    .default = x
  )
}



# handling NAs when inserting prefixes 
# prefix x with a word/phrase, or make the string "" if NA. 
# NB: default adds insert one space before the prefix, one between prefix and x, but none afterwards.

prefix_na <- function(x, prefix, before=" ", between=" ", after=""){
  if_else(!is.na(x),
          glue("{before}{prefix}{between}{x}{after}"), 
          ""
  )
}


# definite articles

# for most use cases, assumes all names require 'the':
# if name already starts with "The" leave it as it is, otherwise prefix with "the" (or leave as NA).
# before and after args can adjust space before and after: default is to insert a space before and none after
prefix_the <- function(x, before=" ", after=""){
  case_when(
    # if the string x already starts with the word the/The, don't prefix anything
    str_detect(x, "^([Tt]he)\\b") ~ glue("{before}{x}{after}"),
    !is.na(x) ~ glue("{before}the {x}{after}"),
    .default = x
  )
}


# for higher education orgs which often don't require 'the'
# if `category` is "the" use as prefix; otherwise leave as it is.
# doesn't insert any space
definite_hei <- function(x, category){
  case_when(
    category=="zero" ~ x,
    category=="the" ~ prefix_na(x, category, before = ""),
    .default = x
  )
}






#### preparation ####

# for more about fetching data from the BN wikibase see 
# https://beyond-notability.github.io/bn_notes/notes/workflow.html

## dates
# get (almost) every date we have for every woman
# (occasionally there could be dates in linked pages, eg for excavations or events: not hunting for those!)
# dates are complicated: several different date properties, may be in main statements or qualifiers, and in multiple formats

# dates in main statements
# query fetches both wikibase dateTime and EDTF dates, keeps only dateTime values for the latter.
bn_dates_main_sparql <-
  'SELECT distinct ?person  ?date ?date_prop
  WHERE {
   ?person bnwdt:P3 bnwd:Q3 . #women
   FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } # not project team
   ?person ?p ?s .   
      ?date_prop wikibase:claim ?p . 
      ?date_prop wikibase:statementProperty ?ps. 
      ?s ?ps ?date .
   FILTER ( datatype(?date) = xsd:dateTime  ) . 
} # /where
ORDER BY ?person ?date'

bn_dates_main_query <-
  bn_std_query(bn_dates_main_sparql) |>
  make_bn_ids(c(person, date_prop))  |>
  make_date_year() 


# dates in qualifiers. 
bn_dates_qual_sparql <-
  'SELECT distinct ?person  ?date  
WHERE {
    ?person bnwdt:P3 bnwd:Q3 .
    FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } 
    ?person ?p ?s .   
        ?s ?pq ?date .   
          ?qual_date_prop wikibase:qualifier ?pq .
          ?qual_date_prop wikibase:propertyType wikibase:Time.  
} # /where
ORDER BY ?person'

bn_dates_qual_query <-
  bn_std_query(bn_dates_qual_sparql) |>
  make_bn_ids(person)  |>
  make_date_year() 


# combine them
bn_dates_all <-
  bind_rows(
    bn_dates_main_query,
    bn_dates_qual_query
  )


# and get the earliest year in the wikibase for each woman
bn_earliest_years <-
  bn_dates_all |>
  distinct(person, year) |>
  group_by(person) |>
  summarise(earliest = min(year)) |>
  ungroup()


# get birth dates for those who have them
bn_birth_years <-
  bn_dates_main_query |>
  filter(date_prop=="P26") |>
  distinct(person, year) |>
  # very occasionally a woman has had more than one birth date recorded in the wikibase
  # this may have been fixed but ensure you get only the earliest
  group_by(person) |>
  top_n(-1, year) |>
  ungroup() |>
  rename(year_birth = year)



## localities-for-hei

# for use in constructing "the" for HEIs.
# get every `instance of` locality in the wikibase

location_sparql <-
  'SELECT distinct ?locationLabel ?location   
WHERE {  
  ?location bnwdt:P12 bnwd:Q2147 . # i/o locality
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

location_query <-
  bn_std_query(location_sparql) |>
  make_bn_ids(location) |>
  arrange(locationLabel)

location_rgx <-
  location_query |>
  # filter out a few generics that start with lower case letters
  filter(str_detect(locationLabel, "^[A-Z]")) |>
  # remove unwanted detail in some names and then make unique
  mutate(locationLabel = str_trim(str_remove(locationLabel, " *\\((district|village|borough|parish|area|county|unitary authority|civil parish|community|region|settlement|city|council area)\\)"))) |>
  distinct(locationLabel) |>
  # turn them into a regex with | separator
  summarise(rgx = glue_collapse(locationLabel, sep="|")) |>
  # final regex: 
  # must be at the beginning of the name
  # add a few HEI placenames not found in the database
  mutate(rgx = glue("^({rgx}|Chelsea|Clapham|Edmonton|Hampton Court|Harpenden|Harrow|Sheringham|South Kensington|South Tottenham|Welwyn Garden City|Whitechapel|New Cross|North Hackney|Regent Street|Victoria)\\b"))



## hei-orgs 

# items in the database that are higher education institutions.

hei_sparql <-
  'SELECT distinct ?item ?itemLabel  
WHERE {
    ?item bnwdt:P12 bnwd:Q2914 .
 SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
} # /where
ORDER BY ?itemLabel'

hei_query <-
  bn_std_query(hei_sparql) |>
  make_bn_ids(item)


# rules to categorise HEI names as "the" (the name takes the definite article) or "zero" (takes the zero article)
# - if the name starts with a placename = "zero"
# - if it's a "University/College/etc *of*" = "the"
# - any remaining names containing "College" = "zero"
# - most of the remainder = "the"; a few exceptions can be handled separately

hei <-
  hei_query |>
  # filter out a couple of non-specific items. but keep extension centres.
  filter(!item %in% c("Q2485", "Q2916") ) |>
  # categorise as "zero" or "the"
  mutate(the_label = case_when(
    # London exceptions for placenames
    str_detect(itemLabel, "^London (Society for|School of)") ~ "the",
    # starts with a placename
    str_detect(itemLabel, location_rgx$rgx) ~ "zero",
    # University etc of
    str_detect(itemLabel, "(University|University College|Royal College|School|Institute) of") ~ "the",
    # anything else that's a College
    str_detect(itemLabel, "College") ~ "zero",
    # most of the rest are "the"; this fixes a few exceptions
    str_detect(itemLabel, "Greenway Court|Harvard University|^Lady\\b") ~ "zero",
    # everything else
    .default = "the"
  )) |>
  mutate(the_label = definite_hei(itemLabel, the_label)) 



## starter-list 

# get a list of all the women in the wikibase
# plus number of statements (roughly indicates how much info we have about someone)

bn_starter_list_sparql <-
  'SELECT distinct ?person ?personLabel ?statements
WHERE {
   ?person bnwdt:P3 bnwd:Q3 ; # women
         wikibase:statements ?statements . 
   FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}
   SERVICE wikibase:label { 
      bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". 
      } 
}'

bn_starter_list_query <-
  bn_std_query(bn_starter_list_sparql) |>
  make_bn_ids(person) 


## surnames 

# surnames for sorting, nee/aka, etc.
# fetch married and birth surnames from the wikibase (but NB not all women have this data)

bn_surnames_sparql <- 'SELECT ?person ?personLabel ?birth_name ?married_name
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . #select women
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team
  OPTIONAL {?person bnwdt:P140 ?birth_name .} #look for birth name
  OPTIONAL {?person bnwdt:P141 ?married_name .} #look for married name
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?personLabel'


bn_surnames_query <- 
  bn_std_query(bn_surnames_sparql) |>
  make_bn_ids(c(person)) |>
  mutate(across(c(birth_name, married_name), ~na_if(., ""))) |>
  filter(!is.na(birth_name) | !is.na(married_name))


# normalise the surnames and the personLabel column for matching/sorting (all lower case, strip punctuation, etc)
bn_surnames_std <-
  bn_surnames_query  |>
  bn_std_surnames(birth_name) |>
  bn_std_surnames(married_name) |>
  bn_std_surnames(personLabel) 


# a few women married more than once; check how many surnames they have
bn_surnames_count <-
  bn_surnames_std |>
  select(person, birth_name, married_name) |>
  pivot_longer(-person, values_drop_na = TRUE) |>
  distinct(person, name, value) |> 
  count(person, name, sort = T) |>
  pivot_wider(names_from = name, values_from = n) 


bn_surnames <-
  bn_surnames_std |>
  left_join(bn_surnames_count |> rename(birth_name_n=birth_name, married_name_n=married_name), by="person") |>
  add_count(person, name="name_n") 




# sorting rules.
bn_surnames_sort <-
  bn_surnames |>
  mutate(which_name = case_when(
    str_detect(personLabel_std, married_name_std ) & str_detect(personLabel_std, birth_name_std ) ~ "both",
    str_detect(personLabel_std, married_name_std ) ~ "married",
    str_detect(personLabel_std, birth_name_std) ~ "birth"
  )) |>
  arrange(person) |> 
  filter(!is.na(which_name)) |>
  # filter out people who have more than one married surname; use the personLabel fallback
  # hopefully this will pick the project's preferred surname... really ought to find a better way
  filter(name_n==1) |>
  mutate(sort_surname = case_when(
    # don't need to look for both because you'd use married name anyway?
    str_detect(personLabel_std, married_name_std )  ~ married_name,
    str_detect(personLabel_std, birth_name_std)  ~ birth_name,
  )) 


# for women without any surname info, use the last word of personLabel
bn_make_sorting_names <-
  bn_starter_list_query |>
  anti_join(bn_surnames_sort, by=c("person")) |>
  distinct(person, personLabel) |>
  # another hacky tweak for one anonymous person
  mutate(sort_surname = case_when(
    person=="Q1609" ~ personLabel,
    .default = word(personLabel, start=-1L)
  ))



## names-list 

# make names list ordered by surname, personLabel

bn_names_list <-
  bn_starter_list_query |>
  distinct(person, personLabel, statements) |>
  inner_join(
    bind_rows(
      bn_surnames_sort,
      bn_make_sorting_names
    ) |>
      arrange(person) |>
      select(person, sort_surname), by="person"
  ) |>
  # tweak for anon lady cataloguer
  mutate(personLabel = case_when(
    person=="Q1609" ~ str_to_title(personLabel),
    .default = personLabel
  )) |>
  # make surname sort ignore case
  arrange(str_to_lower(sort_surname), personLabel)


# and add birth/earliest dates
bn_names_list_dates <-
  bn_names_list |>
  left_join(bn_birth_years, by="person") |>
  left_join(bn_earliest_years, by="person")




## birth-married-names alternatives

# to make nee/aka statements
# need nee if personLabel uses married name (even if it has birth name as well)
# aka for women who married more than once or whose nee name is used in personLabel *instead of* married name

bn_birth_names <-
  bn_surnames_std |>
  distinct(person, personLabel_std, birth_name, birth_name_std) |>
  filter(!is.na(birth_name)) |>
  mutate(birth_in_label = if_else(str_detect(personLabel_std, birth_name_std), "y", "n"))


bn_married_names <-
  bn_surnames_std |>
  distinct(person, personLabel_std, married_name, married_name_std) |>
  filter(!is.na(married_name)) |>
  add_count(person, name="n_marr_names") |>
  mutate(married_in_label = if_else(str_detect(personLabel_std, married_name_std), "y", "n"))


# list of people who can be ignored in aka/nee processing
bn_names_no_akaing <-
  bind_rows(
    # women who have birth surnames only, no married names (478)
    bn_names_list |>
      semi_join(bn_birth_names, by="person") |>
      anti_join(bn_married_names, by="person") ,
    # women who don't have any surname data (33)
    bn_names_list |>
      anti_join(bn_birth_names, by="person") |>
      anti_join(bn_married_names, by="person") ,
    # women with 1 married name and no birth name (113)
    bn_names_list |>
      inner_join(bn_married_names |> select(person, n_marr_names), by="person") |>
      anti_join(bn_birth_names, by="person") |>
      filter(n_marr_names==1) |>
      select(-n_marr_names)
  )

bn_names_for_akaing <-
  bn_surnames_std |>
  anti_join(bn_names_no_akaing, by="person") |>
  add_count(person, name="rows_n")



bn_nee_names <-
  bn_names_for_akaing |>
  distinct(person, rows_n) |>
  # women who have a birth name  
  inner_join(bn_birth_names, by="person") |>
  # get summary of married name(s).
  left_join(
    bn_married_names |>
      group_by(person, n_marr_names) |> 
      summarise(married_in_label = paste(married_in_label, collapse = ""), .groups = "drop_last") |>
      ungroup(), by="person"
  ) |>
  mutate(needs_nee = case_when(
    # a couple of personLabel have neither! they'll need nee anyway.
    # if y birth_in_label and *no* y in married in label we don't need nee
    birth_in_label=="y" & !str_detect(married_in_label, "y") ~ NA, 
    # otherwise we do need nee
    .default = "y"
  )) |>
  # filter out the ones we don't want.
  filter(needs_nee=="y") |>
  mutate(nee_glue = glue("née {birth_name}")) |>
  select(nee_glue, person)


# aka for married names

bn_aka_names <-
  bn_names_for_akaing |>
  distinct(person) |>
  inner_join(bn_married_names, by="person") |>
  filter(married_in_label=="n") |>
  group_by(person) |>
  summarise(aka_glue = glue_collapse(married_name, sep=", ", last = " and ")) |>
  ungroup() |>
  mutate(aka_glue = glue("a.k.a. {aka_glue}"))




#### 1 beginnings ####


## heading 

# full name (plus Q number and number of statements in the wikibase) 

bn_heading <-
  bn_names_list |>
  mutate(name_id_statements = glue("{personLabel} ({person}: {statements} statements)\n"))  |>
  select(name_id_statements, person) |>
  mutate(order=1.0) 


## first-sentence 

bn_first_sentence <-
  bn_names_list_dates |>
  left_join(bn_nee_names, by="person") |>
  left_join(bn_aka_names, by="person") |>
  # fix personLabel 's for earliest
  mutate(label_glue = case_when(
    is.na(year_birth) & !is.na(earliest) ~ glue("{personLabel}'s"),
    .default = personLabel
  )) |>
  # nee/aka (in brackets) 
  mutate(add_glue = case_when(
    !is.na(nee_glue) & !is.na(aka_glue) ~ glue(" ({nee_glue}, {aka_glue})"),
    !is.na(nee_glue) & is.na(aka_glue) ~ glue(" ({nee_glue})"),
    is.na(nee_glue) & !is.na(aka_glue) ~ glue(" ({aka_glue})"),
    is.na(nee_glue) & is.na(aka_glue) ~ ""
  )) |>
  # make statement
  mutate(statement = case_when(
    !is.na(year_birth) ~ glue("{label_glue}{add_glue} was born in {year_birth}."),
    !is.na(earliest) ~ glue("{label_glue}{add_glue} earliest appearance in Beyond Notability's records is dated {earliest}."),
    .default = glue("Beyond Notability has recorded little information about {label_glue}{add_glue}.")
  )) |>
  select(statement, person) |>
  # order column for sorting statements later
  mutate(order=1.1)


## alternative ident

# alt names are a bit too messy to use in general
# but this is to pull out uses of "possibly" and "probably"

bn_altnames_sparql <-
  '
SELECT DISTINCT ?personLabel ?person  ?altLabel 
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 .
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team 
  
  # add alternative labels (can be multiple, will be duped b/c lang)
  ?person skos:altLabel ?altLabel . 
  
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?personLabel'

bn_altnames_query <-
  bn_std_query(bn_altnames_sparql) |>
  make_bn_ids(person) 

bn_altnames <-
  bn_altnames_query |>
  distinct(personLabel, person, altLabel) |>
  filter(str_detect(altLabel, regex("possibly|probably", ignore_case=TRUE))) |>
  # remove square brackets
  mutate(altLabel = str_remove_all(altLabel, "\\[|\\]")) |>
  # consistency!
  mutate(altLabel = str_replace_all(altLabel, "^P", "p") )


bn_altnames_statements <-
  bn_altnames |>
  mutate(statement = glue("She was {altLabel}.")) |>
  select(statement, person) |>
  mutate(order=1.5)


## maybe-same 

# "may be the same as" P151 

bn_maybe_sparql <-
'SELECT DISTINCT ?personLabel ?person  ?maybeLabel ?maybe
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 .
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team 
  # may be the same as. 
  ?person bnwdt:P151 ?maybe . 
  
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?personLabel'

bn_maybe_query <-
  bn_std_query(bn_maybe_sparql) |>
  make_bn_ids(c(person, maybe))



bn_maybe_statements <-
  bn_maybe_query |>
  mutate(maybe_person = glue("{maybeLabel} ({maybe})")) |>
  # just in case it's ever multiple
  group_by(person, personLabel) |>
  summarise(maybe_person = glue_collapse(maybe_person, sep="; "), .groups = "drop_last") |>
  ungroup() |>
  # insert extra linebreaks at the beginning.
  mutate(statement = glue("She may be the same person as {maybe_person}.")) |>
  select(statement, person) |>
  mutate(order=1.9)



#### 9 ending ####

bn_death_statements <-
  bn_dates_main_query |>
  filter(date_prop=="P15") |>
  distinct(person, year) |>
  group_by(person) |>
  top_n(1, year) |>
  ungroup() |>
  mutate(statement = glue("She died in {year}.") ) |>
  select(statement, person) |>
  mutate(order=9.1)



#### 2 education ####


# educated at P94
# (higher education only Q2914)

bn_educated_dates_sparql <-
'SELECT distinct ?personLabel ?collegeLabel ?universityLabel ?organisedLabel ?date_qual ?date_prop ?s ?college ?university ?organised ?person

WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . #select women
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team
  ?person bnp:P94 ?s .  # educated at
    ?s bnps:P94 ?college .
      ?college bnwdt:P12 bnwd:Q2914 .   # tertiary ed inst
      optional {?college bnwdt:P4 ?university . } # a few college arent part of a university
      optional {?s bnpq:P109 ?organised . } # some extension centres
      #optional {?s bnpq:P60 ?subject . } # leave this.
  # dates. 
         # pit/start/end. there are a few earliest/latest as well.
    optional {
      ?s (bnpq:P1 | bnpq:P27 | bnpq:P28  ) ?date_qual . 
      ?s ?date_prop ?date_qual .
    }
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?personLabel ?collegeLabel ?date_qual'

bn_educated_dates_query <-
  bn_std_query(bn_educated_dates_sparql) |>
  make_bn_ids(c(person, college, university, date_prop, organised, s)) |>
  mutate(across(c(university, organised, universityLabel, organisedLabel, date_qual, date_prop), ~na_if(., ""))) |>
  mutate(date_label = date_property_labels(date_prop)) |>
  # parsed posixct date
  mutate(date = if_else(str_detect(date_qual, "^_:t"), NA, date_qual))  |>
  mutate(date = parse_date_time(date, "ymdHMS"))  |>
  mutate(year = year(date)) |>
  relocate(s, .after = last_col()) 


# single "circa" dates: use point in time/start time; end time if no start time. don't use earliest/latest at all
bn_educated_circa_dates <-
  bn_educated_dates_query |>
  filter(!is.na(year)) |>
  distinct(person, year, date_label, s) |>
  pivot_wider(id_cols = c(person, s), names_from = date_label, values_from = year) |>
  clean_names("snake") |>
  mutate(circa_year = case_when(
    !is.na(point_in_time) ~ point_in_time,
    !is.na(start_time) ~ start_time,
    !is.na(end_time) ~ end_time
  )) |>
  select(s, circa_year)



bn_educated_dates <-
  bn_educated_dates_query |>
  # drop alternative provision without an organised by (should keep extension centres)
  filter(college != "Q2485" | !is.na(organised)) |>
  # extension centre names are in organisedLabel rather than collegeLabel
  mutate(atLabel = case_when(
    college == "Q2485" ~ organisedLabel,
    college != "Q2485" ~ collegeLabel,
  )) |>
  mutate(at = case_when(
    college == "Q2485" ~ organised,
    college != "Q2485" ~ college,
  )) |>
  distinct(person, atLabel, at, s) |>
  left_join(bn_educated_circa_dates, by="s") |>
  # inner join should work here...
  inner_join(hei |> select(item, the_label), by=c("at"="item"))



bn_educated_statements <-
  bn_educated_dates |>
  arrange(person, the_label, circa_year) |>
  # collapse years for multiple at same institution into single row. 
  group_by(person, the_label) |>
  summarise(glue_years = glue_collapse(na.omit(circa_year), sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  # oops. reinstate NAs for blanks so prefix_na will still work
  mutate(glue_years = na_if(glue_years, "")) |>
  arrange(person, the_label, glue_years) |>
  mutate(college_year = prefix_na(glue_years, "c. ", between = "")) |>
  mutate(college_the_year = glue("{the_label}{college_year}")) |>
  group_by(person) |>
  arrange(glue_years, .by_group = T) |>
  summarise(educated_collapse = glue_collapse(college_the_year, ", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(statement = glue("She studied at {educated_collapse}.") ) |>
  select(statement, person) |>
  mutate(order=2.1) 



## degrees 

# academic degree P59 (includes some diplomas and certificates)
# academic qualifications, with awarding institutions and dates where known

bn_academic_degrees_sparql <-
  'SELECT distinct ?person ?degreeLabel ?byLabel ?date ?date_prop ?s ?by ?degree 

WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . #select women
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team
  # academic degree = P59. 
  ?person bnp:P59 ?s .  
    ?s bnps:P59 ?degree . # type of degree
  # P61 conferred by
    optional { ?s bnpq:P61 ?by . }
  # dates.  optional. point in time/start time only. (start time is rare)
  optional {
      ?s (bnpq:P1 | bnpq:P27  ) ?date .
      ?s ?date_prop ?date .
  }
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?personLabel'


bn_academic_degrees_query <-
  bn_std_query(bn_academic_degrees_sparql) |>
  make_bn_ids(c(person, degree, by, date_prop, s)) |>
  mutate(across(c(by, byLabel, date, date_prop), ~na_if(., ""))) |>
  make_date_year() |>
  mutate(date_propLabel = date_property_labels(date_prop)) |>
  relocate(s, .after = last_col())


bn_academic_degrees <-
  bn_academic_degrees_query |>
  # convert uv by/subject to NA
  mutate(across(c(by, byLabel),  ~if_else( str_detect(., "^(_:)?t\\d+$"), NA, . )))  |>
  mutate(degree_level = case_when(
    str_detect(degreeLabel, regex("Baccalaureate|Bachelor", ignore_case=TRUE)) ~ 2,
    str_detect(degreeLabel, regex("master|Lady Literate", ignore_case=TRUE)) ~ 3,
    str_detect(degreeLabel, regex("Doctor", ignore_case=TRUE)) ~ 4,
    .default = 1
  )) |>
  # add the the labels . needs left join
  left_join(hei |> select(by=item, the_label), by="by") 


# levels for sorting
# 1. "other" - diploma, certificate etc. todo: a few diplomas are postgrad... but which?
# 2. bachelor
# 3. master
# 4. doctorate
# occasionally ordering by level will look odd, eg Q111 got a masters in 1939 and a doctorate in 1938. 
# but i think it works better than sorting year first since there are quite a lot of NA dates




bn_degrees_statements <-
  bn_academic_degrees |>
  distinct(person, degreeLabel, the_label, year, degree_level) |>
  arrange(person, degree_level, degreeLabel, year) |>
  # collapse years for multiple of same degree+institution into single row. 
  group_by(person, degreeLabel, the_label, degree_level) |>
  summarise(glue_years = glue_collapse(na.omit(year), sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  # reinstate NAs for blanks so prefix_na will still work!
  mutate(glue_years = na_if(glue_years, "")) |>
  arrange(person, degree_level, glue_years) |>
  # use the_label instead of the original
  mutate(glue_by = prefix_na(the_label, "by")) |>
  mutate(glue_year = prefix_na(glue_years, "in")) |>
  mutate(glue_degree = glue("a {degreeLabel}{glue_by}{glue_year}")) |>
  group_by(person) |>
  summarise(degree_collapse = 
              glue_collapse(glue_degree, ", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(statement= glue("She was awarded {degree_collapse}.")) |>
  select(statement, person) |>
  mutate(order=2.2)




#### 3 fellowships and memberships ####

# most SAL fellows P75 are undated, so you need to get them separately from election data 
# so filter them out of the fellows query. 

# for RAI members there may be a few remaining discrepancies between RAI elections and RAI member of P67
# so you need to fetch both sets and then merge them

# P75 fellow of
bn_fellows_ex_sal_sparql <-
  'select distinct ?person ?personLabel ?fellow ?fellowLabel 
where
{
  ?person bnwdt:P3 bnwd:Q3 .
  ?person bnp:P75 ?s .
    ?s bnps:P75 ?fellow.
  filter not exists { ?s bnps:P75 bnwd:Q8. } # exclude FSAs
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". }
}'
  
  bn_fellows_ex_sal_query <-
    bn_std_query(bn_fellows_ex_sal_sparql) |>
    make_bn_ids(c(person, fellow)) 
  
  
# P67 member of
bn_members_sparql <-
    'select distinct ?person ?personLabel ?member ?memberLabel
where
{
  ?person bnwdt:P3 bnwd:Q3 .
  ?person bnp:P67 ?s .
    ?s bnps:P67 ?member .

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". }
}'
  
bn_members_query <-
    bn_std_query(bn_members_sparql) |>
    make_bn_ids(c(person, member)) 

    
# FSA elections P16 (election proposed by) (successful elections only)
bn_fsa_sparql <-
    'SELECT ?person ?personLabel  ?date ?s
    WHERE { 
      ?person bnwdt:P3 bnwd:Q3.                          
      ?person bnp:P16 ?s .
        ?s bnps:P16 ?SALproposed .
          OPTIONAL {?s bnpq:P1 ?date .}   # in fact should all be dated  
        ?s bnpq:P22 bnwd:Q36 .            # was elected
   SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
   }
   ORDER BY ?date'
  

bn_fsa_query <-
    bn_std_query(bn_fsa_sparql) |>
    make_bn_ids(c(person, s)) |>
    mutate(across(c(date), ~na_if(., ""))) |>
    make_date_year() 
  

# RAI elections P7 (election proposed by) (successful elections only)
bn_rai_sparql <-
    'SELECT ?person ?personLabel  ?date  ?s
WHERE { 
  ?person bnwdt:P3 bnwd:Q3.                          
  ?person bnp:P7 ?s .
  ?s bnps:P7 ?proposed .
    ?s bnpq:P1 ?date .   
    ?s bnpq:P22  bnwd:Q36 .  # elected
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?date'
  
bn_rai_query <-
    bn_std_query(bn_rai_sparql) |>
    make_bn_ids(c(person, s)) |>
    mutate(across(c(date), ~na_if(., ""))) |>
    make_date_year() 
  
  
bn_rai_members <-
    bn_rai_query |>
    mutate(memberLabel="Royal Archaeological Institute") |>
    bind_rows(bn_members_query |> filter(memberLabel=="Royal Archaeological Institute")) |>
    distinct(person, memberLabel)
  
bn_ex_rai_members <-
    bn_members_query |>
    distinct(person, memberLabel) |>
    filter(memberLabel!="Royal Archaeological Institute")
  
  

bn_fsa_statements <-
    bn_fsa_query |>
    distinct(person, personLabel, year) |>
    mutate(statement = glue("She was elected a Fellow of the Society of Antiquaries of London in {year}.")) |>
    select(statement, person) |>
    mutate(order=3.4)

  
  

bn_fellows_ex_sal_statements <-
    bn_fellows_ex_sal_query |>
    distinct(person, fellowLabel, fellow) |>
    left_join(hei |> select(item, the_label), by=c("fellow"="item")) |>
    mutate(the_label = case_when(
      !is.na(the_label) ~ glue(" {the_label}"),
      !is.na(fellowLabel) ~ prefix_the(fellowLabel)
    )) |>
    left_join(bn_fsa_query |> distinct(person) |> mutate(is_fsa="y"), by="person"  ) |>
    group_by(person, is_fsa) |>
    arrange(fellowLabel, .by_group = T) |>
    summarise(fellow_collapse = glue_collapse(the_label, ",", last = " and"), .groups = "drop_last") |>
    ungroup() |> 
    mutate(statement = case_when(
      is_fsa=="y" ~ glue("She was also a Fellow of{fellow_collapse}."),
      .default = glue("She was a Fellow of{fellow_collapse}.")    
    )) |>
    select(statement, person) |>
    mutate(order=3.5) 
  


bn_members_statements <-
    bind_rows(
      bn_rai_members, 
      bn_ex_rai_members
    ) |>
    mutate(member_theLabel = prefix_the(memberLabel)) |> 
    group_by(person) |>
    arrange(memberLabel, .by_group = T) |>
    summarise(member_collapse = glue_collapse(member_theLabel, ",", last = " and"), .groups = "drop_last") |>
    ungroup() |>
    mutate(statement = glue("She was a member of{member_collapse}.") ) |>
    select(statement, person) |>
    mutate(order=3.9) 

  
  
  
  
#### 4 other PPA: publication, donation ####
  
  
# two sources:
# works published in P101
# but P101 is also found as a qualifier of recorded in P76
  
published_sparql <-
    'SELECT  distinct ?person ?pubLabel ?pub
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . # select women
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team
  {
  ?person bnp:P76 ?s.
    ?s bnps:P76 ?in . 
      ?s bnpq:P101 ?pub . 
  }
  union
  {
    ?person bnp:P101 ?s .
    ?s bnps:P101 ?pub .
    }
  
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb, en". } 
}
order by ?person '


published_query <-
  bn_std_query(published_sparql) |>
  make_bn_ids(c(person, pub)) |>
  # make std pub label for variant names, since there are only a couple.
  mutate(pub_label = case_when(
    pubLabel=="Folk-Lore Record" ~ "Folklore",
    pubLabel=="Journal of the Historical and Archaeological Association of Ireland" ~ "Journal of the Royal Society of Antiquaries of Ireland",
    .default = pubLabel
  ))


bn_published_statements <-
  published_query |>
  distinct(person, pub_label) |>
  # *markdown* formatting for journal titles
  mutate(pub_label = glue("*{pub_label}*")) |>
  group_by(person) |>
  arrange(pub_label, .by_group = T) |>
  summarise(pub_collapse = glue_collapse(pub_label, sep=", ", last = " and ")) |>
  ungroup() |>
  mutate(statement = glue("She published in {pub_collapse}.")) |>
  select(statement, person) |>
  mutate(order = 4.1)




## donors 

bn_donor_sparql <-
  'SELECT distinct ?person ?personLabel     ?donatedLabel  ?donated  

WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . # women
  
  ?person bnwdt:P111 ?donated .

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?personLabel ?donatedLabel '

bn_donor_query <-
  bn_std_query(bn_donor_sparql) |>
  make_bn_ids(c(person, donated))


bn_donor <-
  # handle "the" as for HEIs.
  bn_donor_query |>
  # categorise donatedLabel as "zero" or "the"
  mutate(the_label = case_when(
    str_detect(donatedLabel, "^The\\b") ~ "zero",
    # London etc exceptions for placenames
    str_detect(donatedLabel, "^London (Society for|School of)|^Victoria and Albert") ~ "the",
    # starts with a placename
    str_detect(donatedLabel, location_rgx$rgx) ~ "zero",
    # University etc of
    str_detect(donatedLabel, "(University|University College|Royal College|School|Institute) of") ~ "the",
    # anything else that's a College
    str_detect(donatedLabel, "College") ~ "zero",
    # most of the rest are "the"; this fixes a few exceptions
    str_detect(donatedLabel, "Greenway Court|Harvard University|^Lady\\b") ~ "zero",
    # everything else
    .default = "the"
  )) |>
  # use hei function.
  mutate(the_label = definite_hei(donatedLabel, the_label)) 



bn_donor_statements <-
  bn_donor |>
  group_by(person, personLabel) |>
  summarise(donated_orgs = glue_collapse(the_label, sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(statement = glue("She donated objects to {donated_orgs}.")) |>
  select(statement, person) |>
  # before collaborated, i think.
  mutate(order=4.2)


#### 5 relationships with other BN women ####


## collaborations

# collaborated with P83 (can be both main statement and in a qualifier)
# not checking that collaborated with is reciprocal as that is being fixed in the wikibase.

bn_collaborated_sparql <-
  'SELECT DISTINCT ?personLabel ?person  ?collaboratedLabel ?collaborated
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 .
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} #filter out project team 
  {?person bnwdt:P83 ?collaborated .}
  union
  {?person ?p ?s .
     ?s bnpq:P83 ?collaborated .
   }
      ?collaborated bnwdt:P3 bnwd:Q3. # women collaborators only for the moment. to check.
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?personLabel'

bn_collaborated_query <-
  bn_std_query(bn_collaborated_sparql) |>
  make_bn_ids(c(person, collaborated))


bn_collaborated_statements <-
  bn_collaborated_query |>
  mutate(collab_name = glue("{collaboratedLabel} ({collaborated})")) |>
  group_by(person, personLabel) |>
  summarise(collaborators = glue_collapse(collab_name, sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(statement = glue("She is known to have collaborated with {collaborators}.")) |>
  select(statement, person) |>
  # moved after donors. personal relationships to go after this.
  mutate(order=5.1)



## correspondents 

# corresponded with P73. recipient must be another woman
# not checking if P73 is reciprocated (don't want to assume correspondence is always mutual)

bn_corresponded_sparql <-
  'SELECT distinct ?person ?personLabel   ?correspondedLabel  ?corresponded  

WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . # women
  ?person bnwdt:P73 ?corresponded .
  ?corresponded bnwdt:P3 bnwd:Q3 .
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?personLabel ?corresponded'

bn_corresponded_query <-
  bn_std_query(bn_corresponded_sparql) |>
  make_bn_ids(c(person, corresponded))



bn_corresponded_statements <-
  bn_corresponded_query |>
  mutate(correspondent = glue("{correspondedLabel} ({corresponded})")) |>
  group_by(person, personLabel) |>
  summarise(correspondents = glue_collapse(correspondent, sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(statement = glue("She corresponded with {correspondents}.")) |>
  select(statement, person) |>
  mutate(order = 5.2) 



## family 

# close female family members only: mothers, daughters, sisters.

bn_related_sparql <-
  'SELECT distinct ?person ?personLabel   ?p  ?relativeLabel  ?relative  
WHERE {  
  ?person bnwdt:P3 bnwd:Q3 . # women
  ?person ( bnp:P42 | bnp:P45 | bnp:P46  ) ?s .  # sibling (P42) child (P45) mother (P46)
  ?s  ( bnps:P42 | bnps:P45 | bnps:P46  ) ?relative .
  ?person ?p ?s .
  ?relative bnwdt:P3 bnwd:Q3 . # women only
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}
ORDER BY ?personLabel ?relative '

# CBA to get property labels so add them here instead
bn_related_query <-
  bn_std_query(bn_related_sparql) |>
  make_bn_ids(c(person, relative, p)) |>
  mutate(relationship = case_when(
    p=="P42" ~ "sister", # should be reciprocated
    p=="P45" ~ "daughter", # person=mother, relative=daughter. 
    p=="P46" ~ "mother" # relative = mother. person=daughter
  ))


# mother, daughter, sister need to be handled separately given differences in wording. 
# then bind_rows into a single statement

bn_related_mother <-
  bn_related_query |>
  filter(relationship=="mother") |> 
  #add_count(person) |> filter(n>1) # this had better be 0! but what if it isn't?
  mutate(mother = glue("{relativeLabel} ({relative})")) |>
  mutate(sentence = glue("Her mother was {mother}.")) |>
  select(sentence, person) |>
  mutate(order = 5.21)

# this needs glue collapse. and variable verbing etc for "was her daughter"
bn_related_daughters <-
  bn_related_query |>
  filter(relationship=="daughter") |>
  add_count(person, name="n_children") |>
  mutate(daughter = glue("{relativeLabel} ({relative})"))  |>
  group_by(person, personLabel, n_children) |>
  summarise(daughters = glue_collapse(daughter, sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(sentence = case_when(
    n_children==1 ~ glue("{daughters} was her daughter."),
    n_children>1 ~ glue("{daughters} were her daughters.")
  )) |>
  select(sentence, person) |>
  mutate(order = 5.22)

bn_related_sisters <-
  bn_related_query |>
  filter(relationship=="sister") |>
  add_count(person, name="n_sibling") |>
  mutate(sister = glue("{relativeLabel} ({relative})"))  |>
  group_by(person, personLabel, n_sibling) |>
  summarise(sisters = glue_collapse(sister, sep=", ", last = " and "), .groups = "drop_last") |>
  ungroup() |>
  mutate(sentence = case_when(
    n_sibling==1 ~ glue("{sisters} was her sister."),
    n_sibling>1 ~ glue("{sisters} were her sisters.")
  )) |>
  select(sentence, person) |>
  mutate(order = 5.23)

# put them together
bn_related_statements <-
  bind_rows(
    bn_related_mother,
    bn_related_daughters,
    bn_related_sisters
  )  |>
  group_by(person) |>
  # ensure sentences are in the right order!
  arrange(order, .by_group = TRUE) |>
  summarise(statement = glue_collapse(sentence, sep=" ")) |>
  ungroup() |>
  mutate(order=5.3)






#### completion ####

# put them all together in a single paragraph
# (a very few women will have a very long paragraph... but most are too short to break up)

bn_bind_statements <-
  bind_rows(
    bn_first_sentence, 
    bn_altnames_statements,
    bn_maybe_statements,
    bn_educated_statements,
    bn_degrees_statements,
    bn_fsa_statements,
    bn_fellows_ex_sal_statements,
    bn_members_statements,
    bn_published_statements,
    bn_donor_statements,
    bn_collaborated_statements,
    bn_corresponded_statements,
    bn_related_statements,
    bn_death_statements
  ) |>
  group_by(person) |>
  # ensure statements are in the right order
  arrange(order, .by_group = TRUE) |>
  summarise(statements = glue_collapse(statement, sep=" ")) |>
  ungroup()



# NB bn_output is intended for use in an Rmd code chunk with "results='asis'" to handle markdown formatting
# it will not produce the desired output if the R script is run on its own

bn_output <-
bn_heading |>
  left_join(
    bn_bind_statements, by="person"
  ) |>
  # markdown formatting for H3 headings
  glue_data("### {name_id_statements}\n\n{statements}\n\n")
