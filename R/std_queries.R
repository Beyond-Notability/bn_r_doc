## frequently used code for querying the wikibase ####

## call (using source()) *after* shared.R



## sparql packages ####

#SPARQLchunks needs to be installed from github.
#remotes::install_github("aourednik/SPARQLchunks", build_vignettes = TRUE)
suppressPackageStartupMessages(library(SPARQLchunks) ) # can't get chunks working! but it's fine for inline queries.



## endpoint URL ####

bn_endpoint <- "https://beyond-notability.wikibase.cloud/query/sparql"

## prefixes 

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





## functions ####





# a standard query using bn_prefixes and bn_endpoint. 
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql  # 'query string *without prefixes*'
  )) |>
    sparql2df(endpoint=bn_endpoint) # using SPARQLchunks
}



## make a bn_id out of a wikibase item URL. (v is often but not always person; can also be eg item, place, woman, etc)
make_bn_item_id <- function(df, v) {
  df |>
    mutate(bn_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_id)
}

## the same when querying for properties rather than items
make_bn_prop_id <- function(df, v) {
  df |>
    mutate(bn_prop_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_prop_id)
}


## use across() to extract IDs from wikibase URIs for Items/Properties for 1 or more cols, no renaming or relocating
# across_cols can be any tidy-select kind of thing eg c(var1, var2) / var1:var5 / -var3 / starts_with("abc")
# NB sometimes content can be mixed free text/wikibase Items

make_bn_ids <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~str_extract(., "([^/]*$)")))
}


## turn <unknown value>s into NAs. 
uv_to_na_across <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~if_else(str_detect(., "^(_:)?t\\d+$"), NA, . ) ))
}


## make union/values queries (usually for querying a subset of previous query results)

# construct lists of IDs for union/values query; bn_id is default ID column but can name another.
# nb will still need to be enclosed in appropriate brackets in the sparql.
bn_make_union <- function(data, bn_id=bn_id){
  data |>
    mutate(bn_bnwd = paste0("bnwd:",{{bn_id}})) |> # for VALUES
    mutate(bn_bnwdt = paste0("bnwdt:",{{bn_id}})) |>
    mutate(bn_bnp = paste0("bnp:",{{bn_id}})) |>
    mutate(bn_bnps = paste0("bnps:", {{bn_id}})) |>
    # construct the contents of the query (add to sparql with data$thing or pull(thing))
    summarise(
              bn_bnwd_values = paste(unique(bn_bnwd), collapse = " "), # list for VALUES
              bn_bnp_union = paste(unique(bn_bnp), collapse = " | "), # shorthand union query (using |) - usually needs to be followed by  bn_bnps_union
              bn_bnps_union = paste(unique(bn_bnps), collapse = " | "), # shorthand union query
              bn_bnwdt_union = paste(unique(bn_bnwdt), collapse = " | ") 
              ) 
}



## use glue() to build query string eg for VALUES query or shorthand union query
# improved! RTFM and discovered .open and .close. options for glue
# spql = the VALUES sparql query string from WQS; need to insert "glue_values" placeholder  
# default values = bn_bnwd_values for use with bn_make_union, but could be any list of Ps or Qs to go into a sparql.
# also a ptential template for more complex replacements... 

mutate_glue_sparql <- function(data, spql, values=bn_bnwd_values){
  data |>
    mutate(s = glue(
      spql,
      .open = "<<", .close = ">>"
    )) |>
    pull(s)
}

## example usage
# example_spql <- 
#   'select distinct ?item ?itemLabel ?location ?locationLabel ?wd
#     where {
#       values ?item { <<bn_bnwd_values>> }
#       ?item bnwdt:P2 ?location .
#       optional {?location bnwdt:P117 ?wd .}
#     SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
#     }'
#
# example_sparql <-
# work_loc_query |>
#   filter(!is.na(location)) |>
#   bn_make_union(location) |>
#   mutate_glue_sparql(example_spql)
#
# example_query <- bn_std_query(example_sparql)




## std reference queries ####

## all the properties in the wikibase with label and description.
bn_properties <-
  c("SELECT DISTINCT ?property ?propertyType ?propertyLabel  ?propertyDescription  # ?propertyAltLabel
      WHERE {
        ?property a wikibase:Property ;
              rdfs:label ?propertyLabel ;
              wikibase:propertyType ?propertyType .

      # OPTIONAL { ?property skos:altLabel ?propertyAltLabel . } # not many of these

      OPTIONAL {SERVICE wikibase:label { bd:serviceParam wikibase:language '[AUTO_LANGUAGE], en-gb, en'.
              ?property schema:description ?propertyDescription . }
      } 
  
      FILTER(LANG(?propertyLabel) = 'en') 
    }
    order by ?propertyLabel") |>
  sparql2df(endpoint=bn_endpoint) |>
  make_bn_prop_id(property) |>
#  mutate(bn_id = str_extract(property, "\\bP\\d+$")) |>
  mutate(property_type = str_extract(propertyType, "[A-Za-z]+$")) |>
  relocate(property_type, .after = bn_prop_id) |>
  relocate(property, propertyType, .after = last_col())  |>
  mutate(propertyDescription = na_if(propertyDescription, "")) |>
  arrange(parse_number(str_extract(bn_prop_id, "\\d+"))) 


# std triples, filters etc 
# haven't used these much as it turns out, but keep for reference

bn_triple_woman <- "?person bnwdt:P3 bnwd:Q3 . " # get women
bn_filter_project <- "FILTER NOT EXISTS { ?person bnwdt:P4 bnwd:Q12 . } " # filter out project team
wb_service_label <- 'SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE], en, en-gb". }' # labels


## make a list of all women in the wikibase with birth/death dates if we have them.
bn_women_list_sparql <-
  'SELECT distinct ?person ?personLabel ?statements ?dob ?dod
WHERE {
   ?person bnwdt:P3 bnwd:Q3 ;
         wikibase:statements ?statements .
   FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}

      optional { ?person bnwdt:P15 ?dod .   }
      optional { ?person bnwdt:P26 ?dob .   }

    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
}'

## minimal processing to use as a basic list of all women or for dob/dod
# this has (or had) some dups because a few women had more than one birth date

bn_women_list_query <-
  bn_std_query(bn_women_list_sparql) |>
  make_bn_item_id(person) |>
  relocate(bn_id, personLabel) |>
  mutate(across(c(dob, dod), ~na_if(., ""))) |>
  arrange(parse_number(str_remove(bn_id, "Q")))


# dedup the list
bn_women_list <-
  bn_women_list_query |>
  distinct(bn_id, personLabel, statements, person) 



# process dates of birth/death and dedup. 
bn_women_dob_dod <-
  bn_women_list_query |>
  filter(!is.na(dob) | !is.na(dod)) |>
  mutate(across(c(dob, dod), ~parse_date_time(., "ymdHMS"), .names = "bn_{.col}")) |>
  mutate(across(c(bn_dob, bn_dod), year, .names = "{.col}_yr")) |>
  select(-dob, -dod) |>
  # only one row per person please (use the earliest)
  group_by(bn_id) |>
  top_n(1, row_number()) |>
  ungroup() 








## spql templates ####


# NB use of <<values>> (with .open=<< and .close=>> in glue function), so {} in sparql don't clash with glue defaults.
# bn_bnwd_values is created in bn_make_union() function

## linked P2 locations. may occasionally get multis.
bn_linked_p2_spql <-
  'select distinct ?item ?itemLabel ?location ?locationLabel ?wd
  where {
    values ?item { <<bn_bnwd_values>> }
  ?item bnwdt:P2 ?location .
  optional {?location bnwdt:P117 ?wd .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }' 


## linked P33 admin territories. could get multis.
bn_linked_p33_spql <-
  'select distinct ?item ?itemLabel ?admin ?adminLabel ?wd
  where {
    values ?item { <<bn_bnwd_values>> }
  ?item bnwdt:P33 ?admin .
  optional { ?admin bnwdt:P117 ?wd .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en,en-gb". } 
  }'  




