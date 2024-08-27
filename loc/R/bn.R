## shared libraries, functions etc ####
source(here::here("loc/R/shared.R"))


## NB these queries were run 27/2/24-28/2/24 
## the BN database continued to be updated during 2024, so re-running the queries will not give exactly the same results

bn_women_statements_sparql <-
  'SELECT distinct ?person ?personLabel ?statements ?dod ?dob
WHERE {
   ?person bnwdt:P3 bnwd:Q3 ;
         wikibase:statements ?statements .
   FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}
    optional { ?person bnwdt:P15 ?dod .   }
    optional { ?person bnwdt:P26 ?dob .   }
   FILTER (?statements >=8) . 
   SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}'

bn_women_statements_query <-
  bn_std_query(bn_women_statements_sparql) |>
  make_bn_item_id(person) |>
  select(-person)

bn_women_filtered <-
  bn_women_statements_query |> 
  mutate(across(c(dob, dod) , ~parse_date_time(., "ymdHMS"))) |>
  mutate(across(c(dob, dod), year)) |>
  distinct(bn_id, personLabel, statements, dob, dod) |>
  # a few women have more than one year of birth recorded; keep only the earliest
  group_by(bn_id) |>
  arrange(dob, .by_group = T) |>
  top_n(1, row_number()) |>
  ungroup() |>
  # filter out "historical" women
  filter(dob>1750 | is.na(dob)) |>
  filter(dod>1860 | is.na(dod)) |>
  filter(!bn_id %in% c("Q1391")) |>  # Q1391 queen victoria
  rename(name_label=personLabel) |>
  # drop women with Mrs/Miss title only but keep Mrs/Miss with initials
  filter(!str_detect(name_label, "^(Mrs|Miss)") | str_detect(name_label, "[A-Z]\\.")) 

## saved for reference/reuse
## bn_women_filtered |> write_csv(here::here("loc/csv/bn_women_loc_240227.csv"))


bn_women_birth_married_sparql <-
  'SELECT distinct ?person ?personLabel ?birthname ?marriedname 
WHERE {
   ?person bnwdt:P3 bnwd:Q3 ;
         wikibase:statements ?statements .
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .}
  OPTIONAL {?person bnwdt:P140 ?birthname .} #look for birth name
  OPTIONAL {?person bnwdt:P141 ?marriedname .} #look for married name
  FILTER (?statements >=8) .
  SERVICE wikibase:label {bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en-gb,en".}
}
order by ?person'

bn_women_birth_married_query <-
  bn_std_query(bn_women_birth_married_sparql) |>
  make_bn_item_id(person) |>
  mutate(across(c(birthname, marriedname), ~na_if(., ""))) |>
  select(-person)

bn_women_birth_married_names <-
  bn_women_birth_married_query |>
  filter(!is.na(birthname) | !is.na(marriedname)) |>
  # pull first names out of personLabel
  mutate(personLabel = str_trim(personLabel)) |>
  mutate(forename = case_when(
    str_detect(personLabel, paste0(birthname, "$")) ~ str_remove(personLabel, paste0(birthname, "$")),
    str_detect(personLabel, paste0(marriedname, "$")) ~ str_remove(personLabel, paste0(marriedname, "$"))
  )) |>
  # drop a few that didn't work
  filter(!is.na(forename)) |>
  pivot_longer(c(birthname, marriedname), names_to = "surname_type", values_to = "surname") |>
  filter(!is.na(surname)) |>
  mutate(bn_name = paste(surname, forename, sep = ", ")) |>
  relocate(surname_type, personLabel, .after = last_col()) |>
  mutate(src="birth_married_names")




## ISNI names data

bn_women_isni_sparql <- '
SELECT distinct ?person ?isni_ID  
WHERE {  
  ?person bnwdt:P3 bnwd:Q3  ;
         wikibase:statements ?statements . 
  FILTER NOT EXISTS {?person bnwdt:P4 bnwd:Q12 .} 
  FILTER (?statements >=8)
  ?person bnwdt:P125 ?isni_ID .
}'

bn_women_isni_query <-
  bn_std_query(bn_women_isni_sparql) |>
  make_bn_item_id(person) |>
  # remove any spaces in the IDs
  mutate(isni_ID = str_remove_all(isni_ID, " +")) 



isni_names_csv <-
  read_csv(here::here("loc/csv/isni_personalName-2023-10-09.csv"), show_col_types = FALSE)


isni_names <-
  isni_names_csv |> 
  filter(!is.na(forename)) |>
  # fix a few ALLCAPS surnames  
  mutate(surname = case_when(
    str_detect(surname, "[A-Z][A-Z]+") ~ str_to_title(surname),
    .default = surname
  )) |>
  mutate(forename = str_trim(str_replace_all(forename, "\\. *", "\\. "))) |>
  mutate(forename = case_when(
    str_detect(forename, "\\. *[A-Z]$") ~ paste0(forename, "."),
    .default = forename
  )) |>
  filter(str_detect(surname, "[A-Z]")) |>
  distinct(isni, surname, forename)


bn_women_isni <-
  bn_women_filtered |>
  inner_join(bn_women_isni_query |> select(bn_id, isni_ID), by="bn_id") |> # 494 
  inner_join(isni_names |> select(isni, surname, forename ), by=c("isni_ID"="isni")) |>
  mutate(bn_name = paste(surname, forename, sep = ", ")) |>
  mutate(src="isni")




## a few supplementary names

bn_women_add_names <-
  tribble(~bn_id, ~bn_name, ~src,
          "Q2535", "Hamilton-Gordon, Ishbel", "supplementary",
          "Q2535", "Aberdeen, Ishbel",  "supplementary",
          "Q1497", "Blackwood, Beatrice Mary",  "supplementary",
          "Q3270", "Gordon-Cumming, Constance Frederica",  "supplementary",
          "Q3270", "Gordon-Cumming, C.F.",  "supplementary",
          "Q2554", "de Crespigny, Nancy",  "supplementary",
          "Q2554", "Movius, Nancy",  "supplementary",
          "Q3658", "Blaauw, Margaret Emily",  "supplementary",
          "Q3658", "Gillman, Margaret Emily",  "supplementary",
          "Q3650", "Johnston, Amy",  "supplementary",
          "Q3650", "Johnston, A. Wintle",  "supplementary"
  )


## put together the different sources

bn_women_names_combined <-
  bind_rows(
    bn_women_birth_married_names |>
      select(bn_id, bn_name, src) ,
    bn_women_isni |>
      select(bn_id, bn_name, src) ,
    bn_women_add_names
  ) |>
  # only women who are in the filtered table.
  semi_join(bn_women_filtered, by="bn_id") |>
  mutate(bn_name=str_trim(bn_name)) |>
  arrange(bn_id, bn_name)




## make an api query string for each person
bn_women_loc_summary <-
  bn_women_names_combined |>
  mutate(bn_name=str_trim(bn_name)) |>
  distinct(bn_id, bn_name)  |>
  # need to limit to five names max per query: drop longest strings
  add_count(bn_id) |>
  group_by(bn_id) |>
  arrange(str_length(bn_name), .by_group = T) |>
  mutate(rn = row_number()) |>
  filter(row_number() <= 5) |>
  ungroup() |>
  mutate(au = glue('dc.creator="{bn_name}"')) |>
  group_by(bn_id) |>
  summarise(names = paste(au, collapse = " or ") ) |>
  ungroup()  |>
  # url encoding. requires {RCurl} package https://cran.r-project.org/web/packages/RCurl/index.html
  mutate(escape_names = RCurl::curlEscape(names)) |>
  mutate(url = glue("http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&maximumRecords=100&query={escape_names}")) |>
  arrange(bn_id)

 
## saved copy for reference/reuse
## bn_women_loc_summary |> write_csv(here::here("loc/csv/bn_women_loc_summary_240228.csv"))




