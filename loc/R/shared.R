## shared libraries, functions etc ####

library(readxl) 
library(janitor)
library(glue)
library(tidytext)
suppressPackageStartupMessages(library(tidyverse) )
suppressPackageStartupMessages(library(SPARQLchunks)) 



# a standard query using bn_prefixes and bn_endpoint. sparql= 'query string *without prefixes*'
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql
  )) |>
    SPARQLchunks::sparql2df(endpoint=bn_endpoint) 
}



#mutate(across(c(a, b), ~str_extract(., "([^/]*$)") )) 
# previous: \\bQ\\d+$
# make an ID out of a wikibase item URL. v is often but not always person. could be eg item, place, woman, etc.
make_bn_item_id <- function(df, v) {
  df |>
    mutate(bn_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_id)
}


## endpoint URLs ####
bn_endpoint <- "https://beyond-notability.wikibase.cloud/query/sparql"

## prefixes, as strings for glueing 
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


