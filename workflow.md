# Wikibase to visualisation: a workflow


## Infrastructure

The Beyond Notability database has been created using [Wikibase](https://wikiba.se/), a ["software toolbox that offers a collaborative space to pool, edit and curate information in a structured way"](https://www.mediawiki.org/wiki/Wikibase). The biggest and best-known instance of Wikibase is [Wikidata](https://www.wikidata.org/wiki/Wikidata:Main_Page). 

The BN wikibase lives on [Wikibase Cloud](https://www.wikibase.cloud/), a free service hosted by Wikimedia Deutschland which is designed to make "it easy and quick to host your Wikibase with no need for your own infrastructure". (At the time of writing, August 2024, it's in Open Beta. There have been a few teething troubles over the last year, but it has good community support and is being used by a growing number of projects.)


## SPARQL

The wikibase [Query Service](https://beyond-notability.wikibase.cloud/query/) (WQS) is the essential tool for querying the project database. 

This entailed learning [SPARQL](https://en.wikipedia.org/wiki/SPARQL) and something about the specifics of the Wikibase/Wikidata data model. (Many Wikidata resources are equally applicable to other Wikibases; the Wikidata Query Service (WDQS) works in much the same way as the WQS.)

Key resources I used, many of them free online:

General

- [Learning SPARQL](https://www.learningsparql.com/) book by Bob DuCharme (inexpensive)

Wikidata/wikibase-specific

- [SPARQL (Wikibooks)](https://en.wikibooks.org/wiki/SPARQL)
- [Wikidata SPARQL tutorial](https://www.wikidata.org/wiki/Wikidata:SPARQL_tutorial)
- [Wikidata Query Service tutorial](https://wdqs-tutorial.toolforge.org/)

However, I quickly discovered that I wanted to do a lot of queries that were a bit different from most of the examples in basic Wikidata/wikibase SPARQL tutorials. These tend to cover how to do quite specific things like "find items that are films (P179) in the Star Wars series (Q22092344)"

But I was more likely to want to ask questions that would look like "find items that are associated with the Star Wars series, but I don't know exactly what sort of things there might be yet, oh and can I have associated dates of any type for the results as well?".

Writing queries to explore a wikibase where you don't know exactly what you're looking for, or you want to get several different kinds of a thing at the same time, turned out to be quite a bit harder (and involved getting more familiar with an intimidatingly complex [data model](https://www.mediawiki.org/wiki/Wikibase/DataModel/Primer)). [Stack Overflow](https://stackoverflow.com/questions/46383784/wikidata-get-all-properties-with-labels-and-values-of-an-item) was also invaluable.


## From SPARQL to R

The crucial next step is to get the results of queries into R in order to use the [Tidyverse](https://www.tidyverse.org/) and visualise with [ggplot](https://ggplot2.tidyverse.org/).

As far as possible I wanted the queries underpinning all my work to be documented on the [wikibase queries page](https://beyond-notability.wikibase.cloud/wiki/Project:SPARQL/examples) so they could be run by project colleagues (or anyone else, for that matter) to compare results. 

This influenced my workflow choices in some specific ways (which might not work out the same way for all projects).


## R packages 

Almost all the packages used regularly during the project are available from the official R CRAN repository. SPARQLchunks is only on Github.

### for SPARQL queries

Ultimately, it isn't essential to have a dedicated SPARQL package for sending queries to an endpoint; you could just use something like [RCurl](https://cran.r-project.org/package=RCurl) or [httr](https://httr.r-lib.org/index.html). But it can be convenient to have functions set up for this particular purpose.


- [glitter](https://lvaudor.github.io/glitter/index.html)

I played around a bit with this package which

> aims at writing and sending SPARQL queries without advanced knowledge of the SPARQL language syntax. It makes the exploration and use of Linked Open Data (Wikidata in particular) easier for those who do not know SPARQL well.

It's a nice package that I might well use in the future, but it wasn't quite right for this project. 
I couldn't really avoid getting to know SPARQL and I wanted to keep records of my queries in the wikibase. (My colleagues don't know SPARQL well, it's true... but they don't know R either.) So it was simpler to copy and paste queries from the WQS.


- [SPARQL](https://cran.r-project.org/package=SPARQL)

This used to be the go-to package for querying SPARQL from R, but it was removed from the official R repository a few years ago and isn't online anywhere else. That suggests no one is maintaining it at all, so I decided to avoid it. (But it probably still works if you get an archived copy from CRAN.)


- [SPARQLchunks](https://github.com/aourednik/SPARQLchunks) 

I couldn't quite get this package to work to do the thing it's primarily advertised for (creating SPARQL code chunks)! But it worked perfectly for what I did want: fetching a SPARQL query and turning the results into a dataframe ready for further work. 


### other Wikibase/Wikidata packages

- [WikipediR](https://github.com/Ironholds/WikipediR/)

A Wikibase is installed as part of a Mediawiki site, and can also contain wiki pages that aren't part of the wikibase. This package is useful for getting stuff out of those pages. 

- [WikidataQueryServiceR](https://github.com/wikimedia/WikidataQueryServiceR)

This is a handy package to have around to query Wikidata directly. In practice I've rarely used it as it's easier to write a [federated query](https://www.mediawiki.org/wiki/Wikibase/Federation) from the WQS (though that's likely to be slower, if speed becomes an issue).


### other R packages

Large and small packages that have been used frequently in this project:

- [tidyverse](https://www.tidyverse.org/) ("an opinionated collection of R packages designed for data science"; includes dplyr, tidyr, ggplot2, lubridate, stringr, among others)
- [here](https://here.r-lib.org/) ("uses the top-level directory of a project to easily build paths to files")
- [glue](https://glue.tidyverse.org/) ("an implementation of interpreted string literals", aka a smarter verson of paste functions)
- [reactable](https://glin.github.io/reactable/index.html) (interactive tables)

ggplot2 extensions for visualisations:

- [ggthemes](https://jrnold.github.io/ggthemes/index.html) (a bunch of extra themes and functions for ggplot2)
- [ggalt](https://github.com/hrbrmstr/ggalt) (more extras for ggplot2, including dumbbell charts)
- [ggbeeswarm](https://github.com/eclarke/ggbeeswarm) (a project favourite: beeswarm plots)




## Setting up 


A number of prerequisites and (more or less) frequently used functions and objects are stored in two R scripts that are loaded at the beginning of every new document. (I split the setting up into two scripts early on since one is (meant to be) more general and one more specific, but to be honest I probably didn't really need to bother.)

- `R/shared.R` is the first script; it loads the R packages that I'm likely to use most frequently, the odd bit of data that isn't part of the wikibase and some general purpose functions.
- `R/std_queries.R` comes next and has some more specific SPARQL and query-related functions

For example, the BN endpoint URL and [prefixes](https://en.wikibooks.org/wiki/SPARQL/Prefixes) are in `std_queries`.

```
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
```

The `bn_std_query()` function puts together a query string to send to the WQS and get results in a dataframe:

```
bn_std_query <- function(sparql){
  c(paste(
    bn_prefixes,
    sparql  # a SPARQL query string *without prefixes*
  )) |>
    sparql2df(endpoint=bn_endpoint)  # uses SPARQLchunks
}
```

There are other functions for initial processing of results. The wikibase URIs are long URL strings like  https://beyond-notability.wikibase.cloud/entity/Q537. So `make_bn_item_id()` and `make_bn_ids()` extract the actual IDs (Q537). 

```
make_bn_item_id <- function(df, v) {
  df |>
    mutate(bn_id = str_extract({{v}}, "([^/]*$)")) |>
    relocate(bn_id)
}

make_bn_ids <- function(data, across_cols=NULL, ...) {
  data |>
    mutate(across({{across_cols}}, ~str_extract(., "([^/]*$)")))
}
```

Dates are returned as [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) timestamps and there's a function to convert them to a more R-friendly format. (The date column has to be named `date`. This was laziness on my part; it would have been more useful if I'd made it more flexible.)

```
# requires {lubridate}
make_date_year <-function(data){
  data  |>
# any <unknown value> dates will fail to parse; convert to NA first
    mutate(date = if_else(str_detect(date, "^_:t"), NA, date))  |>
    mutate(date = parse_date_time(date, "ymdHMS"))  |>
    # might as well make a year column while we're here
    mutate(year = year(date))
}
```


## Querying

This query gets a list of all the women currently in the BN database, with dates of birth/death if we have them, and the number of [statements](https://www.wikidata.org/wiki/Help:Statements) for each woman, which gives an idea of how much information we've collected for her. A simple but typical example of the two-step sparql>query process.

```
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

bn_women_list_query <-
  bn_std_query(bn_women_list_sparql) |>
  make_bn_ids(person) |>
  # if there's no data it's imported as a blank cell; convert to NAs.
  mutate(across(c(dob, dod), ~na_if(., ""))) |>
  arrange(person)
```


## Workflow in practice

See the [BN Notes](https://beyond-notability.github.io/bn_notes/) blog for data analysis and visualisations using the workflow and its linked Github repository for all the code.


