
## shared libraries, functions etc ####

## BEFORE std_queries.R! ##


## frequently used libraries

library(knitr)
library(kableExtra)
library(reactable)

library(readxl) 
library(writexl)

library(janitor)
library(scales)
library(glue)

library(tidytext)

library(tidyverse)

## viz/ggplot extras

library(patchwork)
library(ggthemes)
library(ggalt)
library(ggbeeswarm)


# gg theme slightly modified default... because.
theme_set(theme_gray())
theme_update(panel.background = element_rect(fill="#fafafa"), 
             strip.background = element_rect(fill="#f0f0f0"),
             axis.ticks = element_line(colour = 'grey'))




## some general purpose functions ####


## DATES

# convert dates fetched in sparql queries; column has to be named date and be in standard wikibase format. (won't work on edtf dates)
make_date_year <-function(data){
  data  |>
    # sometimes there are <unknown value> dates in results which will fail to parse; convert to NA first
    mutate(date = if_else(str_detect(date, "^_:t"), NA, date))  |>
    mutate(date = parse_date_time(date, "ymdHMS"))  |>
    # might as well make a year column while you're at it
    mutate(year = year(date))
}

# make decade (0-9) for any year
make_decade <- function(data, year) {
  data |>
    mutate(decade = {{year}} - ({{year}} %% 10)) |>
    relocate(decade, .after = {{year}})
}


# add date property labels (inside a mutate). primarily for qualifier dates where it's quicker to get results without the labels
date_property_labels <- function(v) {
  case_when(
    {{v}}=="P1" ~ "point in time",
    {{v}}=="P27" ~ "start time",
    {{v}}=="P28" ~ "end time",
    {{v}}=="P53" ~ "earliest date",
    {{v}}=="P51" ~ "latest date"
  )
}
# but often it's easier to (left) join to bn_properties, renaming as appropriate.
# eg:
#  left_join(bn_properties |> select(date_qual_prop= bn_prop_id, date_qual_label= propertyLabel), by="date_qual_prop") |>



# turn ymdHMS or ymd dates with precision levels into EDTF format. works with posixct dates as well as strings.
# (use inside a mutate)
make_edtf_date_from_ymdhms <- 
  function(p=date_precision, d=date){
    case_when(
      {{p}}==9 ~ str_sub({{d}}, 1, 4), # year only
      {{p}}==10 ~ str_sub({{d}}, 1,7), # year-month
      {{p}}==11 ~ str_sub({{d}}, 1, 10) # ymd
    )
  }

# take a posixct date + wikibase numerical date precision and turn into display date string
# would be trivial to add parse date step for a ymdHMS date straight from wikibase but usually done that already
make_display_date <- 
  function(data, date=date, date_precision=date_precision){
    # requires lubridate
    data |>
      mutate(m = month({{date}}, label=TRUE, abbr=F), d=day({{date}}), y=year({{date}})) |>
      mutate(display_date = case_when(
        is.na({{date}}) ~ NA,
        {{date_precision}}==11 ~ paste(d, m, y),
        {{date_precision}}==10 ~ paste(m, y),
        {{date_precision}}==9 ~ as.character(y)
      )) 
  }




# take a DF (or other object) name to use as a value. (eg sometimes use this in bind_rows to make an src column)
# NB object has to already exist! and may not work as expected if you use sthg like as_tibble before it

obj_name_as_var <- function(d){
  d_name <- deparse(substitute(d))
  d |>
    mutate(src = d_name)
}



## extra data ####


## a few bits of data that aren't part of the wikibase 

## PPA buckets (v1 Feb 2024)

bn_ppa_buckets <-
  read_csv(here::here("data/bn_ppa_buckets_v1_240212.csv"), show_col_types = F)



## to query wikidata directly (not used so far)
# library(WikidataQueryServiceR) 



# stuff in non wikibase pages in the Mediawiki site
library(WikipediR)


## Sorted Properties page
## https://beyond-notability.wikibase.cloud/wiki/MediaWiki:Wikibase-SortedProperties

# not a complete list of properties (use bn_properties in std_queries for that) but has useful section categories
# caveat: some properties might be in more than one section 


# fetch the page
bn_sorted_properties_fetch <-
  page_content(
    domain = "beyond-notability.wikibase.cloud",
    page_name = "MediaWiki:Wikibase-SortedProperties",
    as_wikitext = TRUE
  )

# parse the page content into sections, ids, labels
bn_sorted_properties <-
  bn_sorted_properties_fetch$parse$wikitext$`*` |>
  enframe() |>
  select(-name) |>
  # drop initial ==
  mutate(value=str_remove(value, "^ *==+ *")) |>
  # use start of line == to make new rows
  unnest_regex(section, value, pattern = "\n+ *==", to_lower = F) |>
  # use end of line == to separate heading sfrom text
  separate(section, into = c("section", "text"), sep=" *== *\n+ *") |>
  # include \n here in case there's ever a literal * in the text
  unnest_regex(text, text, pattern = " *\n+\\* *", to_lower = F) |>
  # get rid of the remaining * at start of text
  mutate(text = str_remove(text, "^ *\\* *")) |>
  # extract P id from text
  mutate(bn_prop_id = word(text)) |>
  # extract label in (...) nb that some of the labels contain ()
  mutate(label = str_match(text, "\\((.+)\\) *$")[,2]) |>
  mutate(across(c(section, label), str_trim)) |>
  relocate(bn_prop_id, label)




## labels and display stuff ####


# for abbreviating names of the three main societies in labels (use in str_replace_all)
sal_rai_cas_abbr <-
  c("Society of Antiquaries of London"="SAL", "Royal Archaeological Institute"="RAI", "Congress of Archaeological Societies"="CAS")

##(make sure you do this after sal_rai_cas_abbr if you're using that)
organisations_abbr <-
  c("Archaeological" = "Arch", "Antiquarian" = "Antiq", "Society" = "Soc", "Association" = "Assoc")


# see dates_timelines_231114 rmd
# function to make a named vector of colours for variable values
# the df needs cols: group, colour_category
# get rows in required order before using this (this sorts by frequency; could make another version for random/uncounted etc) and then add rowid
# grpname is the name of the group or bucket in the group col
# **grpcols has to be a vector of colour codes** - sthg like viridis_pal()(16)  will work, just have to have enough colours. may need to reverse direction to stop the first one being white grr.

make_named_colours <- function(df, grpname, grpcols){
  df |>
    filter(group==grpname) |>
    count(colour_category, sort = TRUE) |>
    rowid_to_column() |>
    left_join(grpcols |> enframe(), by=c("rowid"="name")) |>
    fill(value) |>
    select(colour_category, value) |>
    # turn the df into a named vector with deframe which can be used in scale_color_manual
    deframe()
}




