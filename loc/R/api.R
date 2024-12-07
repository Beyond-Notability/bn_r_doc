## NB this code is provided for reference, but it has been commented out because:

## running it would take some time and the XML that was generated by the code is included in loc/xml/
## moreover, the code should not be expected to produce exactly the same results, as the catalogue is not static data
## finally, no guarantees can be given that the API will continue to work in the same way (... or at all)

## data was fetched on 28/2/2024

## shared libraries, functions etc ####
# source(here::here("loc/R/shared.R"))

# library(httr)
# library(xml2)


## use slowly() to be nice to the server
# slow_get <- 
#   # 1 is default rate_delay
#   slowly(GET, rate = rate_delay(4))
# 
## the query strings [see bn.R for the code used to create this CSV file]
# bn_women_loc_summary <-
#   read_csv(here::here("loc/csv/bn_women_loc_summary_240228.csv"), show_col_types = FALSE)


## in addition to using slowly, this was split up to fetch 100 at a time as the server seemed a bit flaky sometimes.
## it takes a while to fetch each 100!

# bn_women_loc_001 <-
#   bn_women_loc_summary |>
#   slice(1:100)
# 
# get_bn_women_loc_001 <-
#   map(bn_women_loc_001$url, slow_get)

# bn_women_loc_101 <-
#   bn_women_loc_summary |>
#   slice(101:200)
# 
# get_bn_women_loc_101 <-
#   map(bn_women_loc_101$url, slow_get)
# 
# 
# bn_women_loc_201 <-
#   bn_women_loc_summary |>
#   slice(201:300)
# 
# get_bn_women_loc_201 <-
#   map(bn_women_loc_201$url, slow_get)
# 
# 
# 
# bn_women_loc_301 <-
#   bn_women_loc_summary |>
#   slice(301:400)
# 
# get_bn_women_loc_301 <-
#   map(bn_women_loc_301$url, slow_get)
# 
# 
# 
# bn_women_loc_401 <-
#   bn_women_loc_summary |>
#   slice(401:500)
# 
# get_bn_women_loc_401 <-
#   map(bn_women_loc_401$url, slow_get)
# 
# 
# 
# bn_women_loc_501 <-
#   bn_women_loc_summary |>
#   slice(501:570)
# 
# get_bn_women_loc_501 <-
#   map(bn_women_loc_501$url, slow_get)
# 


## hackyness because cba to do it properly
## tweaked a few queries that failed on first run 

# bn_women_loc_corrected <-
#   bn_women_loc_summary |>
#   inner_join(
#     tribble(~bn_id, ~corrected_url,
#             "Q482",    "http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&maximumRecords=100&query=dc.creator%3D%22Cecil%2C%20Mary%20Rothes%20Margaret%22%20or%20dc.creator%3D%22Amherst%2C%20Mary%20Rothes%20Margaret%22%20or%20dc.creator%3D%22Tyssen-Amherst%2C%20Mary%20Rothes%20Margaret%22%20or%20dc.creator%3D%22Amherst%2C%20Mary%20Rothes%20Margaret%20Tyssen-Amherst%20Cecil%22" ,
#             "Q624",    "http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&maximumRecords=100&query=dc.creator%3D%22Evans%2C%20Maria%20Millington%22%20or%20dc.creator%3D%22Lathbury%2C%20Maria%20Millington%22%20or%20dc.creator%3D%22Evans%2C%20Maria%20Millington%20Lathbury%22%20or%20dc.creator%3D%22Lathbury%20Evans%2C%20Maria%20Millington%22",
#             "Q898",    "http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&maximumRecords=100&query=dc.creator%3D%22Gray%2C%20Elizabeth%20Caroline%22%20or%20dc.creator%3D%22Johnstone%2C%20Elizabeth%20Caroline%22%20or%20dc.creator%3D%22Gray%2C%20Elizabeth%20Caroline%20Hamilton%22%20or%20dc.creator%3D%22Hamilton%20Gray%2C%20Elizabeth%20Caroline%22"
#     ) , by="bn_id"
#   )
# 
# # get_bn_women_loc_corrected <-
# #  map(bn_women_loc_corrected$corrected_url, slow_get)


## API limit of 100 results per query; only 3 women have more than 100, so just fetch the extras separately

# bn_women_loc_extras_101 <-
#   bn_women_loc_summary |>
#   filter(bn_id %in% c("Q1061", "Q1084", "Q275")) |>
#   mutate(url_101 = paste0(url, "&startRecord=101"))
# 
# get_bn_women_loc_extras_101 <-
#   map(bn_women_loc_extras_101$url_101, slow_get)


## read in the fetched XML
## this was separated out from the fetch process in case of problems with the API server; easier to check results if it was done in two stages

# xml_bn_women_loc_001 <-
#   map(get_bn_women_loc_001, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_001 |>
#               select(bn_id, names)) 

# xml_bn_women_loc_101 <-
#   map(get_bn_women_loc_101, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_101 |>
#               select(bn_id, names)) 
# 
# 
# xml_bn_women_loc_201 <-
#   map(get_bn_women_loc_201, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_201 |>
#               select(bn_id, names)) 
# 
# 
# xml_bn_women_loc_301 <-
#   map(get_bn_women_loc_301, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_301 |>
#               select(bn_id, names)) 
# 
# 
# xml_bn_women_loc_401 <-
#   map(get_bn_women_loc_401, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_401 |>
#               select(bn_id, names)) 
# 
# 
# xml_bn_women_loc_501 <-
#   map(get_bn_women_loc_501, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_501 |>
#               select(bn_id, names)) 
# 
# xml_bn_women_loc_corrected <-
#   map(get_bn_women_loc_corrected, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_corrected |>
#               select(bn_id, names, corrected_url)) 
# 
# xml_bn_women_combined <-
# bind_rows(
#   xml_bn_women_loc_001,
#   xml_bn_women_loc_101,
#   xml_bn_women_loc_201,
#   xml_bn_women_loc_301,
#   xml_bn_women_loc_401,
#   xml_bn_women_loc_501,
#   xml_bn_women_loc_corrected
# )

# xml_bn_women_loc_extras_101 <-
#   map(get_bn_women_loc_extras_101, read_xml) |>
#   enframe() |>
#   bind_cols(bn_women_loc_extras_101 |>
#               select(bn_id, names, url_101)) 


## write to XML files, one per woman
## adapted from code for writing multiple files at
## https://martinctc.github.io/blog/vignette-write-and-read-multiple-excel-files-with-purrr/

# output_xml <- function(data, names, folder) {
#   folder_path <- here::here("loc/xml")
#   write_xml(data, paste0(folder_path, "/", names, ".xml"))
# }
# 
# bn_ids <-
#   xml_bn_women_combined |>
#   pull(bn_id)
# 
# bn_xml <-
#   xml_bn_women_combined |>
#   pull(value)
# 
## suppress annoying printing of output in console with invisible()
# invisible(
#   list(
#     data=bn_xml,
#     names=bn_ids
#   ) |>
#     pmap(output_xml)
# )


## extras need distinct file names...

# output_xml_extras <- function(data, names, folder) {
#   folder_path <- here::here("loc/xml")
#   write_xml(data, paste0(folder_path, "/", names, "__101.xml"))
# }
# 
# bn_ids <-
#   xml_bn_women_loc_extras_101 |>
#   pull(bn_id)
# 
# bn_xml <-
#   xml_bn_women_loc_extras_101 |>
#   pull(value)
# 
# 
# invisible(
#   list(
#   data=bn_xml,
#   names=bn_ids
# ) |>
#   pmap(output_xml_extras)
# )

