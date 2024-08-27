## shared libraries, functions etc ####
source(here::here("loc/R/shared.R"))

library(openxlsx)

## additional functions 

# make standardised names (full _std and short _std_s versions)
bn_std_names <- function(df, col){
  df |>
    # lowercase
    mutate(name_std = str_to_lower({{col}})) |>
    # handle accented letters for consistency
    mutate(name_std = stringi::stri_trans_general(name_std, id = "Latin-ASCII"))  |>
    # short version: non-d surname + first word/initials only
    mutate(name_std_s = str_extract(name_std, "^[^,]+, [^ ]+")) |>
    # remove all punct and spaces 
    mutate(across(c(name_std, name_std_s), ~str_remove_all(., "[:punct:]| ")))
}



## DATA ####

## get the list of women in the API query [see bn.R]
bn_women_list <-
  read_csv(here::here("loc/csv/bn_women_loc_240227.csv"), show_col_types = FALSE)



## read in the CSVs of data extracted from LOC XML

loc_fields_with_subfields_csv <-
  read_csv(here::here("loc/csv/loc_fields_subfields-2024-03-01.csv"), show_col_types = FALSE) |>
  mutate(record_id = paste( bn_id, str_pad(recordPosition, 3, side="left", pad="0"), sep = "_" ))  |>
  rename_with(~paste0("loc_", .x), .cols=c(leader, field, sub_field, code, cf008, cf001) ) |>
  rename(bn_query=query, file_id=id, loc_records=number) |>
  select(-record_count) |>
  mutate(field_pad = str_pad(field_count, 3, pad="0"), sub_pad = str_pad(sub_count, 3, pad="0")) |>
  # unwanted [from old catalog] is in various places, so remove it at the beginning 
  mutate(loc_sub_field = str_remove(loc_sub_field, "\\[from *old *catalog\\]|\\[old catalog heading\\]")) |>
  relocate(record_id)

 loc_fields_csv <-
  read_csv(here::here("loc/csv/loc_fields-2024-03-01.csv"), show_col_types = FALSE) |>
  mutate(record_id = paste( bn_id, str_pad(recordPosition, 3, side="left", pad="0"), sep = "_" ))  |>
  rename_with(~paste0("loc_", .x), .cols=c(leader, field, cf008, cf001) ) |>
  rename(bn_query=query, file_id=id, loc_records=number) |>
  select(-record_count) |>
  mutate(field_pad = str_pad(field_count, 3, pad="0") ) |>
  relocate(record_id)


## just the women whose names matched records in the loc xml. 
## if they have only one of dob or dod, infer an approximate value for the other using std +/- 80
 
bn_women_loc <-
bn_women_list |>
      mutate(bn_yob = case_when(
        !is.na(dob) ~ dob,
        is.na(dob) ~ dod - 80
      )) |>
      mutate(bn_yod = case_when(
        !is.na(dod) ~ dod,
        is.na(dod) ~ dob+80
      ) ) |>
  select(bn_id, bn_yob, bn_yod, bn_name= name_label) |>
  inner_join(
    loc_fields_csv |>
      distinct(bn_id, loc_records, bn_query), by="bn_id"
  )


## PROCESSING ####


## LOC: works 

# edition. [MARC 250]
loc_data_edition <-
  loc_fields_with_subfields_csv |>
  filter(loc_field=="250" & loc_code=="a") |>
  select(record_id, loc_code, loc_sub_field, field_pad, sub_pad) |>
  pivot_wider(values_from = loc_sub_field, names_from = loc_code) |>
  rename(loc_edition=a)


# topical terms MARC field 650. inconsistent but maybe handy for eyeballing.
loc_fields_topical_collapsed <-
  loc_fields_with_subfields_csv |> 
  filter( loc_field %in% c("650") & loc_code %in% c("a", "x", "v")  ) |>
  # a bit of cleaning
  mutate(loc_sub_field = str_replace_all(loc_sub_field, "  +", " ")) |>
  # collapse into one row
  group_by(record_id) |>
  arrange(loc_sub_field, .by_group = T) |>
  summarise(loc_sub_fields_avx =paste(unique(loc_sub_field), collapse = " | "), .groups = "drop_last") |>
  ungroup() 


# bibliographic level and type of record. 
loc_leader <-
  loc_fields_csv |>
  mutate(loc_bib_level = str_sub(loc_leader, 8,8)) |>
  mutate(loc_type_record = str_sub(loc_leader, 7,7)) |>
  distinct(record_id, loc_bib_level, loc_type_record) 


# loc pub dates. [MARC 260 and 264]
# 260 first then 264 fallback (in case any have both)

loc_data_pub_date_260 <-
  loc_fields_with_subfields_csv |> 
  select(loc_sub_field, loc_field, loc_code, recordPosition, field_pad, sub_pad, bn_id, loc_records, record_id) |>
  # c=date. b can occasionally contain pub date instead as part of text
  filter( loc_field %in% c("260")  & loc_code %in% c("b", "c") ) |>  
  # paste/collapse subfields to fix any multi code. 260 needs this
  group_by(record_id, bn_id, loc_field, recordPosition, field_pad, loc_code) |>
  summarise(loc_sub_field=paste(loc_sub_field, collapse = "|"), .groups = "drop_last") |>
  ungroup() |>
  pivot_wider(id_cols = c(record_id, bn_id, loc_field, recordPosition, field_pad), values_from = loc_sub_field, names_from = loc_code) |>
  # some cleaning up
  mutate(loc_date_pub = case_when(
    !is.na(c) ~ c,
    str_detect(b, "\\b(1[89]\\d\\d)") ~ str_extract(b, "\\b1[89]\\d\\d.*$")
  )) |>
  mutate(loc_date_pub = str_remove(loc_date_pub, "\\. *$")) |>
  # make numerical version
  mutate(loc_date_pub_n = parse_number(str_extract(loc_date_pub, "\\d\\d\\d\\d"))) |>
  unite("loc_pub_field", c(loc_field, field_pad)) |>
  select(bn_id, loc_pub_field, recordPosition, loc_date_pub, loc_date_pub_n, record_id)

loc_data_pub_date_264 <-
  loc_fields_with_subfields_csv |>
  filter(loc_field %in% c("264") & loc_code %in% c("c")) |> 
  unite("loc_pub_field", c(loc_field, field_pad)) |>
  rename(loc_date_pub=loc_sub_field) |>
  mutate(loc_date_pub_n = parse_number(str_extract(loc_date_pub, "\\d\\d\\d\\d"))) |>
  distinct(bn_id, record_id, loc_date_pub_n, loc_date_pub, loc_pub_field, recordPosition) |>
  # a few with multi dates; get the earliest only
  group_by(record_id) |>
  arrange(loc_date_pub_n, .by_group = TRUE) |>
  top_n(-1, row_number()) |>
  ungroup() |>
  # in case any have 260
  anti_join(loc_data_pub_date_260, by="record_id")


## loc title [MARC 245] + date

loc_data_title_date <-
  loc_fields_with_subfields_csv |>
  distinct(record_id, bn_id, recordPosition) |>
  left_join(
    loc_fields_with_subfields_csv |> 
      select(loc_sub_field, loc_field, loc_code, recordPosition, field_pad, sub_pad, bn_id, loc_records, record_id) |>
      filter( loc_field %in% c("245") & loc_code=="a" ) |>
      pivot_wider(id_cols = c(record_id, bn_id, loc_field, recordPosition, field_pad), values_from = loc_sub_field, names_from = loc_code) |>
      unite("loc_title_field", c(loc_field, field_pad)) |>
      rename(loc_title=a)  |>
      # clean/standardise loc_title
      # first light clean of trailing punctuation/space. mainly for reading in the final reduced version
      mutate(loc_title_cln = str_trim(str_remove(loc_title, "[:punct:]+ *$")))  |>
      # now really strip it for collapsing.
      mutate(loc_title_cln2 = str_remove_all(loc_title_cln, "'")) |>
      mutate(loc_title_cln2 = str_replace_all(loc_title_cln2, "[:punct:]", " ")) |>
      mutate(loc_title_cln2 = str_trim(str_replace_all(loc_title_cln2, "  +", " "))) |>
      mutate(loc_title_cln2 = str_to_lower(loc_title_cln2)) 
    , by=c("record_id", "bn_id", "recordPosition")
  ) |>
  # loc pub date
  left_join(
    bind_rows(
      loc_data_pub_date_260,
      loc_data_pub_date_264
    ) , by=c("bn_id", "recordPosition", "record_id")
  ) |>
  arrange(bn_id, recordPosition) |>
  relocate(recordPosition, loc_title_field, loc_pub_field, .after = last_col())





## LOC: names

# distinct record_id + bn_query 
# normalisation: lower case, remove spaces, remove punctuation, replace accented characters
# full names, shortened names (surname, first word)

# unpack bn_query
bn_query_strings <-
  loc_fields_csv  |>
  mutate(bn_query = str_replace_all(bn_query, "  +", " ")) |>
  mutate(bn_query = str_remove_all(bn_query, "dc.creator=")) |>
  unnest_regex(bn_string, bn_query, pattern = " +or +", to_lower = F) |>
  mutate(bn_string = str_remove_all(bn_string, '^"|"$')) |>
  distinct(bn_string, record_id) 

loc_data_name_strings <-
  bn_query_strings |>
  bn_std_names(bn_string) |>
  arrange(record_id, name_std)

loc_data_names <-
  loc_fields_with_subfields_csv |> 
  select(loc_sub_field, loc_field, loc_code, recordPosition, field_pad, sub_pad, bn_id, bn_query, loc_records, record_id) |>
  filter(loc_field %in% c("100")  ) |>  
  group_by(bn_id, loc_field, recordPosition, field_pad, loc_code, bn_query, record_id) |>
  summarise(loc_sub_field=paste(loc_sub_field, collapse = "|"), .groups = "drop_last") |>
  ungroup() |>
  pivot_wider(id_cols = c(bn_id, loc_field, recordPosition, field_pad, bn_query,  record_id), values_from = loc_sub_field, names_from = loc_code) |>
  unite("loc_name_field", c(loc_field, field_pad)) |>
  rename(loc_name=a, loc_name_dates=d, loc_name_terms=e, loc_name_add=q) |>
  select(-`4`, -c, -`0`) |>
  bind_rows(
    loc_fields_with_subfields_csv |> 
      select(loc_sub_field, loc_field, loc_code, recordPosition, field_pad, sub_pad, bn_id, loc_records, bn_query, record_id) |>
      filter(loc_field %in% c("700") & loc_code %in% c("a", "e", "d", "q") ) |>  
      group_by(bn_id, loc_field, recordPosition, field_pad, loc_code, bn_query, record_id) |>
      summarise(loc_sub_field=paste(loc_sub_field, collapse = "|"), .groups = "drop_last") |>
      ungroup() |>
      pivot_wider(id_cols = c(bn_id, loc_field, recordPosition, field_pad, bn_query, record_id), values_from = loc_sub_field, names_from = loc_code) |>
      unite("loc_name_field", c(loc_field, field_pad)) |>
      select(record_id, bn_id, loc_name_field, recordPosition, loc_name=a, loc_name_terms=e, loc_name_dates=d, loc_name_add=q, bn_query) 
  ) |>
  # is it 100 or 700 [for exploring whether there seemed to be any systematic difference...]
  mutate(loc_name_field_no=str_extract(loc_name_field, "^[^_]+")) |>
  # get rid of trailing commas
  mutate(loc_name_cln = str_remove(loc_name, ",+ *$"))  |>
  # [brackets]
  mutate(loc_name_cln = str_remove_all(loc_name_cln, "[\\[\\]()]")) |>
  # a name (but not initial) ends with full stop
  mutate(loc_name_cln = str_remove(loc_name_cln, "(?<=[a-z])\\. *$")) |>
  # rogue extra spaces (hopefully none of these in the query strings...)
  mutate(loc_name_cln = str_replace_all(loc_name_cln, "  +", " ")) |>
  mutate(loc_name_cln = str_trim(loc_name_cln)) |>
  # make std name cols for joins
  bn_std_names(loc_name_cln) |>
  # does name_std match a name in bn_query
  left_join(
    loc_data_name_strings |>
      # removing diacritics in strings causes manytomany so you need distinct here
      distinct(record_id, name_std) |>
      mutate(bn_q_string="y"), by=c("record_id", "name_std")
  )  |>
  # does shortened name_std_s match a name in bn_query - secondary match
  left_join(loc_data_name_strings |> 
              distinct(record_id, name_std_s) |> 
              mutate(bn_q_string_s="y"), by=c("record_id", "name_std_s")) |>
  rename(loc_name_cln_std=name_std, loc_name_cln_std_s=name_std_s) |>
  # add a count of names per record
  add_count(record_id, name="n_names_record")  |>
  # add name position number (bn name not necessarily first)
  group_by(record_id) |>
  mutate(loc_name_pos = row_number()) |>
  ungroup() |>
  # add prefix pos and suffix 100/700 to name_cln for a second loc names all
  mutate(loc_name_cln_field = paste(loc_name_pos, loc_name_cln, loc_name_field_no)) |>
  arrange(bn_id, recordPosition, loc_name_field)



## reduce titles to single row per work
# the earliest date for a work is used as the definitive record. 
# undated are dropped
# use std titles

loc_data_title_date_reduced <-
    # dated titles
    loc_data_title_date |>
      filter(!is.na(loc_date_pub_n)) |>
      # group by bn_id as well as title; nb any titles with multiple BN authors (very few) will be kept separate per name
      group_by(bn_id, loc_title_cln2) |>
      # sort by date to ensure earliest is 1st for record_id1
      arrange(loc_date_pub_n, .by_group = T) |>
      summarise(titles_n=n(), 
                earliest=min(loc_date_pub_n), 
                latest=max(loc_date_pub_n), 
                loc_pub_dates = paste(loc_date_pub_n, collapse = " | "), 
                record_ids = paste(record_id, collapse = " | "), # list all record_ids for reference
                record_id1= first(record_id), # extract first record_id for join
                .groups = "drop_last") |>
      ungroup() |>
  # add edition
  left_join(loc_data_edition |> select(record_id, loc_edition), by=c("record_id1"="record_id")) |>
  # add title/date
  inner_join(loc_data_title_date |> 
               select(record_id, loc_title_cln, loc_date_pub, loc_date_pub_n, recordPosition, loc_title_field, loc_pub_field), 
             by=c("record_id1"="record_id")) |>
  # add subjects
  left_join(
    loc_fields_topical_collapsed, by=c("record_id1"="record_id")
  ) |>
  # add bib level/record type
  left_join(loc_leader, by=c("record_id1"="record_id"))  |>
  select(-loc_title_cln2)



### match BN/LOC names and reduce multiple names to one row per title

# matched bn_name [plus 2ndary match] in its own column

bn_women_loc_titles_names_reduced <-
  loc_data_title_date_reduced |>
  # bn women info
  inner_join(bn_women_loc, by="bn_id")  |> 
  # is loc pub date within project scope; is it during woman's lifetime?
  mutate(pub_bn_dates = case_when(
    loc_date_pub_n >1955  ~ "after_1955",
    loc_date_pub_n <=1830 ~ "before_1831",
    is.na(bn_yob) | is.na(bn_yod) | is.na(loc_date_pub_n) ~ NA,
    loc_date_pub_n < bn_yob ~ "before_yob",
    between(loc_date_pub_n, bn_yob, bn_yob+10) ~ "in_yob_10",
    between(loc_date_pub_n, bn_yob, bn_yod) ~ "in",
    between(loc_date_pub_n, bn_yod, bn_yod+10) ~ "yod_10",
    loc_date_pub_n > bn_yod ~ "after_yod+10",
    .default = "?" # this shouldn't happen... 
  )) |>
  rename(record_id=record_id1) |>
  left_join(
    # join on std name.
    loc_data_names |>
      filter(bn_q_string=="y") |>
      # get first one only; occasionally more than one joins
      group_by(record_id) |>
      top_n(1, row_number()) |>
      ungroup() |>
      select(record_id, loc_bn_name_cln_1=loc_name_cln, loc_bn_name_dates_1 = loc_name_dates, loc_bn_name_terms_1 = loc_name_terms, 
             loc_bn_name_field_no_1= loc_name_field_no, loc_bn_name_pos_1 =loc_name_pos, loc_name_cln_std)
    , by=c("record_id")
  ) |>
  # join on shortened std name.
  left_join(
    loc_data_names |>
      filter(bn_q_string_s=="y") |>
      group_by(record_id) |>
      top_n(1, row_number()) |>
      ungroup() |>
      select(record_id, loc_bn_name_cln_s=loc_name_cln, loc_bn_name_dates_s =loc_name_dates, loc_bn_name_terms_s =loc_name_terms, 
             loc_bn_name_field_no_s =loc_name_field_no, loc_bn_name_pos_s =loc_name_pos, loc_name_cln_std_s), by=c("record_id")
  ) |>
  
  # get *all* the names for the record on one row
  left_join(
    loc_data_names |>
      group_by(record_id, n_names_record) |>
      summarise(loc_all_names = paste(loc_name_cln, collapse = " | "), 
                loc_all_names_field = paste(loc_name_cln_field, collapse=" | "), .groups="drop_last") |>
      ungroup() , by="record_id"
  ) |>
  # is there a matched name? and is it std or std_s
  mutate(loc_bn_name_match = case_when(
    !is.na(loc_bn_name_cln_1) ~ "match",
    !is.na(loc_bn_name_cln_s) ~ "s match",
    .default = "no match"
  )) |>
  # context of any matched name in all_names
  mutate(bn_in_all_names = case_when(
    loc_bn_name_match=="no match" ~ "no match",
    loc_bn_name_cln_1==loc_all_names ~ "only name",
    str_detect(loc_all_names, paste0("^", loc_bn_name_cln_1)) ~ "first name",
    loc_bn_name_cln_s==loc_all_names ~ "only name s",
    str_detect(loc_all_names, paste0("^", loc_bn_name_cln_s)) ~ "first name s",
    .default = "not first" # might need separating into _1 and _s as well? 
  )) |>
  # consolidate std (_1) and std_s. loc_bn_name_cln, loc_bn_name_dates, loc_bn_name_terms, loc_bn_name_field_no, loc_bn_name_pos.
  mutate(loc_bn_name_cln = case_when(
    !is.na(loc_bn_name_cln_1) ~ loc_bn_name_cln_1,
    .default = loc_bn_name_cln_s
  )) |>
  mutate(loc_bn_name_dates = case_when(
    !is.na(loc_bn_name_cln_1) ~ loc_bn_name_dates_1,
    .default = loc_bn_name_dates_s
  )) |>
  mutate(loc_bn_name_terms = case_when(
    !is.na(loc_bn_name_cln_1) ~ loc_bn_name_terms_1,
    .default = loc_bn_name_terms_s
  )) |>
  mutate(loc_bn_name_field_no = case_when(
    !is.na(loc_bn_name_cln_1) ~ loc_bn_name_field_no_1,
    .default = loc_bn_name_field_no_s
  )) |>
  mutate(loc_bn_name_pos = case_when(
    !is.na(loc_bn_name_cln_1) ~ loc_bn_name_pos_1,
    .default = loc_bn_name_pos_s
  ))  |>
  # do stuff with consolidated loc_bn_name cols
  # additional info about the matched name. 
  mutate(loc_bn_name_seg_n = str_count(loc_bn_name_cln, " +")+1) |> # (add 1 to count of separators!)
  mutate(sur = str_extract(loc_bn_name_cln, "^[^,]+")) |>
  mutate(first = str_remove(loc_bn_name_cln, paste0(sur, ", "))) |>
  # if first contains lower case letters, assume it's a name, otherwise initials
  mutate(loc_bn_name_initials = if_else(str_detect(first, "[a-z]"), "name", "initials"))   |>
  # do loc birth/death dates match bn ?
  mutate(loc_bn_name_dates = str_remove(loc_bn_name_dates, "[.,]+ *$")) |>
  mutate(loc_bn_name_dates = str_trim(str_remove_all(loc_bn_name_dates, " +"))) |>
  # a few dates contain words/letters ("century", stuff liek that). make them NA.
  mutate(loc_bn_name_dates = case_when(
    str_detect(loc_bn_name_dates, "[A-Za-z]") ~ NA,
    .default = loc_bn_name_dates
  )) |>
  separate(loc_bn_name_dates, into=c("d1", "d2"), sep = "-+", remove = F) |>
  mutate(across(c(d1, d2), parse_number)) |>
  mutate(name_bn_dates = case_when(
    d1==bn_yob & d2==bn_yod ~ "match",
    d1==bn_yob | d2==bn_yod ~ "part match",
    between(d1, bn_yob-2, bn_yob+2) | between(d2, bn_yod-2, bn_yod+2) ~ "near match",
    ( !is.na(bn_yob) | !is.na(bn_yod) ) & !is.na(loc_bn_name_dates) ~ "?"
  ))



## OUTPUT ####

## final selection for checking; include columns that might be useful

titles_names_reduced <-
  bn_women_loc_titles_names_reduced |> 
  select(record_id, bn_id,	bn_name,	
         loc_title_cln,	loc_date_pub_n,	
         bn_yob, bn_yod,	
         loc_bn_name_cln,  loc_bn_name_match, 
         loc_bn_name_terms,	loc_bn_name_dates,	
         loc_bn_name_seg_n, loc_bn_name_initials,  
         bn_in_all_names,  loc_bn_name_pos,
         loc_bn_name_field=loc_bn_name_field_no,	
         pub_bn_dates, name_bn_dates,
         n_names_record, 
         loc_all_names= loc_all_names_field,
         loc_sub_fields_avx,	
         loc_date_pub, loc_edition, loc_bib_level, loc_type_record,
         titles_n,	loc_pub_dates,	record_ids,
         loc_name_cln_std,
         loc_name_cln_std_s, 
         bn_query
  )  |>
  relocate(pub_bn_dates, name_bn_dates, .after = bn_yod)  |>
  arrange(record_id, loc_title_cln, loc_date_pub_n) 


## IN
# pub date between 1831 and 1955
# record type "a" (language materials)
# match name on at least one of _std or _std_s.
# published between bn birth+10 and bn death+10		
# (keep note if published between bn_yob and bn_yob + 10)
# drop a few erroneous male names

titles_names_reduced_in <-
  titles_names_reduced |>
  filter(between(loc_date_pub_n, 1831, 1955) & 
           loc_type_record=="a" & 
           !pub_bn_dates %in% c("before_yob", "after_yod+10", "in_yob_10") &
           !loc_name_cln_std_s %in% c("henryfrancois", "jonesherbert", "kerralexander", "smitheustace") &
           !name_bn_dates %in% c("?") &
           loc_bn_name_match !="no match"
  )


# OUT (rejected records, included in the xlsx for reference)
titles_names_reduced_out <-
  titles_names_reduced |>
  anti_join(titles_names_reduced_in, by="record_id")


# make the spreadsheet
# requires the {openxlsx} package https://cran.r-project.org/package=openxlsx
# bn_loc_list <-
#   list(
#     "reduced_in"=  titles_names_reduced_in,
#     "reduced_out"= titles_names_reduced_out
#   )
#  
## openxlsx::write.xlsx(bn_loc_list,
##     here::here("loc/xlsx/bn_loc_titles_20240318.xlsx")), colNames=TRUE)



