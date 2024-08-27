# shared libraries, functions etc ####
source(here::here("loc/R/shared.R"))

library(jsonlite)


## DATA ####

## post-checking XLSX 
## a few last-minute removals
loc_checked_xlsx <-
  read_excel(here::here("loc/xlsx/bn_loc_titles_20240619_post-checking.xlsx")) |>
  # overlooked that bn name is not a creator
  filter(!record_id %in% c("Q1154_012")) |>
  # overlooked a few duplicates
  filter(!record_id %in% read_lines(here::here("loc/csv/loc_dups_202408.txt")))



## LOC CSVs extracted from XML
## remove all records that are not in the checked xlsx 

loc_fields_with_subfields_csv <-
  read_csv(here::here("loc/csv/loc_fields_subfields-2024-03-01.csv"), show_col_types=FALSE) |>
  mutate(record_id = paste( bn_id, str_pad(recordPosition, 3, side="left", pad="0"), sep = "_" ))  |>
  rename_with(~paste0("loc_", .x), .cols=c(leader, field, sub_field, code, cf008, cf001) ) |>
  rename(bn_query=query, file_id=id, loc_records=number) |>
  select(-record_count) |>
  mutate(field_pad = str_pad(field_count, 3, pad="0"), sub_pad = str_pad(sub_count, 3, pad="0")) |>
  # [from old catalog] is in various places, so remove it at the beginning 
  mutate(loc_sub_field = str_remove(loc_sub_field, "\\[from *old *catalog\\]|\\[old catalog heading\\]")) |>
  # collapse multiple spaces into one; normalises slightly inconsistent LOC 010
  mutate(loc_sub_field = str_replace_all(loc_sub_field, "  +", " ")) |>
  relocate(record_id) |>
  # only keep records in checked xlsx
  semi_join(loc_checked_xlsx, by="record_id") 



## PROCESSING ####


## make reference IDs with MARC 010 - Library of Congress Control Number. 
## "Unique number assigned to a MARC record by the Library of Congress"

# lookup for loc 010 to internal project record ids 
loc_010_recordid <-
  loc_fields_with_subfields_csv |> 
  filter(loc_field=="010" & loc_code =="a") |>
  select(record_id, bn_id, loc_010=loc_sub_field)


# add loc_010 to data as ID column
loc_data_010 <-
  loc_fields_with_subfields_csv |> 
  left_join(loc_010_recordid |>
              distinct(loc_010, record_id) , by="record_id") |>
  relocate(loc_010) 


# remove unwanted columns and reduce any duplication
loc_data_bn_reduced <-
  loc_data_010 |> 
  distinct(loc_010, record_id, bn_id, file_id, loc_field, field_count, loc_sub_field, sub_count, loc_code, loc_records, recordPosition, bn_query) |>
  arrange(record_id, loc_field, loc_sub_field) |>
  relocate(loc_010, .after = record_id) 


# *without* the bn record_id 
loc_data_reduced <-
  loc_data_bn_reduced |>
  distinct(loc_010, loc_field, field_count, loc_sub_field, sub_count, loc_code)




## full titles [MARC 245]

loc_title <-
  loc_data_reduced |>
  filter( loc_field %in% c("245")  & loc_code %in% c("a", "b") ) |>  
  pivot_wider(id_cols = loc_010, values_from = loc_sub_field, names_from = loc_code) |>
  mutate(a = str_replace(a, " +: *$", ":")) |>
  mutate(across(c(a, b), ~str_remove(., "\\.\\.\\.+ */?$|^ *\\.\\.\\. *"))) |>
  mutate(across(c(a, b), str_trim)) |>
  unite(c(a, b), col="title", sep=" ", na.rm = T) |>
  mutate(title = str_remove(title, " *[/,;:] *$")) |>
  mutate(title = case_when(
    str_detect(title, "[A-Z]\\. *$") ~ title,
    str_detect(title, "\\. *$") ~ str_remove(title, "\\. *$"),
    .default = title
  )) |>
  mutate(title = str_replace_all(title, "  +", " "))


## subjects [MARC 650]

loc_terms_unnested <-
  loc_data_reduced |> 
  filter(loc_field=="650" & loc_code %in% c("a", "v", "x", "y", "z")) |> 
  arrange(loc_010, loc_code) |>
  mutate(loc_sub_field = str_remove(loc_sub_field, "\\. *$")) |>
  distinct(loc_010, keyword=loc_sub_field) 


## loc call number [MARC 050] 
loc_cn_050 <-
  loc_data_reduced |>
  filter(loc_field=="050") |>
  distinct(loc_010, loc_sub_field, loc_code) |>
  filter(!str_detect(loc_sub_field, "[a-z]")) |>
  # keep first of each code only if more than one.
  group_by(loc_010, loc_code) |>
  top_n(-1, row_number()) |>
  ungroup() |>
  pivot_wider(id_cols =loc_010, values_from = loc_sub_field, names_from = loc_code) |>
  unite(c(a, b), col="loc_050", na.rm = TRUE, sep = " ") 




## publication details [MARC 260, 264]

loc_publication <-
  loc_data_reduced |> 
  select(loc_sub_field, loc_code, loc_field, loc_010) |>
  # c=date. b=publisher occasionally contains pub date. a=place
  filter( loc_field %in% c("260", "264")  & loc_code %in% c("a", "b", "c") )  |>
  # keep first of each only if more than one.
  group_by(loc_010, loc_field, loc_code) |>
  top_n(-1, row_number()) |>
  ungroup() |>
  pivot_wider(id_cols=loc_010, values_from = loc_sub_field, names_from = loc_code) |>
  # some cleaning up of dates
  mutate(loc_date = case_when(
    !is.na(c) ~ c,
    str_detect(b, "\\b(1[89]\\d\\d)") ~ str_extract(b, "\\b1[89]\\d\\d.*$")
  )) |>
  mutate(loc_date = str_remove(loc_date, "\\. *$")) |>
  # make a numerical year for sorting and suchlike
  mutate(loc_date_n = parse_number(str_extract(loc_date, "\\d\\d\\d\\d"))) |>
  # light cleanup of place and publisher.
  mutate(loc_place = str_remove(a, " *[,;:] *$")) |>
  mutate(loc_publisher = str_remove(b, " *[,;:] *$")) |>
  mutate(across(c(loc_place, loc_publisher, loc_date), ~str_remove_all(., "\\[|\\]"))) |>
  mutate(across(c(loc_place, loc_publisher), ~str_replace_all(., "  +", " "))) |>
  # clean date for whatever. keep yyyy-yyyy, but otherwise only first date, drop c etc.
  mutate(loc_date_cln = str_remove(loc_date, "^[A-Za-z., ]+|[a-z]+$|([iv]\\.|, c|[&/] *).*$")) |>
  mutate(loc_date_cln = str_remove_all(loc_date_cln, ", *$|[()]")) |>
  mutate(across(c(loc_place, loc_publisher, loc_date_cln), str_trim)) |>
  select(-a, -b, -c) |>
  arrange(loc_010)




## names [MARC 100, 700]

## reuse cleaned xlsx for bn linking info
loc_checked_xlsx_names <-
  loc_checked_xlsx |>
  select(record_id, bn_id, bn_name, loc_bn_name_cln, loc_bn_name_match, loc_bn_name_terms, loc_bn_name_dates, bn_in_all_names, loc_bn_name_pos, loc_bn_name_field, n_names_record, loc_all_names)  |>
  mutate(role = case_when(
    str_detect(loc_bn_name_terms, "\\b(ed|comp)") ~ "editor"
  )) |> 
  inner_join(loc_010_recordid |> select(record_id, loc_010), by="record_id") |>
  relocate(loc_010)

loc_checked_xlsx_names_unnested <-
  loc_checked_xlsx_names |>
  select(record_id, loc_010, loc_all_names) |>
  unnest_regex(name, loc_all_names, pattern = " +\\| +", to_lower = F) |>
  separate(name, into=c("name_pos", "name"), sep = " ", extra = "merge", convert = T) |>
  mutate(name_type = str_extract(name, "\\d+ *$")) |>
  mutate(name = str_remove(name, " *\\d+ *$"))


loc_names_fields <-
  loc_data_reduced |>
  distinct(loc_010, loc_field, field_count, loc_sub_field, sub_count, loc_code) |>
  arrange(loc_010, loc_field, field_count, sub_count, loc_code ) |>
  filter(loc_field %in% c("100", "700") & loc_code %in% c("a", "c", "d", "e")) 


loc_names_wide <-
  loc_names_fields  |>
  # a few people have more than one role; collapse into one 
  group_by(loc_010, loc_field, field_count, loc_code) |>
  summarise(loc_sub_field = paste(loc_sub_field, collapse = " "), .groups = "drop_last") |>
  ungroup() |>
  pivot_wider(id_cols = c(loc_010, loc_field, field_count), names_from = loc_code, values_from = loc_sub_field )|>  
  # sort again to ensure original order
  arrange(loc_010, loc_field, field_count)  |>  
  group_by(loc_010) |>
  mutate(name_pos = row_number()) |>
  ungroup()  |>
  left_join(loc_checked_xlsx_names_unnested |> select(-name), by=c("loc_010",  "name_pos", "loc_field"="name_type")) |>
  relocate(record_id, name_pos, a, .after = loc_010)  |>
  left_join(loc_checked_xlsx_names |> select(loc_010, record_id, bn_name, bn_id, loc_bn_name_pos), by=c("loc_010", "record_id")) |>
  # dedup - keep original pos for reference.
  group_by(loc_010, record_id, a, e, loc_field, c, d, bn_name, loc_bn_name_pos) |>
  top_n(-1, field_count) |>
  ungroup() 


loc_names_wide_cleaned <-
  loc_names_wide  |>
  # get rid of trailing commas
  mutate(loc_name = str_remove(a, ",+ *$"))  |>
  # [brackets]
  mutate(loc_name = str_remove_all(loc_name, "[\\[\\]()]")) |>
  # name (but not initial) ends with full stop
  mutate(loc_name = str_remove(loc_name, "(?<=[a-z])\\. *$")) |>
  # rogue extra spaces (hopefully none of these in the query strings...)
  mutate(loc_name = str_replace_all(loc_name, "  +", " ")) |>
  mutate(loc_name = str_trim(loc_name))  |>
  # drop a few erroneous non personal names
  filter(!str_detect(loc_name, regex("\\b(parish|family|New Romney)", ignore_case=T))) |>
  # drop a few names who don't look like direct contributors.
  filter(!e %in% c("former owner.", "printer.")  | is.na(e)) |>
  # creator types. **author unless some other info** 
  mutate(creator_type = case_when(
    str_detect(e, "\\b(ed|editor)\\b") ~ "editor", # before translator
    str_detect(e, "\\b(tr)") ~ "translator",
    str_detect(e, "\\b(comp|compiler)\\b") ~ "editor", # after translator
    str_detect(e, "author") ~ "author",
    str_detect(e, "\\b(ill)") & bn_id=="Q3767" ~ "author",
    str_detect(e, "\\b(ill)") ~ NA,
    is.na(e) ~ "author", 
  )) |>
  filter(!is.na(creator_type)) |>
  mutate(dates = str_replace_all(d, "approximately", "c.")) |>
  mutate(dates = str_remove(dates, "[.,;:]+ *$"))  |>
  relocate(e, creator_type, dates, .after = loc_name)  |>
  # drop bn_ id and name 
  select(-bn_id, -bn_name) |>
  left_join(
    loc_names_wide |>
      distinct(loc_010, bn_id, bn_name, loc_bn_name_pos) |>
      mutate(pos_is_bn = "y"), by=c("loc_010", "name_pos"="loc_bn_name_pos")
  ) |>
  distinct(loc_010, name_pos, loc_name, creator_type, dates, loc_field, field_count, pos_is_bn, bn_id, bn_name) |>
  arrange(loc_010, name_pos) |>
  # is the name a BN
  mutate(pos_is_bn = case_when(
    pos_is_bn=="y" ~ pos_is_bn, 
    .default = "n")) |>
  # following removals, renumber names 
  group_by(loc_010) |>
  mutate(name_sort = row_number()) |>
  ungroup()  



## OUTPUTS ####


## summarised version as tabular format, one bn-work per row. 
## multiple names are collapsed into a pipe-separated list. leave out subject terms.

loc_bn_names_works <-
  loc_names_wide_cleaned |>
  filter(!is.na(bn_id)) |>
  inner_join(loc_010_recordid, by=c("loc_010", "bn_id")) |>
  select(bn_name, bn_id, record_id, loc_010, loc_name, creator_type, bn_pos= name_sort) |>
  inner_join(loc_title, by="loc_010") |>
  left_join(loc_publication |> 
              select(-loc_date) |> 
              rename(date=loc_date_cln, year_n=loc_date_n, place=loc_place, publisher=loc_publisher) |>
              relocate(date, year_n, .after = loc_010)
            , by="loc_010") |>
  # simple list of all names
  inner_join(
    loc_names_wide_cleaned |>
      group_by(loc_010) |>
      summarise(names_n=n(), names = paste(loc_name, collapse = " || ")) |>
      ungroup(), by="loc_010"
  )  |> 
  relocate(bn_pos, record_id, loc_010, .after = last_col()) |>
  rename(lccnID=loc_010) |> 
  arrange(loc_name, year_n)


## file: "loc/outputs/loc_bn_names_records.csv"




## fuller version in bibtex format; can be imported into Zotero

## function adapted from bib2df::df2bib which saves a DF to a .bib file
## a useful library BUT original df2bib doesn't include translator in creator list
## (also it converts var names to uppercase which makes my eyes hurt.)

df2bib_lc <- function(x, file = "", append = FALSE, allfields = TRUE) {

  if (!is.character(file)) {
    stop("Invalid file path: Non-character supplied.", call. = FALSE)
  }
  if (as.numeric(file.access(dirname(file), mode = 2)) != 0 && file != "") {
    stop("Invalid file path: File is not writeable.", call. = FALSE)
  }

  if (any({df_elements <- sapply(x$author, inherits, "data.frame")})) {
    x$author[df_elements] <- lapply(x$author[df_elements], na_replace)
    x$author[df_elements] <- lapply(x$author[df_elements],
                       function(x) {
                         paste(x$last_name,
                               ", ",
                               x$first_name,
                               " ",
                               x$middle_name,
                               sep = "")
                         }
                       )
    x$author[df_elements] <- lapply(x$author[df_elements], trimws)
  }

  #names(x) <- toupper(names(x))
  fields <- lapply(seq_len(nrow(x)), function(r) {
    rowfields <- rep(list(character(0)), ncol(x))
    names(rowfields) <- names(x)
    for (i in seq_along(rowfields)) {
      f <- x[[i]][r]
      if (is.list(f)) {
        f <- unlist(f)
      }
      rowfields[[i]] <- if (!length(f) | any(is.na(f))) {
          character(0L)
        } else if (names(x)[i] %in% c("author", "editor", "translator")) {
          paste(f, collapse = " and ")
        } else {
          paste0(f, collapse = ", ")
        }
    }
    rowfields <- rowfields[lengths(rowfields) > 0]
    rowfields <- rowfields[!names(rowfields) %in% c("category", "bibtexkey")]
    ########################################################################
    # This only uses the non-empty fields for the bib file (adds '[nzchar(rowfields)]')
    # if 'allfields' is set to FALSE
    if(allfields) {
      paste0("  ",
             names(rowfields),
             " = {",
             unname(unlist(rowfields)),
             "}",
             collapse = ",\n")
    } else {
      paste0("  ",
             names(rowfields[nzchar(rowfields)]),
             " = {",
             unname(unlist(rowfields[nzchar(rowfields)])),
             "}",
             collapse = ",\n")
    }
    ####################################################################
  })
  cat(paste0("@",
             x$category,
             "{",
             x$bibtexkey,
             ",\n",
             unlist(fields),
             "\n}\n",
             collapse = "\n\n"),
      file = file,
      append = append)
  invisible(file)
}

## make a bibtexkey using name and loc_010 so guaranteed to be unique
loc_bibtexkey <-
  loc_names_wide_cleaned |>
  filter(row_number()==1, .by = loc_010) |>
  mutate(bibtexkey = word(loc_name)) |>
  mutate(bibtexkey = str_remove_all(bibtexkey, "[:punct:]")) |>
  mutate(bibtexkey = paste0(bibtexkey, str_remove_all(loc_010, " ") )) |>
  # replace diacritics with non-d letters. requires {stringi} package
  mutate(bibtexkey = stringi::stri_trans_general(bibtexkey, id = "Latin-ASCII")) |>
  select(loc_010, bibtexkey)


## for bibtex each creator type needs to be in a distinct field
## can be multiple names so list-columns are needed
loc_authors <-
  loc_names_wide_cleaned |>
  select(loc_010, loc_name, creator_type) |>
  filter(creator_type=="author") |>
  group_by(loc_010) |>
  summarise(author = list(loc_name)) |>
  ungroup()

loc_editors <-
  loc_names_wide_cleaned |>
  filter(creator_type=="editor") |>
  group_by(loc_010) |>
  summarise(editor = list(loc_name)) |>
  ungroup()

loc_translators <-
  loc_names_wide_cleaned |>
  filter(creator_type=="translator") |>
  group_by(loc_010) |>
  summarise(translator = list(loc_name)) |>
  ungroup()


## preserve information about BN people's roles in the work:
## a note field to list BN IDs, names and pos for reference/linkage
loc_names_bn_note <-
  loc_names_wide_cleaned |>
  filter(pos_is_bn=="y") |>
  distinct(loc_010, bn_id, bn_name, loc_name, creator_type, name_sort) |>
  mutate(lead = if_else(name_sort==1, " [1]", "")) |>
  mutate(bn_person = glue("BN ID: {bn_id} | name: {loc_name} | creator type: {creator_type}{lead}")) |>
  group_by(loc_010) |>
  summarise(note = glue_collapse(bn_person, "\n\n")) |>
  ungroup()


## put the fields together
loc_bn_records_bib <-
  loc_title |>
  inner_join(loc_publication |> select(year=loc_date_cln, address=loc_place, publisher=loc_publisher, loc_010), by="loc_010") |>
  inner_join(loc_bibtexkey, by="loc_010") |>
  left_join(loc_cn_050 |> rename(callNumber=loc_050), by="loc_010") |>
  left_join(loc_names_bn_note, by="loc_010") |>
  left_join(loc_authors, by="loc_010") |>
  left_join(loc_editors, by="loc_010") |>
  left_join(loc_translators, by="loc_010") |>
  left_join(
    loc_terms_unnested |>
      group_by(loc_010) |>
      nest(keywords=c(keyword)) |>
      ungroup(), by="loc_010"
  )  |>
  mutate(permalink = paste0("https://lccn.loc.gov/", str_remove_all(loc_010, " ")))|>
  rename(lccnID=loc_010) |> 
  mutate(category="book") |>
  mutate(libraryCatalog = "catalog.loc.gov")



## to write to .bib file using the function above: loc_bn_records_bib |> df2bib_lc(here::here("loc/outputs/loc_bn_records.bib"))






## JSON version using list-columns for variables with multiple values; Zotero-ish

loc_bn_records_json <-
  loc_title |>
  inner_join(
    loc_publication |> 
      select(date=loc_date_cln, place=loc_place, publisher=loc_publisher, loc_010), by="loc_010") |>
  left_join(loc_cn_050 |> rename(callNumber=loc_050), by="loc_010") |>
  # preserve info about BN people's roles
  inner_join(
    loc_names_wide_cleaned |>
      select(loc_010, name=loc_name, creatorType=creator_type, bn_id, bn_name, namePos= name_sort) |>
      group_by(loc_010) |>
      nest(creators=c(name, creatorType, namePos, bn_id, bn_name)) |>
      ungroup(), by="loc_010"
  ) |>
  left_join(
    loc_terms_unnested |>
      group_by(loc_010) |>
      nest(keywords=c(keyword)) |>
      ungroup(), by="loc_010"
  )  |>
  mutate(permalink = paste0("https://lccn.loc.gov/", str_remove_all(loc_010, " "))) |>
  rename(lccnID=loc_010) |>
  mutate(itemType="book") 


## file: "loc/outputs/loc_bn_records.json"
