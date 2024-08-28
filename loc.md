# A bibliographic dataset for Beyond Notability

## Introduction

The `loc/` directory in this repository contains scripts and data files for the creation of a bibliographic dataset of books by women in the BN database which are in the [Library of Congress online catalogue](https://catalog.loc.gov/).

This document records the key steps and decision making for each stage of the creation process.

Note: while it should be possible to successfully run the code referenced in this document, the results of queries to the project database and the LOC catalogue API are *not* exactly reproducible since neither of the sources are static objects. Reference copies of all the data obtained at the time (February 2024) are included in the repository.


### summary of the workflow

- create a subset of BN women for searching the catalogue
- build name variant strings for API queries from ISNI and the BN database
- send queries to the LOC API and save results in xml files
- extract the XML data into CSV format to make it easier to work with
- process the CSV data to create an interim spreadsheet for checking
- manual checking
- final processing and creation of dataset versions


### files overview

- `loc/csv/` - interim CSV files
- `loc/outputs/` - final dataset files
- `loc/R/` - R scripts
- `loc/xlsx/` - interim Excel spreadsheets
- `loc/xml/` - downloaded XML files
- `loc/xquery/` - XQuery scripts


## 1. Select a subset of BN women and prepare API queries

R script:

- `loc/R/bn.R`

Associated files:

- `loc/csv/bn_women_loc_240227.csv` - list of the selected women
- `loc/csv/isni_personalName-2023-10-09.csv` - ISNI data [see `isni.qmd`]
- `loc/csv/bn_women_loc_summary_240228.csv` - query strings for the API


### criteria for inclusion

- gender=female
- 8+ statements in the BN wikibase (at 27/2/2024)
- must have first name or initials (ie exclude "Miss Lastname" or "Mrs Lastname")
- where birth/death dates are available, filter out people who were born before 1750 or died before 1860 

The 8 statements cutoff was chosen after some experimentation because it suggested that enough was known about a woman to aid reasonably confident identification of uncertain cases. For example, very few women with fewer than 8 statements had at least one of birth/death date.

Similarly, the absence of a first name or initials would make it difficult to identify an individual's work with any confidence.

The birth/death dates filter was an attempt to exclude a number of "historical" people such as Elizabeth Montagu who are only in the database to record a family connection. In practice the statements criteria was probably more effective.


### build list of name variants for API query strings

Names were constructed in a standard "Surname, Forename(s)/Initial(s)" format for the API query strings. 

Women might have published under several different names during their lifetiems - for example because of marriage, as well as choices to publish using full names or initials - and so it was desirable to maximise the likelihood of finding their publications (at the risk of generating false positives). Two main sources were used: surname information in the wikibase itself, and ISNI (International Standard Name Identifiers) data.

- ISNI data which had previously been fetched via the ISNI API and lightly cleaned. [See `isni.qmd` for more detail.]
- birth/married surnames in the BN database, where available
- a hand-built list for a very small handful of individuals who had neither ISNI nor birth/married surnames

One possible source *not* used was [VIAF](https://viaf.org/), because it added very little information not provided by ISNI, and its data was considerably messier and harder to work with.



## 2. Fetch data from the LOC API and save as XML

R script:

- `loc/R/api.R`

Associated files:

- `loc/xml/*.xml` - 563 XML files containing the downloaded XML data

Elements of the script:

- use the query strings created in the previous script to fetch data from the LOC API
- write the downloaded data to XML files (one per BN woman).


### notes on the API

The API is a Search/Retrieval via URL (SRU) API which returns XML [(full documentation)](https://www.loc.gov/standards/sru/sru-1-1.html).


In query strings, names are separated by "or" and URL-encoded, for example:

- http://lx2.loc.gov:210/lcdb?version=1.1&operation=searchRetrieve&maximumRecords=100&query=dc.creator%3D%22Cecil%2C%20Mary%20Rothes%20Margaret%22%20or%20dc.creator%3D%22Amherst%2C%20Mary%20Rothes%20Margaret%22%20or%20dc.creator%3D%22Tyssen-Amherst

Issues encountered for querying:

- searches can't be limited by date
- the number of results returned per query is limited to 100 (this was rarely a problem)
- there also appeared to be a limit on the number of name variants (or length of query string) that could be passed to one API query, so a very small number of queries needed to be truncated.
- searches are *not* exact matching, even when using quotation marks around names; they work more like wildcard searches

For example, a query string like "Brown, Mary" will also return results for "Brown, Mary Ann", "Brown, Mary E.", etc. On balance, this proved to be helpful for finding additional name variants, but it increased the likelihood of false positives (and made it more difficult to match names in the LOC data to the original query names).

Fields and subfields in the XML returned by the LOC API are based on the [MARC 21 bibliographical format](https://www.loc.gov/marc/bibliographic/).

Example of the XML for the [Title Statement field (MARC 245)](https://www.loc.gov/marc/bibliographic/concise/bd245.html) of a work:

          <datafield tag="245" ind1="1" ind2="0">
            <subfield code="a">English ecclesiastical studies;</subfield>
            <subfield code="b">being some essays in research in medieval history,</subfield>
            <subfield code="c">by Rose Graham.</subfield>
          </datafield>

The <datafield> @tag attribute encodes the main MARC field (245); the <subfield> @code encodes subfield codes ($a, $b and $c).


## 3. Convert XML to CSV for processing

Scripts:

- `loc/xquery/loc_data_fields_240301.xq` - extracts data fields only (helpful as an overview)
- `loc/xquery/loc_data_fields_subfields_240301.xq` - extracts all data fields and subfields and content

Associated files:

- `loc/csv/loc_fields-2024-03-01.csv`
- `loc/csv/loc_fields_subfields-2024-03-01.csv`

The two XQuery scripts (run in [BaseX](https://basex.org/)) were used to pull the data out of the XML files into a more usable CSV format. 

Every datafield and subfield was extracted from the XML, in addition to some control fields that looked potentially useful. No additional processing was carried out at this stage. 



## 4. Process the extracted CSV data 

Remove false positives, reduce to a single record per person per work and save to a spreadsheet for manual checks.

R script:

- `loc/R/process.R`

Associated file:

- `loc/xlsx/bn_loc_titles_20240318.xlsx`


Key elements of the script:

- standardise dates
- standardise names
- match BN to LOC names
- reduce multiple records of works to a single record
- remove false positives


Features of the extracted data that complicated processing:

- there can be multiple records for the same work (because of different editions or versions that have been catalogued as distinct items)
- many works have multiple associated creator names
- while an API query can be restricted to names (not just keyword searches), several different name fields appear to be searched, and the precise role of any named person is not always clear
- more general variations in use of MARC fields are a trap for the unwary and non-expert MARC user (ie: me!) 
- there is plenty of minor messiness and inconsistency within fields, eg trailing punctuation, hyphenation, variations in capitalisation and sometimes the insertion of extra information in square brackets


### standardise dates for processing 

This was done to enable chronological ordering of works and comparison with BN people's dates

The main MARC publication date field is 260c (and very occasionally the date is mixed up in 260b) but about 300 items instead have dates in 264c. 

Standardising involves

- removal of any approximation and uncertainty (c. [xxxx] ? etc)
- anything less precise than a year is removed (eg "18--" or "195-"; there were not many of these)
- for a date range, the first year only is used

The original date string was retained for reference 

Author birth-death dates where present (usually yyyy-yyyy) were split into separate birth and death years.

Note: the sources and reliability of the LOC author dates are unknown (although in many cases they did seem to closely match BN data).



### match LOC names to BN names

Because many of the works in the LOC data had multiple associated names, and the wildcard-style search of the API often returned names that didn't exactly match the query strings I'd sent, this was rather more complex than I anticipated initially.

Various inconsistencies in name strings necessitated normalisation of name strings ("_std"):

- convert to lower case
- strip out all punctuation and spaces
- convert accented characters to non-accented

To approximate the API's wildcard style matching, a short version of the standardised name consisting of "surname, first word/initials" only was created ("_std_s")

Additionally, there appears to be a lack of consistency/context for the names. Two MARC fields (100 and 700) included names of primary creators (authors, editors, translators, etc). The primary one seems to be intended to be 100, but 700 is also very frequently used and clearly includes creators. (Some records *only* have 700 names.)
  
Unless other information about the role of a name (eg, editor) is provided it's assumed that they are an author. This assumption is probably quite dubious.


### reduce multiple records for the same work to a single record

It's quite common for one work to have several different records (for different editions and suchlike) in the LOC catalogue. Only the earliest dated record was retained in these cases (the small number of undated works were removed).

It was difficult to do this with complete accuracy on a first iteration. Punctuation/case variations could be handled fairly easily, but there can be other minor variations in recorded titles that are harder to deal with. A few were overlooked at this stage and picked up in final processing.


### remove false positives

The criteria used for identifying works that were *probably* by BN women were primarily based on dates and names (following standardisation and cleaning) and leaned towards *excluding* anything uncertain or ambiguous, in order to reduce the amount of manual checking that would be needed.


publication dates:
  
- anything lacking a publication date was removed (this was quite a small number)
- anything with an earliest publication date after 1955 (in order to focus on publications during the period of study)
- anything published before 1831 (not a large number; possibly by "historical" people who were missed by the initial filters) 


creator names:

- records that failed to match BN name on at least one of _std or _std_s (nearly all of these were very obvious false matches)
- a few erroneous male names (due to issues I overlooked in the ISNI data at an earlier stage)


where BN birth/death data was available:

- anything published before the BN year of birth *and* up to 10 years after birth
- anything first published more than *10* years after the BN year of death (to focus on activity during lifetime)


where *both* BN and LOC birth/death data were available:

- LOC birth-death dates that were significantly outside the BN dates (retained all exact, near (plus/minus 2 years) and partial matches)

other:

- anything with a MARC record type other than "a" (language materials) 



## 5. Manual checks of ambiguous/uncertain cases 

The initial dataset created in step 4 was exported to a spreadsheet for review by Amara Thornton, with focus on domain expertise and review of ambiguous names.

Both the exported and post-checking spreadsheets are saved in `loc/xlsx`.



## 6. Processing to create final dataset outputs

R script:

- `loc/R/final.R`

Output files:

- `loc/outputs/`


Key elements of the script:

- import the post-checking XLSX
- final filtering (a few belatedly identified duplicates)
- selection and cleanup of variables for inclusion in data
  - this repeats some methods used in stage 4, though with a different focus for a public release
- create final dataset in multiple formats




## Dataset variables and versions

Bibliographic data is complex and may have multiple uses. To facilitate this, three different formats have been provided here. 
It's also possible to use the `final.R` script as a basis to create other versions/formats if preferred.

Two of the formats are *per work* and might be imported into software like Zotero or used for an analysis of the larger network of the BN women's collaborators. The third is a tabular summary of the data *per BN name per work* and as such loses some of the detail in the other two versions, but would be easier to work with for quantitative analysis of BN women's publishing activity.

Both of the per-work formats preserve information about the BN women and their roles as far as possible.


### variables included

All data sourced from the Library of Congress catalogue is based on the [MARC 21 format](https://www.loc.gov/marc/bibliographic/), which should be referred to for detailed documentation.

**Dataset IDs:**

- `lccnID` - Library of Congress control number (MARC field 010). (In the tabular version this is not a unique ID.)
- `record_id` - BN project-based unique ID 

**Beyond Notability people:**

- `bn_id` - the item identifier (beginning with the letter Q) of a woman in the Beyond Notability database 
- `bn_name` - the full name recorded for a woman in the Beyond Notability database (potentially could change, though this is rare)

It's possible, given the methods of creation outline above, that there could be BN people among creators who are not identified as such.

**Library of Congress work information:**

- `title` (MARC field 245)
- `date` of publication (publication date, place and publisher: MARC fields 260 and 264)
- `place` of publication 
- `publisher`

The `date` has been lightly cleaned; includes date ranges.

**Library of Congress names (MARC 100 and 700):**

There are three `name` types: author, editor and translator

- "editor" and "translator" are derived from information in the MARC subfield $e ("relator term"); "author" is a residual category for any names that were not categorised or excluded on the basis of relator terms and may not always be accurate
- names have been lightly cleaned but not standardised; some further processing may be needed for work such as network analysis

**Library of Congress topical terms (MARC field 650):**

- subject `keywords` extracted into a flat list. These are highly variable and not consistently supplied in the catalogue, so no attempt has been made to preserve any organisation or categorisation in MARC subfields.

**Additional Library of Congress identifiers:**

- `call number` (MARC field 050)
- `permalink` in the online catalogue, based on lccnID



### versions


1. JSON (per work)

`loc/outputs/loc_bn_records.json`

This aims to preserve as much information as possible, so it contains nested fields for variables with multiple values. Variable names (where relevant) should match the current [Zotero API](https://www.zotero.org/support/dev/web_api/v3/start). It may be suitable for network analysis, though names would need further cleaning and matching.

- `lccnID`
- `title`
- `date`
- `place`
- `publisher`
- `callNumber`
- `creators`
  - `name`
  - `creatorType`  (author, editor or translator; see note above on the "author" category)
  - `namePos` (position of name (in cataloguing order) if there are multiple creator names)
  - `bn_id` (BN ID if the name has been identified as one of the BN people)
  - `bn_name` (BN name if identified as a BN person)
- `keywords`
  - `keyword`
- `permalink` 
- `itemType` (all "book")



2. BibTex (per work)

`loc/outputs/loc_bn_records.bib`

This can be directly [imported into a Zotero library](https://www.zotero.org/support/kb/importing_standardized_formats). All fields apart from `lccnID` are standard BibTex fields. Some fields can contain multiple values.

(Installation of the Zotero BetterBibTex extension is helpful for importing BibTex data into Zotero.)

- `lccnID` 
- `title`
- `year` (renamed `date`)
- `address` (renamed `place`)
- `publisher`
- `bibtexkey` (formed of first creator name and lccnID)
- `callNumber`    
- `note` (contains information about identified BN people)
- `author`
- `editor`
- `translator`
- `keywords`
- `permalink`
- `category` (item type; all "book")
- `libraryCatalog`

notes: 

- For this format, names are separated out into creator types
- `note` should import into Zotero as a "Note" listing each BN woman involved on a separate line
- `libraryCatalog` and `callNumber` may not import into Zotero quite as expected (this was not for want of trying!)


3. CSV (per BN person-per work)

`loc/outputs/loc_bn_names_records.csv`

Tabular data summarising information for each work by each identified BN person. Contains additional information about creator count and whether the BN name is the first author, where there are multiple names.

- `bn_name`
- `bn_id`
- `loc_name` (Library of Congress version of the BN name)
- `creator_type` (author, editor or translator; see note above on the "author" category)
- `title`
- `date` 
- `year_n` (first year only if publication date is a range)
- `place`
- `publisher`
- `names_n` (count of LoC creator names for the work)
- `names` (pipe-separated list of the LoC names, in original cataloguing order)
- `bn_pos` (position of the BN name in the list of LoC names)
- `record_id` (unique identifier in this version of the data, based on bn_id)
- `lccnID`


