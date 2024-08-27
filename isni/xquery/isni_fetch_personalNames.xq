xquery version "3.1";

(: 
XQuery script run in BaseX  https://basex.org/download/
The file:write function to save to CSV uses the File Module: https://docs.basex.org/wiki/File_Module
NB: the code was written for XML data downloaded from the ISNI API in late 2023. See https://isni.org/page/technical-documentation/
The script doesn't extract all the data fields available in the XML files; it simply extracts each surname and forename in <personalName> elements in the XML files and saves them to a single CSV file.
:)

declare namespace output = "http://www.w3.org/2010/xslt-xquery-serialization";

(: IMPORTANT: path/to/ $infolder and $outfolder need to be changed to point to existing local folders :)

let $infolder := 'path/to/xml/' (: location of the input XML files :)
let $outfolder := 'path/to/csv/' (: location for the output CSV file :)

let $outdate := format-date(current-date(), "[Y]-[M01]-[D01]" ) 
let $outfilename := concat($outfolder, "isni_personalName-", $outdate, ".csv")


(: file:write code :)
return

file:write( $outfilename,

element csv 

{
(: file:write code :)

for $doc in collection($infolder)

let $isni:= $doc//isniUnformatted

for $name at $count in $doc//personalName

let $forename := $name/forename
let $surname := $name/surname


return

<record>
<isni>{data($isni)}</isni>
<name_count>{$count}</name_count>
<surname>{data($surname)}</surname>
<forename>{data($forename)}</forename>
</record>



(: file:write code :)
}
 ,
map {
    'method': 'csv',
    'csv': map { 'header': 'yes', 'separator': ',' }
  }
)
(: file:write code :)


