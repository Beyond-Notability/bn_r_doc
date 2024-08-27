xquery version "3.1";

(: 
top level datafields in the LOC XML files 
paths to input XML $infolder and output $outfolder will need adjustment.
uses https://docs.basex.org/main/File_Functions to write to CSV. (the sections of file:write code can be commented out for testing the script.)
:)

declare namespace output = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace zs = "http://www.loc.gov/zing/srw/";
declare namespace zsr ="http://www.loc.gov/MARC21/slim";

(: IMPORTANT: path/to/ $infolder and $outfolder need to be changed to point to existing local folders :)

let $infolder := 'path/to/xml/'
let $outfolder := 'path/to/csv/'

let $outdate := format-date(current-date(), "[Y]-[M01]-[D01]" ) 
let $outfilename := concat($outfolder, "loc_fields-", $outdate, ".csv")


(: beginning of file:write opening :)
return

file:write( $outfilename,

element csv 

{
(: end of file:write opening :)


for $doc in collection($infolder) 

(: get filename as bn id... base-uri is the whole path :)
let $id := replace(base-uri($doc),'^(.*/)(.*?)\.\w+$','$2')
(: extras - more than 100 records... idk yet whether this has worked correctly... :)
let $after := substring-after($id, "__")
let $before := substring-before($id, "__")


let $numberOfRecords := $doc//zs:numberOfRecords
let $query := $doc//zs:query

for $record at $record_count in $doc//zs:record
let $recordPosition := $record/zs:recordPosition (: if <100 records should be the same as count, but might be different in __extras? :)

for $fields in $record/zs:recordData/zsr:record

let $leader := $fields/zsr:leader
let $cf008 := $fields/zsr:controlfield[@tag='008']
let $cf001 := $fields/zsr:controlfield[@tag='001']

for $field at $field_count in $fields/zsr:datafield 
let $field_tag := $field/@tag

return

<record>
<id>{$id}</id>
<bn_id>{
  if (contains($id, '__') )
  then $before
  else $id
}</bn_id>
<number>{data($numberOfRecords)}</number>
<recordPosition>{data($recordPosition)}</recordPosition>
<record_count>{$record_count}</record_count>
<leader>{data($leader)}</leader>
<cf008>{data($cf008)}</cf008>
<cf001>{data($cf001)}</cf001>
<query>{data($query)}</query>
<field>{data($field_tag)}</field>
<field_count>{$field_count}</field_count>
</record>



(: beginning of file:write closing :)
}
 ,
map {
    'method': 'csv',
    'csv': map { 'header': 'yes', 'separator': ',' }
  }
)
(: end of file:write closing :)


