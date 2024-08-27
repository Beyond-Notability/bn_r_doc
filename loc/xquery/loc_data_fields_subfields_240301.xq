xquery version "3.1";

(: 
all the data fields and subfields in the LOC XML records, plus leader and control fields 1 and 8 which might be useful 
paths to input $doc and output $folder will need adjustment.
uses https://docs.basex.org/main/File_Functions to write output to CSV
:)


declare namespace output = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace zs = "http://www.loc.gov/zing/srw/";
declare namespace zsr ="http://www.loc.gov/MARC21/slim";

(: IMPORTANT: path/to/ $infolder and $outfolder need to be changed to point to existing local folders :)

let $infolder := 'path/to/xml/'
let $outfolder := 'path/to/csv/'


let $outdate := format-date(current-date(), "[Y]-[M01]-[D01]" ) 
let $outfilename := concat($outfolder, "loc_fields_subfields-", $outdate, ".csv")


(: beginning of file:write opening :)
return

file:write( $outfilename,

element csv 

{
(: end of file:write opening :)


for $doc in collection($infolder)

(: get filename as bn id... base-uri is the whole path :)
let $id := replace(base-uri($doc),'^(.*/)(.*?)\.\w+$','$2')
(: extras - more than 100 records... :)
let $after := substring-after($id, "__")
let $before := substring-before($id, "__")

let $numberOfRecords := $doc//zs:numberOfRecords
let $query := $doc//zs:query

for $record at $record_count in $doc//zs:record
let $recordPosition := $record/zs:recordPosition (: if <100 records should be same as record_count BUT might be different in __extras :)

for $fields in $record/zs:recordData/zsr:record

let $leader := $fields/zsr:leader
let $cf008 := $fields/zsr:controlfield[@tag='008']
let $cf001 := $fields/zsr:controlfield[@tag='001']

for $field at $field_count in $fields/zsr:datafield 
let $field_tag := $field/@tag
for $sub_field at $sub_field_count in $field/zsr:subfield
let $sub_code := $sub_field/@code
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
<query>{data($query)}</query>
<leader>{data($leader)}</leader>
<cf008>{data($cf008)}</cf008>
<cf001>{data($cf001)}</cf001>
<field>{data($field_tag)}</field>
<field_count>{data($field_count)}</field_count>
<sub_field>{data($sub_field)}</sub_field>
<sub_count>{data($sub_field_count)}</sub_count>
<code>{data($sub_code)}</code>

</record>



(: beginning of file:write closing :)
}
 ,
map {
    'method': 'csv',
    'csv': map { 'header': 'yes', 'separator': ',' }
  }
)
(: eng of file:write closing :)


