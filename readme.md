*An open-source implementation of NASA's 'NASAView'.* The purpose of this
program if provide a ported command line version of the decompression in order
to allow others to view the historic planetary images.

**Dislaimer:** The content in `archive` is for reference only. Please refer to
the license specifically in those source files.

**Reason:** Whilst NASA's ['NASAView'](pds.nasa.gov/tools/nasa-view.shtml) is
currently open to the public to be used for image conversion, it's possible it
will not be forever. It's also closed source, meaning you can't entirely be
sure of what you are running. By writing an open source Java implementation,
all machines with a JVM should be able to run the program and perform
bug-fixes/extensions where required.

# Building

You'll need Java 7 or above and [Apache Ant](http://ant.apache.org/). Then you
can simply run:

```
ant
```

You should see `BUILD SUCCESSFUL` if all went well.

# Example Usage

The following is an example of how to convert an
[IMQ Voyager image](archive/images/c3495731.imq) to a PNG with engineering
information:

```
java -jar dist/openview.jar -i archive/images/c3495731.imq -o saturn.png -f json
```

![Output `saturn.png` from the above command.](doc/saturn.png)

JSON output from the above command (`saturn.png.txt`):

```json
{
  "IMAGE.LINE_SAMPLES":"800",
  "LABEL_RECORDS":"54",
  "IMAGE.SAMPLE_TYPE":"UNSIGNED_INTEGER",
  "ENCODING_HISTOGRAM.ITEM_TYPE":"VAX_INTEGER",
  "IMAGE.LINE_SUFFIX_BYTES":"36",
  "SPACECRAFT_NAME":"VOYAGER_1",
  "SCAN_MODE_ID":"\'3:1\'",
  "EDIT_MODE_ID":"\'1:1\'",
  "NJPL1I00PDS100000000":"SFDU_LABEL",
  "SHUTTER_MODE_ID":"WAONLY",
  "^IMAGE":"61",
  "EXPOSURE_DURATION":"2.8800 <SECONDS>",
  "RECORD_BYTES":"836",
  "IMAGE_NUMBER":"34957.31",
  "^IMAGE_HISTOGRAM":"55",
  "INSTRUMENT_NAME":"WIDE_ANGLE_CAMERA",
  "IMAGE.LINES":"800",
  "IMAGE_HISTOGRAM.ITEM_TYPE":"VAX_INTEGER",
  "ENGINEERING_TABLE.^STRUCTURE":"\'ENGTAB.LBL\'",
  "NOTE":"\"WA MOSAIC OF EAST ANSA\"",
  "ENCODING_HISTOGRAM.ITEM_BITS":"32",
  "FILTER_NUMBER":"2",
  "IMAGE_HISTOGRAM.ITEM_BITS":"32",
  "IMAGE.SAMPLE_BIT_MASK":"2#11111111#",
  "ENGINEERING_TABLE.BYTES":"242",
  "ENCODING_HISTOGRAM.ITEMS":"511",
  "IMAGE.ENCODING_TYPE":"HUFFMAN_FIRST_DIFFERENCE",
  "GAIN_MODE_ID":"LOW",
  "TARGET_NAME":"S_RINGS",
  "IMAGE.^LINE_SUFFIX_STRUCTURE":"\'LINESUFX.LBL\'",
  "IMAGE_HISTOGRAM.ITEMS":"256",
  "MISSION_PHASE_NAME":"SATURN_ENCOUNTER",
  "^ENCODING_HISTOGRAM":"57",
  "FILE_RECORDS":"860",
  "^ENGINEERING_TABLE":"60",
  "EARTH_RECEIVED_TIME":"1980-11-13T11:58:56Z",
  "RECORD_TYPE":"VARIABLE_LENGTH",
  "IMAGE_ID":"\'0815S1+000\'",
  "FILTER_NAME":"CLEAR",
  "IMAGE_TIME":"1980-11-13T10:34:10Z",
  "IMAGE.SAMPLE_BITS":"8"
}
```
