# GED2DENDRO

## Purpose

To transform a GED text tree to a dendrograph XML for import into DrawIO.

GED text trees are genealogical family trees from services like Ancestry.com and GEDMatch. Artists make dendrographs from those to make them better presentable and to anonimize them for public release. They use services like DrawIO (app.diagram.net) to create dendrographs manually.

This project aims to automate the start of that process, to reduce the amount of time spent by the artists.

Separate coloring and lettering is used to indicate a difference between a target person, other interesting people, all other people, and families. 

Hint: have app.diagrams.net auto-generate a layout. From our testing, both horizontal flow and vertical flow yield the prettiest results.
  
Next topic to tackle:
- endogamy loops.


## Testing

A test suite exists in the module named Testing. Fire up ghci and invoke:
Testing.runTests

That will run all tests and display which is successful and which failed, and a percentage summary of both groups.


## Running the program

Right now, running the program requires the following steps:
- download the executable for your system;
- download the trafo.xsl;
- determine the filters you want to apply to the tree;
- run the tree through the program;
- run its output through the trafo.xsl.

We are working on reducing the amount of steps. Read on for detailed instructions.

Given an input file named input.ged, run the ged2dendro application like so using a terminal on **Linux / Unix** based machines:
```shell
    > cat input.ged | ./ged2dendro-1.1.0.2 @P1@ dnamatch 5 > raw.xml
```

or like so using a command prompt on **Windows** based machines:
```cmd
    > type input.ged | .\ged2dendro-1.1.0.2.exe @P5@ cM 7 > raw.xml
```

The program accepts 3 optional arguments:
- The identifier of the last person or family to output; 
  if omitted, the program will output everyone.
- A search needle that has the program include additional 
  nodes that may or may not be in the direct ancestry of
  the person or family identified. For instance, any node 
  titled "dnamatch", or whose name contains "cM". This is 
  case-sensitive: casing matters. If omitted or set to empty, 
  the program will ignore it. Set to "" if it needs to be 
  empty but you need to specify the 3rd parameter.
- The maximum amount of generations of ancestors to the person or 
  family identified in the 1st argument;
  if omitted, the program will output 2 generations (parents and grandparents).
  Note: The lineage trees produced by Ancestry.com contain separate
  nodes for families and the spouses in each family. Both the 
  family and its spouses count as a generation. So to have the 
  program return parents and grandparents (which you would think
  counts as 2 generations) you should ask for 4 generations.
  Note: if the oldest family does not show names of its spouses,
  ask for 1 generation more.
  

Then on **MS Windows**, convert the raw xml output to a dendrograph xml using **nxslt2**:
```cmd
    > .\nxslt2 .\raw.xml .\trafo.xsl -o .\dendro.drawio
```

Or, if you prefer a dendrograph with birth and death dates, use the trafo-birthdates.xsl.

You can find a copy of nxslt2 in the bin folder where you found the source of this program.

*Sometimes nxslt2 will fail. In which case you could use the free Notepad++ application, with its XML Tools plugin. It can perform XSL Transformations. Or you can use an online web service like:*
http://xslttest.appspot.com/
*or*
http://www.xpathtester.com/xslt

Alternatively on **Linux**, convert it using **xsltproc** like so:
```shell
    > xsltproc -o ./dendro.drawio ./trafo.xsl ./raw.xml
```

Or, if you prefer a dendrograph with birth and death dates, use the trafo-birthdates.xsl.

