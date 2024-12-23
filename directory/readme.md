# About the Beyond Notability Directory code

The directory has been created using RMarkdown which combines text and R code into a readable document. 
This folder contains the files needed to do this, as well as the most recent versions of the code output.

See: https://rmarkdown.rstudio.com/

There are two Rmarkdown files, for different formats:

- directory-pdf.Rmd to make a nicely formatted PDF for reading.
- directory-md.Rmd to make a Markdown file (for further transformations using a tool such as Pandoc).


## Requirements

R: https://cran.r-project.org/

Tidyverse: https://www.tidyverse.org/

The following R packages need to be installed (some will be installed automatically with tidyverse). 
All but one should be available via CRAN.

- tidyverse
- glue
- rmarkdown				
- knitr				
- here				
- janitor				
- SPARQLchunks [Github only]		
- stringi				
- tidytext		


Additionally, the PDF version requires LaTex to be installed, and LaTex is monstrous. 
The R {tinytex} package is recommended, and dependencies have been kept as minimal as possible.
But I can't guarantee a pain-free experience. See https://bookdown.org/yihui/rmarkdown/installation.html


Note that all code was written and run in RStudio Desktop (https://posit.co/download/rstudio-desktop/), 
although this is not an essential requirement.

