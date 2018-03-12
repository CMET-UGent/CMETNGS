[![Build Status](https://travis-ci.org/CMET-UGent/CMETNGS.svg?branch=master)](https://travis-ci.org/CMET-UGent/CMETNGS)

# CMETNGS_package
R package with some convenience functions for CMETs NGS pipeline

## Installation

```
library(devtools)
install_github("https://github.ugent.be/LabMETNGS/CMETNGS_package")
```

## Available functions

For more details on a function just call `?functionname`

Function name | Description
--------------|-------------
highqualgraphR | export publication-grade graphics from ggplot2 object
makebargraphrawggplot2 | make stacked bar graphs from preformatted taxonomy
preformattax | preformat mothur taxonomies
coldiss | fast plots of dissimilarity matrices
cbindPad | function to combine unequal-length data frames (or vectors) into a dataframe
gg_color_hue | function to generate ggplot-like color pallettes
myround | alternative plotting function
fasta2dataframe | bioconductor-based readout of (mothur formatted) sequences into a dataframe
