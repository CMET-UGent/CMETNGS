[![Build Status](https://travis-ci.org/CMET-UGent/CMETNGS.svg?branch=master)](https://travis-ci.org/CMET-UGent/CMETNGS)

# CMETNGS_package
R package with some convenience functions for CMETs NGS pipeline

## Installation

```
library(devtools)
install_github("CMET-UGent/CMETNGS")
```

## Available functions

For more details on a function call `?functionname`

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
KimPalettes | Function to select a discrete color palette as created by Kim De Paepe
MakeEDABargraphs | create some exploratory stacked bargraphs based upon mothur output files and phyloseq, as created by Tim Lacoere
df2fasta | bioconductor-based inverse of `fasta2dataframe` (useful in case you did some filtering via fasta2dataframe but need a fasta output for downstream)
MakeExcelReport | R function that takes absolute (!) paths to a shared, taxonomy and optional otureps fasta and automatically creates an excel file from it.
myround | more robust rounding function
construct_phyloseq | create a phyloseq object from mothur output, according to CMET SOP (different from `phyloseq::import_mothur` in that it works with the final files from our current SOP, with support for metadata)
Analyze_Sangers | wrapper function for sangeranalyzeR package to deal with LGC/Tim Lacoere formatted data for 16S Sangers
