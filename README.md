# CrossVA  [![Travis-CI Build Status](https://travis-ci.org/verbal-autopsy-software/CrossVA.svg?branch=master)](https://travis-ci.org/verbal-autopsy-software/CrossVA) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/CrossVA)](https://cran.r-project.org/package=CrossVA)


## **Description** 	

CrossVA is an R package for transforming verbal autopsy (VA) data collected using the 2016 WHO VA instrument (versions 1.4.1 &
1.5.1) into a format designed for [openVA](http://openva.net) -- an R package for using algorithms to assign causes of
death to VA data.  The CrossVA package includes vignette that demonstrates how to use the package along with openVA (the
vignette is also posted [here](https://www.asc.ohio-state.edu/thomas.3912/software/CrossVA/using-crossva-and-openva.html) as
an html page).

This package contains older functions -- `map_records` -- capable of reading user-supplied mappings, and provides unvalidated mapping definitions to transform to InterVA4, Tariff 2, and InSilicoVA.  This functionality, however, is no longer actively supported since a new function -- `odk2openVA` -- has been added to support the latest versions of InterVA and InSilicoVA.

This package is made available by WHO, in collaboration with Swiss Tropical and Public Health Institute and the Bloomberg Data for Health Initiative.  Craig Hales from CDC reviewed and commented on earlier version of the mapping definitions.


## **Input**		
- `odk2openVA`: CSV file containing submissions of the 2016 WHO VA questionnaire (versions 1.4.1 & 1.5.1) exported 
from ODKAggregate (e.g. using ODK Briefcase).
- `map_records`
  + CSV file containing submissions of the 2016 WHO VA questionnaire (versions 1.4.1) exported from ODKAggregate (e.g. using
  ODK Briefcase).
  + A mapping files (tab-delimited text file). Minimal content: the first columns contains the names of all indicators needed
  by the coding algorithm (called "target indicators" here). The second column contains the mapping to each target indicators,
  as a valid R expression: expressions can be functions of zero or more variables of the WHO VA instrument, or any of the
  preceding target indicators. In addition to standard R functions, a small set of convenience functions which is provided in
  utils.R can be called to achieve the mapping. The release comes with three mapping files:
    1. CrossVA/inst/insilicova_mapping.txt
    1. CrossVA/inst/interva_mapping.txt
    1. CrossVA/inst/tariff2_mapping.txt

## **Output**
- `odk2openva`: a data frame intended to be an input to the InterVA5 and InSilicoVA (data.type = "WHO2016") algorthms
implemented by [openVA](http://openva.net)
- `map_records`: a data frame inteded to be an input to the InterVA4 and InSilicoVA (data.type = "WHO2012") algorithms 
implemented by [openVA](http://openva.net); as well as a CSV file intended for processing by a coding algorithm

## **Status**		
- `odk2openVA` is actively supported, with additional functionality for the PHMRC questionnaire under developement.
- `map_records` is still included, but not actively maintained (or debugged).

## **Installation**

For testing purposes, install via
```
install.packages("CrossVA")
```

Get the latest development version with the following commands

```
install.packages("devtools")
devtools::install_github("verbal-autopsy-software/CrossVA", subdir = "CrossVA")
```
## **Examples**
- `odk2openva` -- for use with InterVA5 & InSilicoVA (data.type = "WHO2016")

    ```
    vignette("using-crossva-and-openva")
    ```
    
    (also available [here](https://www.asc.ohio-state.edu/thomas.3912/software/CrossVA/using-crossva-and-openva.html) as html)

- `map_records` -- for use with InterVA4, InSilicoVA (data.type = "WHO2012"), & Tariff2

    ```
    library(CrossVA)
    record_f_name <- system.file('sample', 'who_va_output.csv', package = 'CrossVA')
    records <- read.csv(record_f_name)
    
    # map to interva4, use name of algorithm
    output_data <- map_records(records, 'interva4')
    output_f_name <- "output_for_interva4.csv"
    write.table(output_data, output_f_name, row.names = FALSE, na = "", qmethod = "escape", sep = ",")

    # map by providing a mapping file (here using the package-provided tariff2 mapping)
    mapping_file <- system.file('mapping', 'tariff2_mapping.txt', package = 'CrossVA')
    output_data <- map_records(records, mapping_file)
    output_f_name <- "output_for_smartva.csv"
    write.table(output_data, output_f_name, row.names = FALSE, na = "", qmethod = "escape", sep = ",")

    # convenience wrapper (here using the package-provided InsilicoVA mapping)
    output_data <- map_records_insilicova(records, "isoutput.csv")
    ```
