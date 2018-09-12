# CrossVA

## **Description** 	

CrossVA is an R package for transforming verbal autopsy records collected using the WHO VA 2016 instrument (Revision 1.4.1) to be
used as input for different coding algorithms. Currently supports user-supplied mappings, and provides unvalidated mapping
definitions to transform to InterVA4, InterVA5, Tariff 2, and InSilicoVA. This package is made available by WHO, in collaboration
with Swiss Tropical and Public Health Institute and the Bloomberg Data for Health Initiative.  Craig Hales from CDC reviewed and
commented on earlier version of the mapping definitions.


## **Input**		
- CSV file containing submissions of the 2016 WHO VA questionnaire (Revision 1.4.1, exported from ODKAggregate using ODKBriefcase)
- A mapping files (tab-delimited text file). Minimal content: the first columns contains the names of all indicators needed by the
  coding algorithm (called "target indicators" here). The second column contains the mapping to each target indicators, as a valid
  R expression: expressions can be functions of zero or more variables of the WHO VA instrument, or any of the preceding target
  indicators. In addition to standard R functions, a small set of convenience functions which is provided in utils.R can be called
  to achieve the mapping. The release comes with currently two mapping files, one for interVA4, and one for tariff2).

## **Output**		
A CSV file intended for processing by a coding algorithm.

## **Status**		
Under development.

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
```
library(CrossVA)
record_f_name <- system.file('sample', 'who_va_output.csv', package = 'CrossVA')
records <- read.csv(record_f_name)
#map to interva4, use name of algorithm
output_data <- map_records(records, 'interva4')
output_f_name <- "output_for_interva4.csv"
write.table(output_data, output_f_name, row.names = FALSE, na = "", qmethod = "escape", sep = ",")

#map by providing a mapping file (here using the package-provided tariff2 mapping)
mapping_file <- system.file('mapping', 'tariff2_mapping.txt', package = 'CrossVA')
output_data <- map_records(records, mapping_file)
output_f_name <- "output_for_smartva.csv"
write.table(output_data, output_f_name, row.names = FALSE, na = "", qmethod = "escape", sep = ",")

#convenience wrapper (here using the package-provided InsilicoVA mapping)
output_data <- map_records_insilicova(records, "isoutput.csv")
```
