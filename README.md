# CrossVA  [![Build Status](https://www.travis-ci.com/verbal-autopsy-software/CrossVA.svg?branch=master)](https://www.travis-ci.com/verbal-autopsy-software/CrossVA) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/CrossVA)](https://cran.r-project.org/package=CrossVA)


## **Description** 	

CrossVA is an R package for transforming verbal autopsy (VA) data collected using the 2016 WHO VA instrument (versions 1.4.1 &
1.5.1) or the 2014 WHO VA instrument into a format designed for [openVA](http://openva.net) -- an R package for using algorithms to assign causes of
death to VA data.  The CrossVA package includes vignette that demonstrates how to use the package along with openVA (the
vignette is also posted on [openva.net](http://openva.net/vignettes/using-crossva-and-openva.html) as an html page).

The most recent version of this package (0.9.6) has removed the functions -- `map_records*` -- capable of reading user-supplied mappings, and provides unvalidated mapping definitions to transform to InterVA4, Tariff 2, and InSilicoVA.  This functionality, however, is no longer actively supported since a new function -- `odk2openVA` -- has been added to support the latest versions of InterVA and InSilicoVA.

This package is made available by WHO, in collaboration with Swiss Tropical and Public Health Institute and the Bloomberg Data for Health Initiative.  Craig Hales from CDC reviewed and commented on earlier version of the mapping definitions.


## **Input**		
- `odk2openVA`: CSV file containing submissions of the 2016 WHO VA questionnaire (versions 1.4.1 & 1.5.1) or the 2014 WHO VA questionnaire exported 
from ODKAggregate (e.g. using ODK Briefcase).

## **Output**
- `odk2openva`: a data frame intended to be an input to the InterVA5 and InSilicoVA (data.type = "WHO2016") algorthms
implemented by [openVA](http://openva.net)

## **Status**		
- `odk2openVA` is actively supported, with additional functionality for the PHMRC questionnaire under developement.
- `map_records` is no longer included.

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
    
    (also available at [openva.net](http://openva.net/vignettes/using-crossva-and-openva.html) as html page)
