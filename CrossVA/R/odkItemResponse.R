#' Translate ODK relevant field
#'
#' \code{translate} converts conditions in ODK to R.
#'
#' @param relevant A string from ODK$relevant that needs to be translated.
#'
#' @param death A dataframe (one death) of ODK data.
#'
#' @details
#' This is a worker function.
#' 
#' @examples
#' \dontrun{
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' odkData <- read.csv("who151_odk_export.csv", stringsAsFactors = FALSE)
#' names(odkData)
#' odkForm <- read_xlsx("08_1_WHOVA2016_v1_5_1_XLS_form_for_ODK.xlsx", sheet = "survey")
#' }
#'
#' @importFrom stringi stri_replace_all_regex
#' @export
#'

## ## set up inputs
## death <- odkData[1,]
## names(death)

## splitNames <- strsplit(names(death), "\\.")
## fieldNames <- unlist(lapply(splitNames, function (x) x[length(x)]))
## names(death) <- fieldNames

## relevant <- odkForm$relevant[15] ## "selected(${Id10013}, 'yes')"
## relevant <- odkForm$relevant[462] ## "(selected(${isNeonatal}, '1') or selected(${isChild}, '1'))"
##                                   ## check if numbers are character or numeric in actual data
##                                   ## however, note: 1 == "1"
## relevant <- odkForm$relevant[515] ## "not(selected(${Id10114}, 'yes'))"
## relevant <- odkForm$relevant[37] ## "${isChild1} = '1'"
## relevant <- odkForm$relevant[39] ##  "${Id10020} != 'yes' or ${Id10022} != 'yes'"
## relevant <- odkForm$relevant[69] ##  ${ageInMonthsByYear} >=48 ) with other stuff
## relevant <- odkForm$relevant[465] ## string-length(${ageInMonthsByYear}) = 0))
## relevant <- odkForm$relevant[486] ## multiple not selected's
## relevant <- odkForm$relevant[437] ## selected and not selected and condition

## I think it is easiest if we just translate

translate <- function (relevant, death) {

    # parse field names
    splitNames <- strsplit(names(death), "\\.")
    fieldNames <- unlist(lapply(splitNames, function (x) x[length(x)]))
    names(death) <- fieldNames

    # translate selected() -- maybe have separate functions for these
    patternSelected <- "(?<!not\\()selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)"
    newRelevant <- stri_replace_all_regex(relevant, patternSelected, "death\\$$1 == $2")

    ## # translate not(selected())
    ## patternNotSelected <- "not\\(selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)\\)"
    ## newRelevant <- stri_replace_all_regex(newRelevant, patternNotSelected, "!(death\\$$1 == $2)")

    ## # translate ${field_name} (separately for !=, =, >, and <
    ## # \\2 = {, \\3 = field name, group 4 }, group 5 =, group 6 target 
    ## patternFieldEq <- "(?<!selected\\()\\$\\{([^\\}]+)(\\})([[:space:]|=]+[:space:]*)"
    ## newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "death\\$$1 == ")

    ## # translate or replace " or " with " | "
    ## newRelevant <- stri_replace_all_regex(newRelevant, " or ", " | ")
    ## newRelevant <- stri_replace_all_regex(newRelevant, "\\)or\\(", ") | (")
    
    ## # translate and odkForm$relevant[437]
    ## newRelevant <- stri_replace_all_regex(newRelevant, "[[:space:]+and[:space:]+", " & ")
    ## newRelevant <- stri_replace_all_regex(newRelevant, "\\)and\\(", " & ")
    ## newRelevant <- stri_replace_all_regex(newRelevant, "\\)and\\(", " & ")


    # translate 'NaN'

    # newRelevant
    # eval(parse(text = paste0("death$", newRelevant)))
    # with(data, eval(parse(text = newRelevant)))

    return(newRelevant)
}
