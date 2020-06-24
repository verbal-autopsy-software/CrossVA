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
translate <- function (relevant, death) {

    # parse field names
    splitNames <- strsplit(names(death), "\\.")
    fieldNames <- unlist(lapply(splitNames, function (x) x[length(x)]))
    names(death) <- fieldNames

    # replace = with == (but not for >= or <= or !=)
    patternSelected <- "(?<![>|<|!])="
    newRelevant <- stri_replace_all_regex(relevant, patternSelected, "==")

    # translate selected() -- maybe have separate functions for these
    patternSelected <- "(?<!not\\()selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternSelected, "death\\$$1 == $2")

    # translate not(selected())
    patternNotSelected <- "not\\(selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternNotSelected, "death\\$$1 != $2")

    # translate ${field_name} (separately for !=, =, >, and <)
    # \\1 = field name, \\2 = }, \\3 = relational operator/comparators
    patternFieldEq <- "(?<!selected\\()\\$\\{([^\\}]+)(\\})[:space:]*(==|!=|>[^=]|>=|<[^=]|<=)[:space:]*"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "death\\$$1 $3 ")
 
    # translate or replace " or " with " | "
    newRelevant <- stri_replace_all_regex(newRelevant, " or ", " | ")
    newRelevant <- stri_replace_all_regex(newRelevant, "\\)or", ") | (")
    newRelevant <- stri_replace_all_regex(newRelevant, "or\\(", ") | (")
    
    # translate and odkForm$relevant[437]
    newRelevant <- stri_replace_all_regex(newRelevant, "[:space:]+and[:space:]+", " & ")
    newRelevant <- stri_replace_all_regex(newRelevant, "\\)and", " & ")
    newRelevant <- stri_replace_all_regex(newRelevant, "and\\(", " & ")

    # translate 'NaN' (note previous conversions with = and with field name)
    #${ageInMonthsByYear} = 'NaN'
    # \\1 = field name
    patternFieldEq <- "death\\$([^[:space:][:punct:]}]+)[:space:]*==[:space:]*'NaN'"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "is.na(death\\$$1)")
    patternFieldEq <- "death\\$([^[:space:][:punct:]}]+)[:space:]*!=[:space:]*'NaN'"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "!is.na(death\\$$1)")

    # translate string-length(${ageInMonthsByYear}) = 0)) with nchar

    # newRelevant
    # eval(parse(text = paste0("death$", newRelevant)))
    # with(data, eval(parse(text = newRelevant)))

    ## devtools::test_file('../tests/testthat/test-item-response.R')
    return(newRelevant)
}
