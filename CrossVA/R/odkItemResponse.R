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
#' record_f_name151 <- system.file("sample",
#'                                 "who151_odk_export.csv",
#'                                 package = "CrossVA")
#' records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
#' 
#' form_f_name151 <- system.file("forms",
#'                               "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
#'                               package = "CrossVA")
#' form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
#' death <- records151[1,]
#' 
#' relevant <- form151$relevant[15]
#' translate(relevant, death)
#' }
#'
#' @importFrom stringi stri_replace_all_regex stri_count_words
#' @export
#'
translate <- function (relevant, death) {

    # parse field names
    splitNames <- strsplit(names(death), "\\.")
    fieldNames <- unlist(lapply(splitNames, function (x) x[length(x)]))
    names(death) <- fieldNames

    # remove \n
    patternSelected <- "\n"
    newRelevant <- stri_replace_all_regex(relevant, patternSelected, " ")

    # replace = with == (but not for >= or <= or !=)
    patternSelected <- "(?<![>|<|!])="
    newRelevant <- stri_replace_all_regex(newRelevant, patternSelected, "==")

    # translate selected() -- maybe have separate functions for these
    patternSelected <- "(?<!not\\()selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternSelected, "death\\$$1 == $2")

    # translate not(selected())
    patternNotSelected <- "not\\(selected\\(\\$\\{([^\\}]+)\\}[^']+('[^']+')\\)\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternNotSelected, "death\\$$1 != $2")

    # translate ${field_name} (separately for !=, =, >, and <)
    # \\1 = field name, \\2 = }, \\3 = relational operator/comparators
    patternFieldEq <- "(?<!selected\\()\\$\\{([^\\}]+)(\\})[:space:]*([=|!|>|<]=*)[:space:]*"
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
    ## is.na() == TRUE
    patternFieldEq <- "death\\$([^[:space:][:punct:]]+)[:space:]*==[:space:]*'NaN'"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "is.na(death\\$$1)")
    ## is.na() == FALSE
    patternFieldEq <- "death\\$([^[:space:][:punct:]]+)[:space:]*!=[:space:]*'NaN'"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "!is.na(death\\$$1)")

    # translate string-length(${ageInMonthsByYear}) = 0)) with nchar
    patternFieldEq <- "string-length\\(\\$\\{([^\\}]+)\\}\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "nchar(death\\$$1)")

    # translate count-selected 
    patternFieldEq <- "count-selected\\(\\$\\{([^\\}]+)\\}\\)"
    newRelevant <- stri_replace_all_regex(newRelevant, patternFieldEq, "stri_count_words(death\\$$1)")

    # newRelevant
    # eval(parse(text = paste0("death$", newRelevant)))
    # with(data, eval(parse(text = newRelevant)))

    ## devtools::test_file('../tests/testthat/test-item-response.R')
    return(newRelevant)
}


#' Construct item names that capture the hierarchical of an ODK questionnare.
#'
#' \code{itemHierarchy} map the ODK questionnaire hierarchy 
#'
#' @param odkForm A data frame containing an ODK form.
#'
#' @details
#' Returns the the same data frame, but with an additional column
#' consisting of the group names each item belongs to.  The group names
#' are separated by ".".
#' 
#' @examples
#' \dontrun{
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' 
#' form_f_name151 <- system.file("forms",
#'                               "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
#'                               package = "CrossVA")
#' form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
#' itemGroups <- itemHierarchy(form151)$itemGroups
#' }
#'
#' @importFrom stringi stri_replace_all_regex stri_count_words
#' @export
#'
itemHierarchy <- function (odkForm) {

    skipRow <- odkForm$type == ""
    newForm <- odkForm
    newForm$itemGroups <- ""
    current_name <- ""
    n_groups <- 0

    nNames <- function (itemNames) {
        # return the number of groups in groupNames
        splitNames <- strsplit(itemNames, "\\.")
        allNames <- unlist(splitNames)
        return (length(allNames))
    }

    removeLastName <- function (itemNames) {
        # remove last group name; groups separated by a period
        splitNames <- strsplit(itemNames, "\\.")
        allNames <- unlist(splitNames)
        n <- length(allNames)
        namesToKeep <- unlist(splitNames)[-n]
        return (paste0(namesToKeep, collapse = "."))
        
    }
    
    for (i in 1:length(skipRow)) {

        if (skipRow[i]) next

        if (newForm$type[i] == "begin group" & nNames(current_name) == 0) {
            current_name <- newForm$name[i]
            n_groups <- n_groups + 1
            newForm$itemGroups[i] <- current_name
        } else if (newForm$type[i] == "begin group" & nNames(current_name) > 0) {
            current_name <- paste0(c(current_name, newForm$name[i]), collapse = ".")
            n_groups <- n_groups + 1
            newForm$itemGroups[i] <- current_name
        } else if (newForm$type[i] == "end group") {
            current_name <- removeLastName(current_name)
            n_groups <- n_groups - 1
            next
        } else {
            current_name <- paste0(c(current_name, newForm$name[i]),
                                   collapse = ".")
            newForm$itemGroups[i] <- current_name
            current_name <- removeLastName(current_name)
        }
    }

    ## devtools::test_file('../tests/testthat/test-item-response.R')
    return (newForm)
}

## library(stringi)
## record_f_name151 <- system.file("sample",
##                                 "who151_odk_export.csv",
##                                 package = "CrossVA")
## records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)

## form_f_name151 <- system.file("forms",
##                               "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
##                               package = "CrossVA")
## form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
## death <- records151[1,]

## relevant <- form151$relevant[15]
## translate(relevant, death)
## }

## splitNames <- strsplit(names(death), "\\.")
## fieldNames <- unlist(lapply(splitNames, function (x) x[length(x)]))
## names(death) <- fieldNames

## i <- 2
## names(death)[i]
## form151$name[which(form151$name == names(death)[i])]

## if (!(names(death)[i] %in% form151$name)) next
## form_index <- which(form151$name == names(death)[i])

## n_questions <- 0
## n_NA <- 0
## n_blank <- 0
## n_dk <- 0
## n_refused <- 0
    
## i_required <- form151$required[form_index]
## i_relevant <- form151$relevant[form_index]
