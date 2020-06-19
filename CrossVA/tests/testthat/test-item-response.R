# Test that translate properly handles each case.
# testthat::test_file('../tests/testthat/test-item-response.R')

context("Test translate().")

test_that("translate properly handles every type of condition in relevant field.", {

    record_f_name151 <- system.file("sample",
                                    "who151_odk_export.csv",
                                    package = "CrossVA")
    records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)

    form_f_name151 <- system.file("forms",
                                  "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
                                  package = "CrossVA")

    form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
    death <- records151[1,]

    ## "selected(${Id10013}, 'yes')"
    relevant <- form151$relevant[15]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$Id10013 == 'yes'")
})

