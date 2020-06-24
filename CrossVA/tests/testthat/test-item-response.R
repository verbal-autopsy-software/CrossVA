# Test that translate properly handles each case.
# testthat::test_file('../tests/testthat/test-item-response.R')

context("Test translate().")

record_f_name151 <- system.file("sample",
                                "who151_odk_export.csv",
                                package = "CrossVA")
records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)

form_f_name151 <- system.file("forms",
                              "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
                              package = "CrossVA")

form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
death <- records151[1,]

test_that("single selected: selected(${Id10013}, 'yes')", {
    relevant <- form151$relevant[15]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$Id10013 == 'yes'")
})

test_that("double selected: (selected(${isNeonatal}, '1') or selected(${isChild}, '1'))", {
    relevant <- form151$relevant[462]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "(death$isNeonatal == '1' | death$isChild == '1')")
})

test_that("not selected: not(selected(${Id10114}, 'yes'))", {
    relevant <- form151$relevant[515]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$Id10114 != 'yes'")
})

test_that("condition: ${isChild1} = '1'", {
    relevant <- form151$relevant[37]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$isChild1 == '1'")
})

test_that("double condition: ${Id10020} != 'yes' or ${Id10022} != 'yes'", {
    relevant <- form151$relevant[39]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$Id10020 != 'yes' | death$Id10022 != 'yes'")
})

test_that("multiple: ((selected(${isChild}, '1') and ${ageInMonthsByYear} >=48 ) or selected(${isAdult}, '1')) and (selected(${Id10051}, 'yes'))", {
    relevant <- form151$relevant[69]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "((death$isChild == '1' & death$ageInMonthsByYear >= 48 ) | death$isAdult == '1') & (death$Id10051 == 'yes')")
})

test_that("multiple: selected(${isNeonatal}, '1') or (selected(${isChild}, '1') and (${ageInMonthsByYear} = 'NaN' or string-length(${ageInMonthsByYear}) = 0)) or (selected(${isChild}, '1') and ${ageInMonthsByYear} != 'NaN' and string-length(${ageInMonthsByYear}) > 0 and ${ageInMonthsByYear}<12)", {
    relevant <- form151$relevant[465]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$isNeonatal == '1' | (death$isChild == '1' & (is.na(death$ageInMonthsByYear) | nchar(death$ageInMonthsByYear) == 0)) | (death$isChild == '1' & !is.na(death$ageInMonthsByYear) & nchar(death$ageInMonthsByYear) > 0 & death$ageInMonthsByYear < 12)")
})

test_that("double selected", {
    relevant <- form151$relevant[486]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "death$Id10387 != 'yes' & death$Id10388 != 'yes'")
})

test_that("multiple: ((selected(${isAdult}, '1') and not(selected(${Id10310}, 'yes'))) and (${Id10019} = 'female') or ${Id10019}='undetermined')", {
    relevant <- form151$relevant[437]
    newRelevant <- translate(relevant, death)
    expect_equal(newRelevant, "((death$isAdult == '1' & death$Id10310 != 'yes') & (death$Id10019 == 'female') | death$Id10019 == 'undetermined')")
})
