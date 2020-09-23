# Test that translate properly handles each case.
context("Test translate().")

record_f_name151 <- system.file("sample",
                                "who151_odk_export.csv",
                                package = "vaItemResponse")
records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)

form_f_name151 <- system.file("forms",
                              "WHOVA2016_v1_5_1_XLS_form_for_ODK_survey.csv",
                              package = "vaItemResponse")

form151 <- read.csv2(form_f_name151, stringsAsFactors = FALSE)
death <- records151[1,]

test_that("single selected: selected(${Id10013}, 'yes')", {
    relevant <- form151$relevant[15]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$Id10013 == 'yes'")
})

test_that("double selected: (selected(${isNeonatal}, '1') or selected(${isChild}, '1'))", {
    relevant <- form151$relevant[462]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "(death$isNeonatal == '1' | death$isChild == '1')")
})

test_that("not selected: not(selected(${Id10114}, 'yes'))", {
    relevant <- form151$relevant[515]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$Id10114 != 'yes'")
})

test_that("condition: ${isChild1} = '1'", {
    relevant <- form151$relevant[37]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$isChild1 == '1'")
})

test_that("double condition: ${Id10020} != 'yes' or ${Id10022} != 'yes'", {
    relevant <- form151$relevant[39]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$Id10020 != 'yes' | death$Id10022 != 'yes'")
})

test_that("multiple: ((selected(${isChild}, '1') and ${ageInMonthsByYear} >=48 ) or selected(${isAdult}, '1')) and (selected(${Id10051}, 'yes'))", {
    relevant <- form151$relevant[69]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "((death$isChild == '1' & death$ageInMonthsByYear >= 48 ) | death$isAdult == '1') & (death$Id10051 == 'yes')")
})

test_that("multiple: selected(${isNeonatal}, '1') or (selected(${isChild}, '1') and (${ageInMonthsByYear} = 'NaN' or string-length(${ageInMonthsByYear}) = 0)) or (selected(${isChild}, '1') and ${ageInMonthsByYear} != 'NaN' and string-length(${ageInMonthsByYear}) > 0 and ${ageInMonthsByYear}<12)", {
    relevant <- form151$relevant[465]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$isNeonatal == '1' | (death$isChild == '1' & (is.na(death$ageInMonthsByYear) | nchar(death$ageInMonthsByYear) == 0)) | (death$isChild == '1' & !is.na(death$ageInMonthsByYear) & nchar(death$ageInMonthsByYear) > 0 & death$ageInMonthsByYear < 12)")
})

test_that("double selected", {
    relevant <- form151$relevant[486]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "death$Id10387 != 'yes' & death$Id10388 != 'yes'")
})

test_that("multiple: ((selected(${isAdult}, '1') and not(selected(${Id10310}, 'yes'))) and (${Id10019} = 'female') or ${Id10019}='undetermined')", {
    relevant <- form151$relevant[437]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "((death$isAdult == '1' & death$Id10310 != 'yes') & (death$Id10019 == 'female') | death$Id10019 == 'undetermined')")
})

test_that("count-selected: (selected(${Id10235}, 'DK') or selected(${Id10235}, 'Ref')) and count-selected(${Id10235})>1", {
    relevant <- form151$relevant[304]
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "(death$Id10235 == 'DK' | death$Id10235 == 'Ref') & stri_count_words(death$Id10235)>1")
})

test_that("misc: ((selected(${isChild}, '1')and selected(${Id10195}, 'yes')) or (selected(${isAdult}, '1') and selected(${Id10194}, 'yes')))", {
    relevant <- "((selected(${isChild}, '1')and selected(${Id10195}, 'yes')) or (selected(${isAdult}, '1') and selected(${Id10194}, 'yes')))"
    new_relevant <- translate(relevant)
    expect_equal(new_relevant, "((death$isChild == '1' & death$Id10195 == 'yes') | (death$isAdult == '1' & death$Id10194 == 'yes'))")
})

# Test itemHierarchy().
context("Test itemHierarchy().")

test_that("Test itemHierarchy() against 2016 WHO v151", {

    split_names <- strsplit(names(records151), "\\.")
    field_names <- unlist(lapply(split_names, function (x) x[length(x)]))
    comp_field_names <- names(records151)[field_names %in% form151$name]
    item_groups <- itemHierarchy(form151)$item_groups

    for (i in comp_field_names) {
        expect_true(i %in% item_groups, label = i)
    }

})

# Test itemMissing().
context("Test itemMissing().")

test_that("Test itemMissing() returns appropriate list.", {

    results <- itemMissing(records151, form151)
    expect_true(is.list(results))
    expect_true(names(results)[1] == "Deaths")
    expect_true(names(results)[2] == "Items")
    match_items_cols <- names(results$Items) %in%
        c("type", "name", "label..English", "relevant",
          "required", "n_asked", "n_ref", "n_dk", "n_miss", "n_yes", "n_no", "var", "entropy")
    expect_true(sum(match_items_cols) == 13)
    match_deaths_cols <- names(results$Deaths) %in%
        c("ID", "n_items", "n_ref", "n_dk", "n_miss", "n_yes", "n_no")
    expect_true(sum(match_deaths_cols) == 7)

})
