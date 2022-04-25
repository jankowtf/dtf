test_that("valid_dt_options_buttons_names", {
    result <- valid_dt_options_buttons_names()
    expected <- c("colvis", "copy", "csv", "excel", "pdf", "print")
    expect_identical(result, expected)

    result <- valid_dt_options_buttons_names(c("copy", "print"))
    expected <- c("copy","print")
    expect_identical(result, expected)

    expect_error(valid_dt_options_buttons_names("invalid"))
})
