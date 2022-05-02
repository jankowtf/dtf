# Buttons -----------------------------------------------------------------

test_that("valid_dt_options_buttons_names", {
    result <- valid_dt_options_buttons_names()
    expected <- c("colvis", "copy", "csv", "excel", "pdf", "print")
    expect_identical(result, expected)

    result <- valid_dt_options_buttons_names("copy", "print")
    expected <- c("copy","print")
    expect_identical(result, expected)

    result <- valid_dt_options_buttons_names(c("copy", "print"))
    expected <- c("copy","print")
    expect_identical(result, expected)

    expect_error(valid_dt_options_buttons_names("invalid"))
})

# Select buttons ----------------------------------------------------------

test_that("valid_dt_options_buttons_select_names", {
    result <- valid_dt_options_buttons_select_names()
    expected <- c("selectAll", "selectNone", "selectRows",
        "selectColumns", "selectCells")
    expect_identical(result, expected)

    result <- valid_dt_options_buttons_select_names("selectColumns", "selectCells")
    expected <- c("selectColumns", "selectCells")
    expect_identical(result, expected)

    result <- valid_dt_options_buttons_select_names(c("selectColumns", "selectCells"))
    expected <- c("selectColumns", "selectCells")
    expect_identical(result, expected)

    expect_error(valid_dt_options_buttons_select_names("invalid"))
})

