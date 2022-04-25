# Column position lookup --------------------------------------------------

test_that("Lookup column positions", {
    result <- lookup_column_positions(
        data = mtcars,
        columns = "cyl"
    )
    expectation <- 3L
    expect_identical(result, expectation)

    result <- lookup_column_positions(
        data = mtcars,
        columns = c("cyl", "hp")
    )
    expectation <- c(3L, 5L)
    expect_identical(result, expectation)

    result <- lookup_column_positions(
        data = mtcars,
        columns = "cyl",
        no_offset = TRUE
    )
    expectation <- 2L
    expect_identical(result, expectation)

    result <- lookup_column_positions(
        data = mtcars,
        columns = "carb",
        reverse = TRUE
    )
    expectation <- 1L
    expect_identical(result, expectation)

    result <- lookup_column_positions(
        data = mtcars,
        columns = "gear",
        reverse = TRUE
    )
    expectation <- 2L
    expect_identical(result, expectation)
})
