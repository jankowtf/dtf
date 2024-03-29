# Extensions --------------------------------------------------------------

test_that("Extensions", {
    result <- dt_extensions()
    expectation <- c(
        AutoFill = "AutoFill",
        Buttons = "Buttons",
        ColReorder = "ColReorder",
        Editor = "Editor",
        FixedColumns = "FixedColumns",
        FixedHeader = "FixedHeader",
        KeyTable = "KeyTable",
        Responsive = "Responsive",
        RowGroup = "RowGroup",
        RowReorder = "RowReorder",
        Scroller = "Scroller",
        SearchBuilder = "SearchBuilder",
        Select = "Select",
        SearchPanes = "SearchPanes",
        ColVis = "ColVis"
    ) %>% unname()
    expect_identical(result, expectation)

    # result <- dt_extensions(.unname = FALSE)
    # result <- dt_extensions(reverse = TRUE, .unname = FALSE)

    result <- dt_extensions("Buttons")
    # dt_extensions("Buttons", .unname = FALSE)
    expectation <- c(Buttons = "Buttons") %>% unname()
    expect_identical(result, expectation)

    result <- dt_extensions("Buttons", "Scroller")
    # dt_extensions("Buttons", "Scroller", .reverse = TRUE)
    # dt_extensions("Buttons", "Scroller", .reverse = TRUE, .unname = TRUE)
    expectation <- c(Buttons = "Buttons", Scroller = "Scroller") %>% unname()
    expect_identical(result, expectation)

    expect_error(dt_extensions("Invalid"),
        regexp = "Invalid choice: valid_dt_extensions\\(\"Invalid\"\\)")
})

# Options -----------------------------------------------------------------

test_that("Options", {
    result <- dt_options("deferLoading")
    expectation <- c(deferLoading = "deferLoading") %>% unname()
    expect_identical(result, expectation)

    result <- dt_options("autoFill", extension = "AutoFill")
    expectation <- c(autoFill = "autoFill") %>% unname()
    expect_identical(result, expectation)

    expect_error(dt_options("_invalid_"),
        regexp = 'Invalid choice: valid_dt_options\\("_invalid_"\\)')
})

# Process bundles ---------------------------------------------------------

test_that("Process bundle: character", {
    result <- dt_process_bundle(
        bundle = "AutoFill"
    )
    expectation <- list(extensions = "AutoFill",
        options = list(autoFill = TRUE))
    expect_identical(result, expectation)
})

test_that("Process bundle: list", {
    result <- dt_bundle_autofill() %>% dt_process_bundles()
    expectation <- list(extensions = "AutoFill", options = list(autoFill = TRUE))
    expect_identical(result, expectation)
})

test_that("Process bundle: list of lists", {
    result <- list(
        dt_bundle_autofill(),
        dt_bundle_buttons()
    ) %>% dt_process_bundles()
    expectation <- list(extensions = c("AutoFill", "Buttons"), options = list(autoFill = TRUE,
        dom = "Blftipr", buttons = c("colvis", "copy", "csv", "excel",
            "pdf", "print")))
    expect_identical(result, expectation)
})

test_that("Prepare bundles", {
    result <- dt_process_bundles(
        bundle = c("AutoFill", "Buttons")
    )
    expectation <- list(
        extensions = c("AutoFill", "Buttons"),
        options = list(
            autoFill = TRUE,
            dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
            buttons = c("colvis", "copy", "csv", "excel", "pdf", "print")
        )
    )
    expect_identical(result, expectation)
})

# autofill --------------------------------------------------------

test_that("autofill", {
    result <- dt_bundle_autofill()
    expectation <- list(extensions = "AutoFill",
        options = list(autoFill = TRUE))
    expect_identical(result, expectation)

    result <- datatable2(
        data = mtcars,
        bundle = "AutoFill",
        selection = "none"
    )
    expect_identical(result %>% class(), c("datatables", "htmlwidget"))
    # expect_s3_class("datatables", "htmlwidgets")
    # expectation <- structure(list(x = structure(list(filter = "none", vertical = FALSE,
    #     extensions = list(AutoFill = "AutoFill"), data = structure(list(
    #         " " = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive",
    #             "Hornet Sportabout", "Valiant", "Duster 360", "Merc 240D",
    #             "Merc 230", "Merc 280", "Merc 280C", "Merc 450SE", "Merc 450SL",
    #             "Merc 450SLC", "Cadillac Fleetwood", "Lincoln Continental",
    #             "Chrysler Imperial", "Fiat 128", "Honda Civic", "Toyota Corolla",
    #             "Toyota Corona", "Dodge Challenger", "AMC Javelin", "Camaro Z28",
    #             "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2", "Lotus Europa",
    #             "Ford Pantera L", "Ferrari Dino", "Maserati Bora", "Volvo 142E"
    #         ), mpg = c(21, 21, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4,
    #             22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4, 14.7,
    #             32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3,
    #             26, 30.4, 15.8, 19.7, 15, 21.4), cyl = c(6, 6, 4, 6,
    #                 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,
    #                 8, 8, 8, 4, 4, 4, 8, 6, 8, 4), disp = c(160, 160, 108,
    #                     258, 360, 225, 360, 146.7, 140.8, 167.6, 167.6, 275.8,
    #                     275.8, 275.8, 472, 460, 440, 78.7, 75.7, 71.1, 120.1,
    #                     318, 304, 350, 400, 79, 120.3, 95.1, 351, 145, 301, 121
    #                 ), hp = c(110, 110, 93, 110, 175, 105, 245, 62, 95, 123,
    #                     123, 180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150,
    #                     150, 245, 175, 66, 91, 113, 264, 175, 335, 109), drat = c(3.9,
    #                         3.9, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92,
    #                         3.92, 3.07, 3.07, 3.07, 2.93, 3, 3.23, 4.08, 4.93, 4.22,
    #                         3.7, 2.76, 3.15, 3.73, 3.08, 4.08, 4.43, 3.77, 4.22,
    #                         3.62, 3.54, 4.11), wt = c(2.62, 2.875, 2.32, 3.215, 3.44,
    #                             3.46, 3.57, 3.19, 3.15, 3.44, 3.44, 4.07, 3.73, 3.78,
    #                             5.25, 5.424, 5.345, 2.2, 1.615, 1.835, 2.465, 3.52, 3.435,
    #                             3.84, 3.845, 1.935, 2.14, 1.513, 3.17, 2.77, 3.57, 2.78
    #                         ), qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22,
    #                             15.84, 20, 22.9, 18.3, 18.9, 17.4, 17.6, 18, 17.98, 17.82,
    #                             17.42, 19.47, 18.52, 19.9, 20.01, 16.87, 17.3, 15.41,
    #                             17.05, 18.9, 16.7, 16.9, 14.5, 15.5, 14.6, 18.6), vs = c(0,
    #                                 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,
    #                                 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1), am = c(1, 1,
    #                                     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
    #                                     0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1), gear = c(4, 4, 4,
    #                                         3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3,
    #                                         3, 3, 3, 3, 4, 5, 5, 5, 5, 5, 4), carb = c(4, 4, 1, 1,
    #                                             2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,
    #                                             2, 4, 2, 1, 2, 2, 4, 6, 8, 2)), class = "data.frame", row.names = c("Mazda RX4",
    #                                                 "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive", "Hornet Sportabout",
    #                                                 "Valiant", "Duster 360", "Merc 240D", "Merc 230", "Merc 280",
    #                                                 "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC", "Cadillac Fleetwood",
    #                                                 "Lincoln Continental", "Chrysler Imperial", "Fiat 128", "Honda Civic",
    #                                                 "Toyota Corolla", "Toyota Corona", "Dodge Challenger", "AMC Javelin",
    #                                                 "Camaro Z28", "Pontiac Firebird", "Fiat X1-9", "Porsche 914-2",
    #                                                 "Lotus Europa", "Ford Pantera L", "Ferrari Dino", "Maserati Bora",
    #                                                 "Volvo 142E")), container = "<table class=\"display\">\n  <thead>\n    <tr>\n      <th> </th>\n      <th>mpg</th>\n      <th>cyl</th>\n      <th>disp</th>\n      <th>hp</th>\n      <th>drat</th>\n      <th>wt</th>\n      <th>qsec</th>\n      <th>vs</th>\n      <th>am</th>\n      <th>gear</th>\n      <th>carb</th>\n    </tr>\n  </thead>\n</table>",
    #     options = structure(list(autoFill = TRUE, columnDefs = list(
    #         list(className = "dt-right", targets = c(1, 2, 3, 4,
    #             5, 6, 7, 8, 9, 10, 11)), list(orderable = FALSE, targets = 0)),
    #         order = list(), autoWidth = FALSE, orderClasses = FALSE), escapeIdx = "true")), colnames = c(" ",
    #             "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
    #             "gear", "carb"), rownames = TRUE), width = NULL, height = NULL,
    #     sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL,
    #         padding = NULL, viewer = list(defaultWidth = NULL, defaultHeight = NULL,
    #             padding = NULL, fill = TRUE, suppress = FALSE, paneHeight = NULL),
    #         browser = list(defaultWidth = NULL, defaultHeight = NULL,
    #             padding = NULL, fill = FALSE, external = FALSE),
    #         knitr = list(defaultWidth = "100%", defaultHeight = "auto",
    #             figure = FALSE)), dependencies = list(structure(list(
    #                 name = "jquery", version = "3.6.0", src = list(file = "lib/3.6.0"),
    #                 meta = NULL, script = "jquery-3.6.0.min.js", stylesheet = NULL,
    #                 head = NULL, attachment = NULL, package = "jquerylib",
    #                 all_files = TRUE), class = "html_dependency"), structure(list(
    #                     name = "dt-core", version = "1.11.3", src = list(file = "/Users/jankothyson/Library/Caches/org.R-project.R/R/renv/cache/v5/R-4.1/aarch64-apple-darwin20/DT/0.22/fc53164de9ee32692eef6fdd14115381/DT/htmlwidgets/lib/datatables"),
    #                     meta = NULL, script = "js/jquery.dataTables.min.js",
    #                     stylesheet = c("css/jquery.dataTables.min.css", "css/jquery.dataTables.extra.css"
    #                     ), head = NULL, attachment = NULL, package = NULL, all_files = FALSE), class = "html_dependency"),
    #                 AutoFill = structure(list(name = "dt-ext-autofill", version = "1.11.3",
    #                     src = list(file = "/Users/jankothyson/Library/Caches/org.R-project.R/R/renv/cache/v5/R-4.1/aarch64-apple-darwin20/DT/0.22/fc53164de9ee32692eef6fdd14115381/DT/htmlwidgets/lib/datatables-extensions/AutoFill"),
    #                     meta = NULL, script = "js/dataTables.autoFill.min.js",
    #                     stylesheet = "css/autoFill.dataTables.min.css", head = NULL,
    #                     attachment = NULL, package = NULL, all_files = FALSE), class = "html_dependency"),
    #                 structure(list(name = "jquery", version = "3.5.1", src = list(
    #                     file = "lib/jquery"), meta = NULL, script = "jquery.min.js",
    #                     stylesheet = NULL, head = NULL, attachment = NULL,
    #                     package = "crosstalk", all_files = TRUE), class = "html_dependency"),
    #                 structure(list(name = "crosstalk", version = "1.2.0",
    #                     src = list(file = "www"), meta = NULL, script = "js/crosstalk.min.js",
    #                     stylesheet = "css/crosstalk.min.css", head = NULL,
    #                     attachment = NULL, package = "crosstalk", all_files = TRUE), class = "html_dependency")),
    #     elementId = NULL, preRenderHook = function (instance)
    #     {
    #         data = instance[["x"]][["data"]]
    #         if (object.size(data) > 1500000 && getOption("DT.warn.size",
    #             TRUE))
    #             warning("It seems your data is too big for client-side DataTables. You may ",
    #                 "consider server-side processing: https://rstudio.github.io/DT/server.html")
    #         data = escapeData(data, escape, colnames)
    #         data = unname(data)
    #         instance$x$data = data
    #         instance
    #     }, jsHooks = list()), class = c("datatables", "htmlwidget"
    #     ), package = "DT")
    #
    # expect_equal(result, expectation, ignore_attr = TRUE)
})

# buttons ---------------------------------------------------------

test_that("buttons", {
    result <- dt_bundle_buttons()
    expectation <- list(
        extensions = "Buttons",
        options = list(
            # dom = "Bfrtip",
            dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
            buttons = c("colvis", "copy", "csv", "excel", "pdf", "print")
        )
    )
    expect_identical(result, expectation)

    result <- dt_bundle_buttons("colvis")
    expectation <- list(
        extensions = "Buttons",
        options = list(
            dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
            buttons = c("colvis")
        )
    )
    expect_identical(result, expectation)

    result <- dt_bundle_buttons("colvis", "print")
    expectation <- list(
        extensions = "Buttons",
        options = list(
            dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
            buttons = c("colvis", "print")
        )
    )
    expect_identical(result, expectation)

    result <- dt_bundle_buttons(
        list(extend = "colvis", text = "ABC"),
        "copy"
    )
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "ABC"), "copy")))
    expect_identical(result, expectation)
})

test_that("buttons: en", {
    result <- dt_bundle_buttons_en()
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Columns"),
            list(extend = "collection", buttons = c("csv", "excel",
                "pdf"), text = "Download"), "copy", "print")))
    expect_identical(result, expectation)

    result <- dt_bundle_buttons_en("colvis")
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Columns"))))
    expect_identical(result, expectation)

    result <- dt_bundle_buttons_en("colvis", "print")
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Columns"),
            "print")))
    expect_identical(result, expectation)
})

test_that("buttons: de", {
    result <- dt_bundle_buttons_de()
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Spalten"),
            list(extend = "collection", buttons = c("csv", "excel",
                "pdf"), text = "Download"), list(extend = "copy", text = "Kopieren"),
            "print")))
    expect_identical(result, expectation)

    result <- dt_bundle_buttons_de("colvis")
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Spalten"))))
    expect_identical(result, expectation)

    result <- dt_bundle_buttons_de("colvis", "print")
    expectation <- list(extensions = "Buttons", options = list(dom = "Blftipr",
        buttons = list(list(extend = "colvis", text = "Spalten"),
            "print")))
    expect_identical(result, expectation)
})

# colreorder ------------------------------------------------------

test_that("colreorder", {
    result <- dt_bundle_colreorder()
    expectation <- list(
        extensions = c(ColReorder = "ColReorder") %>% unname(),
        options = list(colReorder = list(realtime = FALSE))
    )
    expect_identical(result, expectation)
})

# fixedcolumns ----------------------------------------------------

test_that("fixedcolumns", {
    result <- dt_bundle_fixedcolumns()
    expectation <- list(extensions = "FixedColumns", options = list(
        scrollX = TRUE, fixedColumns = list(left = 1L)
    ))
    expect_identical(result, expectation)

    result <- dt_bundle_fixedcolumns(left = "cyl", .data = mtcars)
    expectation <- list(extensions = "FixedColumns", options = list(scrollX = TRUE,
        fixedColumns = list(left = 3L)))
    expect_identical(result, expectation)
})

# fixedheader -----------------------------------------------------

test_that("fixedheader", {
    result <- dt_bundle_fixedheader()
    expectation <- list(extensions = "FixedHeader", options = list(
        pageLength = 50, fixedHeader = TRUE))
    expect_identical(result, expectation)
})

# keytable --------------------------------------------------------

test_that("keytable", {
    result <- dt_bundle_keytable()
    expectation <- list(extensions = "KeyTable",
        options = list(keys = TRUE))
    expect_identical(result, expectation)
})

# responsive ------------------------------------------------------

test_that("responsive", {
    result <- dt_bundle_responsive()
    expectation <- list(extensions = "Responsive", options = list(responsive = TRUE))
    expect_identical(result, expectation)
})

# rowgroup --------------------------------------------------------

test_that("rowgroup", {
    result <- dt_bundle_rowgroup()
    expectation <- list(
        extensions = "RowGroup",
        options = list(rowGroup = list(dataSrc = 1)),
        selection = "none"
    )
    expect_identical(result, expectation)
})

# rowreorder ------------------------------------------------------

test_that("rowreorder", {
    result <- dt_bundle_rowreorder()
    expectation <- list(
        extensions = "RowReorder",
        options = list(order = list(c("0", "asc")), rowReorder = TRUE)
    )
    expect_identical(result, expectation)
})

# scroller ----------------------------------------------------------------

test_that("scroller", {
    result <- dt_bundle_scroller()
    expectation <- list(extensions = "Scroller", options = list(dom = "Slftipr",
        deferRender = TRUE, scrollY = 200, scroller = TRUE))
    expect_identical(result, expectation)

    result <- dt_bundle_scroller(scrollY = 100)
    expectation <- list(extensions = "Scroller", options = list(dom = "Slftipr",
        deferRender = TRUE, scrollY = 100, scroller = TRUE))
    expect_identical(result, expectation)
})

# searchpanes -------------------------------------------------------------

test_that("searchpanes", {
    result <- dt_bundle_searchpanes()
    expectation <- list(extensions = c("SearchPanes", "Select"), options = list(
        dom = "Plftipr", columnDefs = list(list(searchPanes = list(
            show = FALSE), targets = 1L))), selection = "none")
    expect_identical(result, expectation)

    result <- dt_bundle_searchpanes(targets = 1:4)
    expectation <- list(extensions = c("SearchPanes", "Select"), options = list(
        dom = "Plftipr", columnDefs = list(list(searchPanes = list(
            show = FALSE), targets = 1:4))), selection = "none")
    expect_identical(result, expectation)

    result <- dt_bundle_searchpanes(targets = "mpg", .data = mtcars)
    expectation <- list(extensions = c("SearchPanes", "Select"), options = list(
        dom = "Plftipr", columnDefs = list(list(searchPanes = list(
            show = FALSE), targets = 2:11))), selection = "none")
    expect_identical(result, expectation)
})

# select ------------------------------------------------------------------

test_that("select", {
    result <- dt_bundle_select()
    expectation <- list(extensions = c("Select", "Buttons"), options = list(dom = "Blftipr",
        rowId = 0, buttons = c("selectAll", "selectNone", "selectRows",
            "selectColumns", "selectCells"), select = list(style = "os",
                items = "row")), selection = "none")
    expect_identical(result, expectation)

    result <- dt_bundle_select("selectAll", "selectNone")
    expectation <- list(extensions = c("Select", "Buttons"), options = list(dom = "Blftipr",
        rowId = 0, buttons = c("selectAll", "selectNone"), select = list(
            style = "os", items = "row")), selection = "none")
    expect_identical(result, expectation)

    expect_error(dt_bundle_select("selectAll", "invalid"))
})

# [Option] lengthmenue ----------------------------------------------------

test_that("lengthmenue", {
    result <- dt_bundle_lengthmenue()
    expectation <- list(options = list(dom = "lftipr", paging = TRUE, lengthMenu = list(
        c(15, 50, 100, -1), c("15", "50", "100", "All")), pageLength = 50))
    expect_identical(result, expectation)

    result <- dt_bundle_lengthmenue(lengthMenu = c(15, 50, 100))
    expectation <- list(options = list(dom = "lftipr", paging = TRUE, lengthMenu = c(15,
        50, 100), pageLength = 50))
    expect_identical(result, expectation)
})

# [Option] internationalization -------------------------------------------

test_that("internationalization", {
    result <- dt_bundle_internationalization()
    expectation <- list(options = list(language = list(url = character())))
    expect_identical(result, expectation)
})

test_that("internationalization: de", {
    result <- dt_bundle_internationalization(url = "//cdn.datatables.net/plug-ins/1.12.0/i18n/de-DE.json")
    expectation <- list(options = list(language = list(url = "//cdn.datatables.net/plug-ins/1.12.0/i18n/de-DE.json")))
    expect_identical(result, expectation)
})

# Other -------------------------------------------------------------------

test_that("naturaljs", {
    result <- dt_bundle_naturaljs()
    expectation <- list(
        plugins = "natural",
        options = list(dom = "Pfrtip", columnDefs = list(
            list(
                type = "natural",
                searchPanes = list(show = FALSE),
                targets = 1
            )
        )),
        selection = "none"
    )
    expect_identical(result, expectation)
})

test_that("scroller", {
    result <- dt_bundle_scroller()
    expectation <- list(extensions = "Scroller", options = list(dom = "Slftipr",
        deferRender = TRUE, scrollY = 200, scroller = TRUE))
    expect_identical(result, expectation)
})

test_that("searchpanes", {
    result <- dt_bundle_searchpanes()
    expectation <- list(extensions = c("SearchPanes", "Select"), options = list(
        dom = "Plftipr", columnDefs = list(list(searchPanes = list(
            show = FALSE), targets = 1L))), selection = "none")
    expect_identical(result, expectation)
})

test_that("dom", {
    result <- dt_bundle_dom()
    expectation <- list(options = list(dom = structure("lftipr")))
    expect_identical(result, expectation)

    result <- dt_bundle_dom(f = FALSE, B = FALSE, Q = TRUE, consolidate = FALSE)
    expectation <- list(options = list(dom = structure("Qltipr")))
    expect_identical(result, expectation)
})
