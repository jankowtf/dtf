# Bundles -----------------------------------------------------------------

#' DT bundle: `dom`
#'
#' See https://datatables.net/reference/option/dom
#'
#' @param l [[logical]] The `l`ength changing input control
#' @param f [[logical]] The `f`iltering input
#' @param t [[logical]] The `t`able itself
#' @param i [[logical]] The table `i`nformation summary
#' @param p [[logical]] The `p`agination control
#' @param r [[logical]] The p`r`ocessing display element
#' @param B [[logical]] Buttons
#' @param R [[logical]] ColReorder
#' @param S [[logical]] Scroller
#' @param P [[logical]] SearchPanes
#' @param Q [[logical]] SearchBuilder
#' @param standalone [[logical]] Return actual `dom` value or listified option
#' @param consolidate [[logical]] Consolidate with `dom_default`
#' @param dom_default [[character] or [list]] Dependes on `consolidate`
#'
#' @return [[list]] DT options with `dom` element being set based on function
#'   args
#' @export
#'
#' @examples
#' dt_bundle_dom()
#' dt_bundle_dom(f = FALSE, B = FALSE, Q = TRUE)
dt_bundle_dom <- function(
    # --- Options
    l = TRUE,
    f = TRUE,
    t = TRUE,
    i = TRUE,
    p = TRUE,
    r = TRUE,
    # --- Extensions
    B = FALSE,
    R = FALSE,
    S = FALSE,
    P = FALSE,
    Q = FALSE,
    standalone = FALSE,
    consolidate = TRUE
) {
    f <- ifelse(f, "f", "")
    i <- ifelse(i, "i", "")
    l <- ifelse(l, "l", "")
    p <- ifelse(p, "p", "")
    r <- ifelse(r, "r", "")
    t <- ifelse(t, "t", "")

    B <- ifelse(B, "B", "")
    Q <- ifelse(Q, "Q", "")
    P <- ifelse(P, "P", "")
    R <- ifelse(R, "R", "")
    S <- ifelse(S, "S", "")

    # dom <- "{B}{Q}{P}{R}{S}{f}{i}{l}{p}{r}{t}" %>%
    #     stringr::str_glue() %>%
    #     as.character()
    dom <- "{B}{R}{S}{P}{Q}{l}{f}{t}{i}{p}{r}" %>%
        stringr::str_glue() %>%
        as.character()

    if (consolidate) {
        dom_tokenized <- dom %>% stringr::str_split("") %>% unlist()
        dom_default_tokenized <- dt_bundle_dom(
            standalone = TRUE,
            consolidate = FALSE
        ) %>% stringr::str_split("") %>% unlist()

        # dom <- c(dom_default_tokenized, dom_tokenized) %>%
        dom <- c(dom_tokenized, dom_default_tokenized) %>%
            unique() %>%
            stringr::str_c(collapse = "")
    }

    if (!standalone) {
        list(
            options = list(
                dom = dom
            )
        )
    } else {
        dom
    }
}

#' DT bundle: `AutoFill`
#'
#' See https://datatables.net/extensions/autofill/,
#' https://datatables.net/reference/option/autoFill and
#' https://datatables.net/reference/option/#AutoFill
#'
#' @param columns ([integer] or [character]) Column indexes/positions or names.
#'   In case of names indexes are looked up via [lookup_column_positions()]
#' @param focus ([character]) Focus option
#' @param .options ([list]) Object to pass custom options beyond pre-defined
#'   arguments
#' @param .data ([tibble]) Optional data for column name lookup
#'
#' @return
#' @export
dt_bundle_autofill <- function(
    columns = integer(),
    focus = valid_dt_options_autofill_focus(1),
    # editor = "editor",
    .data = tibble::tibble(),
    .options = list()
) {
    extension <- dt_extensions("AutoFill")
    option_name <- dt_options("autoFill", extension = extension)

    # Early exit
    if (!length(columns) && !length(.options$columns)) {
        bundle <- compose_bundle(
            extension = extension,
            option_name = option_name
        )
        return(bundle)
    }

    extension_options <- compose_extension_options(
        columns = lookup_column_positions(
            data = .data,
            columns = columns,
            offset = FALSE
        ),
        focus = focus,
        .option_name = option_name,
        .extension_options = .options
    )

    options <- compose_options(
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extension = extension,
        options = options,
        option_name = option_name
    )

    bundle
}

#' DT bundle: `Buttons`
#'
#' Convenience wrapper around [dt_bundle_buttons_()].

#' @param ... Button names as [character]. See
#'   [valid_dt_options_buttons_names()]
#' @param .options ([list]) Optional way to provide extension options as list
#'   object. Other arguments that correspond to extension option entities are
#'   disregarded
#' @param .as_is ([logical]) Use argument values "as is", i.e. pass it along
#'   without further processing it. Experimental and not activated yet (see
#'   [dti::dt_bunle_buttons_de()] for commented-out section)
#'
#' @return
#' @export
#'
#' @examples
#' dt_bundle_buttons()
#' dt_bundle_buttons("csv", "print")
dt_bundle_buttons <- function(
    ...,
    .options = list(),
    .as_is = FALSE
) {
    buttons <- rlang::list2(...)

    if (!length(buttons)) {
        buttons <- valid_dt_options_buttons_names()
    }

    .flatten <- buttons %>% has_names() %>% unlist() %>% any()
    .as_is <- .flatten

    dt_bundle_buttons_(
        buttons = buttons,
        .options = .options,
        .as_is = .as_is,
        .flatten = .flatten
    )
}

#' DT bundle: `Buttons`
#'
#' See:
#' - [RStudio examples: buttons](https://rstudio.github.io/DT/003-tabletools-buttons.html)
#' - [DataTables reference: button extension](https://datatables.net/extensions/buttons/)
#' - [DataTables reference: button options](https://datatables.net/reference/option/buttons)
#'
#' Note:
#' - Option `dom` is automagically taken care of (set to `BRSfilprt` via
#' [dt_bundle_dom()])
#'
#' @param buttons ([character]) Button names. See
#'   [valid_dt_options_buttons_names()]
#' @param .options ([list]) Optional way to provide extension options as list
#'   object. Other arguments that correspond to extension option entities are
#'   disregarded
#' @param .as_is ([logical]) Use argument values "as is", i.e. pass it along
#'   without further processing it. Experimental and not activated yet (see
#'   [dti::dt_bunle_buttons_de()] for commented-out section)
#'
#' @return
#' @export
#'
#' @examples
#' dt_bundle_buttons_()
#' dt_bundle_buttons_(buttons = c("csv", "print"))
dt_bundle_buttons_ <- function(
    buttons = valid_dt_options_buttons_names(),
    .options = list(),
    .as_is = FALSE,
    .flatten = FALSE
) {
    extension <- dt_extensions("Buttons")
    option_name <- dt_options("buttons", extension = extension)

    extension_options <- compose_extension_options(
        if (!.as_is) {
            buttons
        } else {
            buttons %>% as.list()
        },
        .option_name = option_name,
        .extension_options = .options,
        .unlist = !.as_is,
        .flatten = .flatten
    )

    options <- compose_options(
        dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}


#' DT bundle: `Buttons`: language `de`
#'
#' See [dt_bundle_buttons()].
#'
#' @param ... ([character]) Button names. See
#'   [valid_dt_options_buttons_names()]
#' @param .options ([list]) Optional way to provide extension options as list
#'   object. Other arguments that correspond to extension option entities are
#'   disregarded
#'
#' @return
#' @export
#'
#' @examples
#' dt_bundle_buttons_de()
dt_bundle_buttons_de <- function(
    ...,
    .options = list()
) {
    extension <- dt_extensions("Buttons")
    option_name <- dt_options("buttons", extension = extension)

    buttons <- rlang::list2(...) %>% unlist()

    if (!length(buttons)) {
        buttons <- valid_dt_options_buttons_names()
    }

    # EXPERIMENTAL
    # Trying to directly leverage 'dt_bundle_buttons()'
    # bundle <- dt_bundle_buttons(
    #     buttons = buttons,
    #     .options = .options,
    #     .as_is = TRUE
    # )
    #
    # return(bundle)

    # Input handling
    # colvis <- if (colvis) list(extend = "colvis", text = "Spalten")
    # csv <- if (csv) "csv"
    # excel <- if (excel) "excel"
    # pdf <- if (pdf) "pdf"
    # download <- list(
    #     extend = "collection",
    #     buttons = c(csv, excel, pdf),
    #     text = "Download"
    # )
    # copy <- if (copy) list(extend = "copy", text = "Kopieren")
    # print <- if (print) list(extend = "print", text = "Drucken")

    # Input handling
    if (any(index <- buttons %in% "colvis")) {
        buttons[index] <- list(list(extend = "colvis", text = "Spalten"))
    }
    if (any(index <- buttons %in% c("csv", "excel", "pdf"))) {
        if (!inherits(buttons, "list")) {
            buttons <- buttons %>% as.list()
        }
        values <- buttons[index]
        buttons[index] <- NULL
        buttons <- append(
            buttons,
            list(list(
                extend = "collection",
                buttons = values %>% unlist(),
                text = "Download"
            )),
            after = 1
        )
    }
    if (any(index <- buttons %in% "copy")) {
        buttons[index] <- list(list(extend = "copy", text = "Kopieren"))
    }
    if (any(index <- buttons %in% "copy")) {
        buttons[index] <- list(list(extend = "print", text = "Drucken"))
    }

    extension_options <- compose_extension_options(
        buttons,
        .option_name = option_name,
        .extension_options = .options,
        .flatten = TRUE,
    )

    options <- compose_options(
        dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
        .extension_options = extension_options
    )

    compose_bundle(
        extensions = extension,
        options = options
    )
}

#' DT bundle: `Buttons`: language `en`
#'
#' @param ... ([character]) Button names. See
#'   [valid_dt_options_buttons_names()]
#' @param .options Placeholder for in case more flexibility is required.
#'   Currently not used
#'
#' @return
#' @export
#'
#' @examples
#' dt_bundle_buttons_en()
dt_bundle_buttons_en <- function(
    ...,
    .options = list()
) {
    extension <- dt_extensions("Buttons")
    option_name <- dt_options("buttons", extension = extension)

    buttons <- rlang::list2(...) %>% unlist()

    if (!length(buttons)) {
        buttons <- valid_dt_options_buttons_names()
    }

    # Input handling
    if (any(index <- buttons %in% "colvis")) {
        buttons[index] <- list(list(extend = "colvis", text = "Columns"))
    }
    if (any(index <- buttons %in% c("csv", "excel", "pdf"))) {
        if (!inherits(buttons, "list")) {
            buttons <- buttons %>% as.list()
        }
        values <- buttons[index]
        buttons[index] <- NULL
        buttons <- append(
            buttons,
            list(list(
                extend = "collection",
                buttons = values %>% unlist(),
                text = "Download"
            )),
            after = 1
        )
    }

    extension_options <- compose_extension_options(
        buttons,
        .option_name = option_name,
        .extension_options = .options,
        .flatten = TRUE
    )

    options <- compose_options(
        dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
        .extension_options = extension_options
    )

    compose_bundle(
        extensions = extension,
        options = options
    )
}

#' DT bundle: `ColReorder`
#'
#' See https://datatables.net/reference/option/
#' TODO: https://datatables.net/reference/option/rowReorder.editor
#'
#' @param realtime ([logical]) Show column reorder in realtime yes/no
#' @param .options ([list]) Object to pass custom options beyond pre-defined
#'   arguments
#' @return
#' @export
#' dt_bundle_colreorder()
#' dt_bundle_colreorder(realtime = TRUE)
dt_bundle_colreorder <- function(
    realtime = FALSE,
    .options = list()
) {
    extension <- dt_extensions("ColReorder")
    option_name <- dt_options("colReorder", extension = extension)

    extension_options <- compose_extension_options(
        realtime = realtime,
        .option_name = option_name,
        .extension_options = .options
    )

    options <- compose_options(
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `FixedColumns`
#'
#' See https://datatables.net/extensions/fixedcolumns/ and
#' https://datatables.net/reference/option/fixedColumns.left.
#'
#' @param left ([integer] or [character]) Column position or name contained. If
#'   *name* then `data` must be proviced for the position lookup.
#' @param right ([integer] or [character]) Column position or name contained. If
#'   *name* then `data` must be proviced for the position lookup.
#' @param .options ([list]) Object to pass custom options for this extension
#'   beyond pre-defined arguments. Placeholder in case more flexibility is
#'   needed
#' @param .data
#'
#' @return
#' @export
#' @examples
#' dt_bundle_fixedcolumns(left = 2)
#' dt_bundle_fixedcolumns(left = 2, right = 1)
#' dt_bundle_fixedcolumns(left = "disp", data = mtcars)
#' dt_bundle_fixedcolumns(.options = list(left = 2, right = 1))
dt_bundle_fixedcolumns <- function(
    left = 1L,
    right = integer(),
    .data = tibble::tibble(),
    .options = list()
) {
    extension <- dt_extensions("FixedColumns")
    option_name <- dt_options("fixedColumns", extension = extension)

    # Handle name lookup if data is  provided
    left <- left %>% lookup_column_positions(data = .data)
    right <- right %>% lookup_column_positions(data = .data, reverse = TRUE)

    extension_options <- compose_extension_options(
        left = left,
        right = right,
        .option_name = option_name,
        .extension_options = .options
    )

    options <- compose_options(
        scrollX = TRUE,
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `FixedHeader`
#'
#' See https://datatables.net/extensions/fixedheader/ and
#' https://datatables.net/reference/option/fixedHeader
#'
#' @param fixedHeader [[logical]] Enable fixed header yes/no
#' @param pageLength [[integer]] Page length option
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#'
#' @return
#' @export
dt_bundle_fixedheader <- function(
    fixedHeader = TRUE,
    pageLength = 50,
    .options = list()
) {
    extension <- dt_extensions("FixedHeader")
    option_name <- dt_options("fixedHeader", extension = extension)

    extension_options <- compose_extension_options(
        fixedHeader,
        .option_name = option_name,
        .extension_options = .options,
        .unlist = TRUE
    )

    options <- compose_options(
        pageLength = pageLength,
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `KeyTable`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/keytable/ and
#' https://datatables.net/reference/option/keys
#'
#' TODO: Add remaining options
#'
#' @param keys [[logical]] Use KeyTable extension yes/no
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#'
#' @return
#' @export
dt_bundle_keytable <- function(
    keys = TRUE,
    .options = list()
) {
    extension <- dt_extensions("KeyTable")
    option_name <- dt_options("keys", extension = extension)

    extension_options <- compose_extension_options(
        keys,
        .option_name = option_name,
        .extension_options = .options,
        .unlist = TRUE
    )

    options <- compose_options(
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `Responsive`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/responsive/ and
#' https://datatables.net/reference/option/responsive
#'
#' TODO: Add remaining options
#'
#' @param responsive [[logical]] Use KeyTable extension yes/no
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#'
#' @return
#' @export
dt_bundle_responsive <- function(
    responsive = TRUE,
    .options = list()
) {
    extension <- dt_extensions("Responsive")
    option_name <- dt_options("responsive", extension = extension)

    extension_options <- compose_extension_options(
        responsive,
        .option_name = option_name,
        .extension_options = .options,
        .unlist = TRUE
    )

    options <- compose_options(
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `RowGroup`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/rowgroup/ and
#' https://datatables.net/reference/option/rowGroup
#'
#' TODO: Add remaining options
#'
#' @param dataSrc [[integer]] Position or name of column to use as grouping
#'   column.
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#' @param .data
#'
#' @return
#' @export
dt_bundle_rowgroup <- function(
    dataSrc = 1,
    .options = list(),
    .data = tibble::tibble()
) {
    extension <- dt_extensions("RowGroup")
    option_name <- dt_options("rowGroup", extension = extension)

    # Handle position lookup if data is  provided
    dataSrc <- dataSrc %>% lookup_column_positions(data = .data, offset = FALSE)

    extension_options <- compose_extension_options(
        dataSrc = dataSrc,
        .option_name = option_name,
        .extension_options = .options
    )

    options <- compose_options(
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options,
        selection = "none"
    )

    bundle
}

#' DT bundle: `RowReorder`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/rowreorder/ and
#' https://datatables.net/reference/option/rowReorder
#'
#' TODO: Add remaining options
#'
#' @param rowReorder [[logical]] Use RowReorder extension yes/no
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#'
#' @return
#' @export
dt_bundle_rowreorder <- function(
    rowReorder = TRUE,
    .options = list()
) {
    extension <- dt_extensions("RowReorder")
    option_name <- dt_options("rowReorder", extension = extension)

    extension_options <- compose_extension_options(
        rowReorder,
        .option_name = option_name,
        .extension_options = .options,
        .unlist = TRUE
    )

    options <- compose_options(
        order = list(c(0 , 'asc')),
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `Scroller`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/scroller/ and
#' https://datatables.net/reference/option/scroller
#'
#' TODO: Add remaining options
#'
#' @param scroller [[logical]] Use extension yes/no
#' @param deferRender [[logical]] Defer rendering yes/no
#' @param scrollY [[integer]] Vertical scrolling space
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#'
#' @return
#' @export
dt_bundle_scroller <- function(
    scroller = TRUE,
    deferRender = TRUE,
    scrollY = 200,
    .options = list()
) {
    extension <- dt_extensions("Scroller")
    option_name <- dt_options("scroller", extension = extension)

    extension_options <- compose_extension_options(
        scroller,
        .option_name = option_name,
        .extension_options = .options,
        .unlist = TRUE
    )

    options <- compose_options(
        dom = dt_bundle_dom(S = TRUE, standalone = TRUE),
        deferRender = deferRender,
        scrollY = scrollY,
        .extension_options = extension_options
    )

    bundle <- compose_bundle(
        extensions = extension,
        options = options
    )

    bundle
}

#' DT bundle: `SearchPanes`
#'
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/searchpanes/ and
#' https://datatables.net/reference/option/searchPanes
#'
#' TODO: Add remaining options
#'
#' @param show
#' @param targets
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#' @param .data
#'
#' @return
#' @export
dt_bundle_searchpanes <- function(
    show = FALSE,
    targets = 1L,
    .options = list(),
    .data = tibble::tibble()
) {
    if (length(.options)) {
        stop("Use of '.options' not supported yet.")
    }

    extension_sp <- dt_extensions("SearchPanes")
    option_name_sp <- dt_options("searchPanes", extension = extension_sp)

    extension_s <- dt_extensions("Select")
    option_name_s <- dt_options("select", extension = extension_s)

    extensions <- c(extension_sp, extension_s)

    extension_options <- compose_extension_options(
        show = show,
        .option_name = option_name_sp,
        .extension_options = .options
    )

    # Handle position lookup if data is provided
    targets <- targets %>% lookup_column_positions(data = .data,
        negate = TRUE, offset = FALSE)

    column_def_options <- compose_extension_options(
        c(
            extension_options,
            list(targets = targets)
        ),
        .option_name = "columnDefs",
        .extension_options = .options
    )

    options <- compose_options(
        # dom = "Pfrtip",
        dom = dt_bundle_dom(P = TRUE, standalone = TRUE),
        # .extension_options = extension_options
        .extension_options = column_def_options
    )

    bundle <- compose_bundle(
        extensions = extensions,
        options = options,
        selection = "none"
    )

    bundle
    # list(
    #     extensions = dt_extensions('Select', 'SearchPanes'),
    #     options = list(
    #         dom = dt_bundle_dom(P = TRUE),
    #         columnDefs = list(list(
    #             searchPanes = list(show = FALSE),
    #             targets = 1:4
    #         ))
    #     ),
    #     selection = "none"
    # )
}

#' DT bundle: `Select` with `Buttons`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/extensions/searchpanes/ and
#' https://datatables.net/reference/option/searchPanes
#'
#' TODO: Add remaining options
#'
#' @param ...
#' @param style
#' @param items
#' @param selection
#' @param .options [[list]] Object to pass custom options for this extension
#'   beyond pre-defined arguments
#' @param .buttons
#' @param .as_is
#' @param .flatten
#' @return
#' @export
dt_bundle_select <- function(
    ...,
    style = "os",
    items = "row",
    selection = "none",
    .options = list(),
    .buttons = valid_dt_options_buttons_select_names(),
    .as_is = FALSE,
    .flatten = FALSE
) {
    # list(
    #     extensions = dt_extensions('Select', 'Buttons'),
    #     select = list(
    #         style = 'os',
    #         items = 'row'
    #     ),
    #     dom = 'Blfrtip',
    #     rowId = 0,
    #     buttons = c(
    #         'selectAll',
    #         'selectNone',
    #         'selectRows',
    #         'selectColumns',
    #         'selectCells'
    #     ),
    #     selection = "none"
    # )

    extension_select <- dt_extensions("Select")
    option_name_select <- dt_options("select", extension = extension_select)

    buttons <- if (!length(.btns <- rlang::list2(...))) {
        .buttons
    } else {
        .btns
    }

    buttons %>% unlist() %>% valid_dt_options_buttons_select_names()

    extension_buttons <- dt_extensions("Buttons")
    option_name_buttons <- dt_options("buttons", extension = extension_buttons)

    extension_options_select <- compose_extension_options(
        style = style,
        items = items,
        .option_name = option_name_select,
        .extension_options = .options
    )

    extension_options_buttons <- compose_extension_options(
        buttons,
        .option_name = option_name_buttons,
        .extension_options = .options,
        .unlist = !.as_is
    )

    options <- compose_options(
        dom = dt_bundle_dom(B = TRUE, standalone = TRUE),
        rowId = 0,
        .extension_options = c(
            extension_options_buttons,
            extension_options_select
        )
    )

    bundle <- compose_bundle(
        extensions = c(extension_select, extension_buttons),
        options = options,
        selection = selection
    )

    bundle
}

#' DT bundle: `lengthMenue`
#'
#' Not fully compliant with full DataTables functionality.
#'
#' See https://datatables.net/reference/option/lengthMenu
#'
#' TODO: Add remaining options
#'
#' See also: https://shiny.rstudio.com/articles/datatables.html
#'
#' @param lengthMenu
#' @param pageLength
#' @param .options
#'
#' @return
#' @export
#'
#' @examples
#' dt_bundle_lengthmenue()
#' dt_bundle_lengthmenue(pageLength = 15)
#' dt_bundle_lengthmenue(lengthMenu = c(15, 50, 100))
dt_bundle_lengthmenue <- function(
    pageLength = 50,
    lengthMenu = list(c(15, 50, 100, -1), c("15", "50", "100", "All")),
    .options = list()
) {
    options <- compose_options(
        dom = dt_bundle_dom(p = TRUE, standalone = TRUE),
        paging = TRUE,
        lengthMenu = lengthMenu,
        pageLength = pageLength,
        .extension_options = list()
    )

    bundle <- compose_bundle(
        extensions = list(),
        options = options
    )

    bundle
}

#' DT bundle: bundle `Internationalization`
#'
#' See https://datatables.net/plug-ins/i18n/
#'
#' @param url [[character]] True URL to DataTables API (see
#'   https://datatables.net/plug-ins/i18n/) or path to local JSON file
#'
#' @return
#' @export
dt_bundle_internationalization <- function(
    url = character()
) {
    # list(
    #     options = list(
    #         oLanguage = list(
    #             # sInfo = "Zeige _START_ bis _END_ von _TOTAL_ Zeilen",
    #             # sInfoThousands = "."
    #             sInfo = "Showing _START_ to _END_ of _TOTAL_ rows",
    #             sInfoThousands = ","
    #         )
    #     )
    # )
    # Keep as reference

    options <- compose_options(
        language = list(url = url),
        .extension_options = list()
    )

    bundle <- compose_bundle(
        extensions = list(),
        options = options
    )

    bundle
}

#' DT bundle: `Internationalization: en`
#'
#' Convenience function for language `en`
#'
#' @param url [[character]] True URL to DataTables API (see
#'   https://datatables.net/plug-ins/i18n/) or path to local JSON file
#'
#' @return
#' @export
#'
#' @examples
dt_bundle_internationalization_en <- function(
    url = character()
) {
    dt_bundle_internationalization(url = url)
}

#' DT bundle: `Internationalization: de`
#'
#' Convenience function for language `de`
#'
#' @param url [[character]] True URL to DataTables API (see
#'   https://datatables.net/plug-ins/i18n/) or path to local JSON file
#'
#' @return
#' @export
#'
#' @examples
dt_bundle_internationalization_de <- function(
    url = "//cdn.datatables.net/plug-ins/1.12.0/i18n/de-DE.json"
) {
    dt_bundle_internationalization(url = url)
}

# Other -------------------------------------------------------------------

#' DT bundle: bundle `initComplete`
#'
#' See https://datatables.net/reference/option/initComplete
#'
#' @return
#' @export
dt_bundle_initcomplete <- function() {
    list(
        options = list(
            initComplete = DT::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"
            )
        )
    )
}



#' DT bundle: plugin `naturalJS`
#'
#' See https://datatables.net/reference/option/
#'
#' @param targets [?]
#'
#' @return
#' @export
dt_bundle_naturaljs <- function(
    targets = 1
) {
    list(
        plugins = "natural",
        options = list(
            dom = 'Pfrtip',
            columnDefs = list(
                list(
                    type = "natural",
                    searchPanes = list(show = FALSE),
                    targets = targets
                )
            )
        ),
        selection = "none"
    )
}
