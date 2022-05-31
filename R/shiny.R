#' data_render_dt UI Function
#'
#' @param id [[character]]
#' @param output_id [[character]]
#' @param verbose [[logical]]
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_render_dt_ui <- function(
    id = character(),
    output_id = "dt",
    verbose = FALSE
){
    ns <- NS(id)

    shiny_trace_ns_ui(
        fn_name = "mod_render_dt_ui",
        id_inner = output_id,
        ns = ns,
        verbose = verbose
    )

    DT::dataTableOutput(ns(output_id))
}

#' data_render_dt Server Functions
#'
#' @param id [[character]]
#' @param output_id [[character]]
#' @param data
#' @param filter [[character]] Column filter settings. See
#'   [valid_dt_filter_values] and [DT::datatable] for details
#' @param scrollY [[integer]]
#' @param left [[integer]]
#' @param right [[integer]]
#' @param trans_fn [[function]]
#' @param rename_fn [[function]]
#' @param .bundles_default [[list]] Default bundles
#' @param .bundles [[list]]
#' @param .rownames [[logical]]
#' @param .editable [[logical]]
#' @param .escape [[logical]]
#' @param verbose [[logical]]
#' @param ... Addtional arguments passed to [DT::datatable()]
#'
#' @export
mod_render_dt_server <- function(
    id = character(),
    output_id = "dt",
    data,
    filter = valid_dt_filter_values(1),
    scrollY = 400,
    left = integer(),
    right = integer(),
    # selection = valid_dt_arg_selection("none"),
    trans_fn = identity,
    rename_fn = identity,
    .bundles_default = list(
        dt_bundle_scroller(scrollY = scrollY),
        dt_bundle_colreorder(),
        dt_bundle_fixedheader(),
        dt_bundle_fixedcolumns(left = left),
        dt_bundle_keytable(),
        dt_bundle_internationalization()
    ),
    .bundles = list(),
    # dt_bundle_buttons_en(), # keep this in mind
    .rownames = TRUE,
    .editable = FALSE,
    .escape = TRUE,
    verbose = FALSE,
    ...
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        shiny_trace_ns_server(
            fn_name = "mod_render_dt_server",
            id_inner = output_id,
            verbose = verbose
        )

        # Bundles
        # bundles_default <- list(
        #     dt_bundle_scroller(scrollY = scrollY),
        #     dt_bundle_colreorder(),
        #     dt_bundle_fixedheader(),
        #     dt_bundle_fixedcolumns(left = left),
        #     dt_bundle_keytable(),
        #     dt_bundle_internationalization()
        # )

        bundles <- c(
            .bundles_default,
            .bundles
        )

        # Render
        data_rendered <- DT::renderDataTable({
            # browser()
            # Input handling
            if (!inherits(data, "reactive")) {
                data <- function() data
            }

            # Transformations
            data <- data() %>%
                # Apply transformation function
                trans_fn() %>%
                # Apply renaming function
                rename_fn()

            # Early exit
            if (is.null(data)) {
                return(tibble::tibble())
            }

            data %>% datatable2(
                bundles = bundles,
                rownames = .rownames,
                editable = .editable,
                escape = .escape,
                # selection = selection,
                filter = filter,
                .verbose = verbose,
                ...
            )
        })

        # Write to output
        if (length(output_id)) {
            # browser()
            output[[output_id]] <- data_rendered
        }

        # Return rendered data
        return(data_rendered)
    })
}

shiny_trace_ns_ui <- function(
    fn_name,
    id_inner,
    ns,
    verbose = FALSE
) {
    if (verbose) {
        logger::log_trace("Function: {fn_name}")
        logger::log_trace("ns: {ns(character())}")
        logger::log_trace("id_inner: {id_inner}")
        logger::log_trace("ns(id_inner): {ns(id_inner)}")
    }
}

shiny_trace_ns_server <- function(
    fn_name,
    id_inner,
    verbose = FALSE,
    .id = character()
) {
    shiny::moduleServer(.id, function(input, output, session) {
        ns <- session$ns

        observe({
            if (verbose) {
                logger::log_trace("Function: {fn_name}")
                logger::log_trace("ns: {ns(character())}")

                id_inner <- if (inherits(id_inner, "reactive")) {
                    id_inner()
                } else (
                    id_inner
                )

                id_inner %>% purrr::walk(~logger::log_trace("id_inner: {.x}"))
                id_inner %>% purrr::walk(~logger::log_trace("ns(id_inner): {ns(.x)}"))
                id_inner %>% purrr::walk(~input[[.x]] %>% logger::log_eval())
            }
        })
    })
}
