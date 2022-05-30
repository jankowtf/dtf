#' data_render_dt UI Function
#'
#' @param id [[character]]
#' @param output_id [[character]]
#' @param .verbose [[logical]]
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
#' @export
mod_render_dt_ui <- function(
    id = character(),
    output_id = "dt",
    .verbose = FALSE
){
    ns <- NS(id)

    if (.verbose) {
        logger::log_trace("Function: mod_render_dt_ui")
        logger::log_trace("ns: {ns(character())}")
        logger::log_trace("output_id: {output_id}")
        logger::log_trace("ns(output_id): {ns(output_id)}")
        observe({
            input %>% names() %>% sort() %>% print()
            input[[output_id]] %>% print()
        })
    }

    DT::dataTableOutput(ns(output_id))
}

#' data_render_dt Server Functions
#'
#' @param id [[character]]
#' @param output_id [[character]]
#' @param data
#' @param scrollY [[integer]]
#' @param left [[integer]]
#' @param right [[integer]]
#' @param trans_fn [[function]]
#' @param rename_fn [[function]]
#' @param .bundles [[list]]
#' @param .rownames [[logical]]
#' @param .editable [[logical]]
#' @param .escape [[logical]]
#' @param .verbose [[logical]]
#' @param ... Addtional arguments passed to [DT::datatable()]
#'
#' @export
mod_render_dt_server <- function(
    id = character(),
    output_id = "dt",
    data,
    # filter = c("none", "bottom", "top"),
    scrollY = 400,
    left = integer(),
    right = integer(),
    # selection = valid_dt_arg_selection("none"),
    # fixedColumns.leftColumns = 1,
    trans_fn = identity,
    rename_fn = identity,
    .bundles = list(),
    # dt_bundle_buttons_en(), # keep this in mind
    .rownames = TRUE,
    .editable = FALSE,
    .escape = TRUE,
    .verbose = FALSE,
    ...
) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (.verbose) {
            logger::log_trace("Function: mod_render_dt_server")
            logger::log_trace("ns: {ns(character())}")
            logger::log_trace("output_id: {output_id}")
            logger::log_trace("ns(output_id): {ns(output_id)}")
            observe({
                input %>% names() %>% sort() %>% print()
                input[[output_id]] %>% print()
            })
        }

        # Bundles
        bundles_default <- list(
            dt_bundle_scroller(scrollY = scrollY),
            dt_bundle_colreorder(),
            dt_bundle_fixedheader(),
            dt_bundle_fixedcolumns(left = left),
            dt_bundle_keytable(),
            dt_bundle_internationalization()
        )

        bundles <- c(
            bundles_default,
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
                # filter = filter,
                .verbose = .verbose,
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
