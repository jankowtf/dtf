#' data_render_dt UI Function
#'
#' @param id [[character]]
#' @param output_id [[character]]
#'
#' @description A shiny Module.
#'
#' @importFrom shiny NS tagList
mod_render_dt_ui <- function(
    id,
    output_id = "dt"
){
    ns <- NS(id)

    shiny::dataTableOutput(ns(output_id))
}

#' data_render_dt Server Functions
#'
#' @param id [[character]]
#' @param rdata Reactive data
#' @param filter [[function]]
#' @param scrollY [[integer]]
#' @param escape [[logical]]
#' @param editable [[logical]]
#' @param scrollX [[integer]]
#' @param selection [[character]]
#' @param fixedColumns.leftColumns [[character]]
#' @param bundles [[list]]
#' @param trans_fn [[function]]
#' @param output_id [[character]]
#' @param verbose [[logical]] Print tracing information
#'
#' @noRd
mod_render_dt_server <- function(
    id,
    output_id = character(),
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

        # Bundles
        bundles_default <- list(
            dt_bundle_scroller(scrollY = scrollY),
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
