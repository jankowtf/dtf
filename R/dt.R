#' Apply DT bundle configuration
#'
#' @param data [[tibble] or [data.frame]] Data
#' @param bundles [[list] (preferred) or [character] (legacy)] "Extension + options" bundles
#' @param .verbose [[logical]] Output tracing info yes/no
#' @param ... Additional arguments passed along to [DT::datatable]
#'
#' @return
#' @export
datatable2 <- function(
    data,
    bundles = character(),
    .verbose = FALSE,
    ...
) {
    # Get args from bundles
    args <- bundles %>%
        dt_process_bundles() %>%
        drop::drop_null()

    # Tracing
    if (.verbose) {
        logger::log_trace("Processed bundles:")
        logger::log_eval(args)
    }

    # Compose arguments
    args <- c(
        list(
            data = data
        ),
        args,
        ...
    )

    # Create DT
    rlang::call2(
        "datatable",
        !!!args,
        .ns = "DT"
    ) %>%
        rlang::eval_tidy()
}

# Extensions --------------------------------------------------------------

dt_extensions <- function(...) {
    valid_dt_extensions(...)
}

# Options -----------------------------------------------------------------

#' Config: DT: options
#'
#' @param ... [[character]] Options
#' @param extension [[character]] Option extension. See valid
#'   [valid_dt_extensions] and https://datatables.net/reference/option/
#'
#' @return
#' @export
#'
#' @examples
#' dt_options("deferLoading")
#' dt_options("autoFill", extension = "AutoFill")
dt_options <- function(..., extension = character()) {
    # Function dispatch
    options_fn <- if (!length(extension)) {
        "valid_dt_options"
    } else {
        "valid_dt_options_{tolower(extension)}"
    }

    options_fn %>%
        stringr::str_glue() %>%
        rlang::call2(
            .,
            ...
        ) %>%
        rlang::eval_tidy()
}


