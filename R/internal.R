#' Title
#'
#' @param ...
#' @param .option_name
#' @param .extension_options
#' @param .unlist
#' @param .flatten
#'
#' @return
#' @export
#'
#' @examples
compose_extension_options <- function(
    ...,
    .option_name,
    .extension_options,
    .unlist = FALSE,
    .flatten = FALSE
) {
    if (length(.extension_options)) {
        return(.extension_options %>% list() %>% purrr::set_names(.option_name))
    }

    values <- rlang::list2(...)

    if (.unlist) {
        values <- values %>% unlist()
    }

    if (.flatten) {
        values <- values %>% purrr::flatten()
    }

    # Drop NULL
    # values <- values %>%
    #     purrr::map_lgl(is.null %>% purrr::negate()) %>%
    #     `[`(values, .)

    # values <- values %>% drop::drop_null() %>% purrr::flatten()

    # Post-process
    values <- if (inherits(values, "list")) {
        values %>%
            drop::drop_null() %>%
            drop::drop_empty()
    } else {
        values
    }

    list(values) %>% purrr::set_names(.option_name)
}

compose_options <- function(
    ...,
    .extension_options
) {
    values <- rlang::list2(...)

    if (!length(values)) {
        return(.extension_options)
    }

    c(
        values,
        .extension_options
    )
}


compose_bundle <- function(
    extensions,
    options = list(TRUE) %>% purrr::set_names(option_name),
    option_name
) {
    list(
        extensions = extensions,
        options = options
    )
}

#' Transfer explicit DT options
#'
#' @param .options
#' @param options_name
#'
#' @return
transfer_options <- function(
    options,
    options_inner = list(),
    options_name
) {
    if (length(.options)) {
        options[[option_name]] <- options_inner
    }

    options
}

has_names <- function(x) {
    x %>% purrr::map(function(.x) {
        if (inherits(.x, "list") && (.x %>% names() %>% is.null())) {
            has_names(.x)
        } else {
            .x %>% names() %>% purrr::negate(is.null)()
        }
    })
}
