#' Title
#'
#' @param ...
#' @param .option_name
#' @param .extension_options
#' @param .unlist
#' @param .flatten
#'
#' @return
#' @importFrom drop drop_null drop_empty
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

#' Title
#'
#' @param extensions
#' @param options
#' @param selection
#' @param option_name
#'
#' @return
#' @importFrom drop drop_null
#'
#' @examples
compose_bundle <- function(
    extensions,
    options = list(TRUE) %>% purrr::set_names(option_name),
    selection = NULL,
    option_name
) {
    list(
        extensions = extensions,
        options = options,
        selection = selection
    ) %>%
        drop::drop_null()
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

# Lookup column positions -------------------------------------------------

#' Lookup column positions
#'
#' @param columns ([character] but also handles [integer] gracefully) Column
#'   names (or positions)
#' @param data ([tibble::tibble]) Data in which to look for column names
#' @param reverse [[logical]] Reverse column names yes/no. Necessary for
#'   "right-hand" logic
#' @param offset [[logical]]
#' @param negate [[logical]]
#'
#' @return ([integer])
#'
#' @examples
#' lookup_column_positions("cyl", mtcars)
#' lookup_column_positions("cyl", mtcars, offset = FALSE)
#' lookup_column_positions("carb", mtcars, reverse = TRUE)
lookup_column_positions <- function(
    columns,
    data,
    reverse = FALSE,
    offset = TRUE,
    negate = FALSE
) {
    # Early exit for bad combination
    if (!nrow(data) && inherits(columns, "character")) {
        stop("Bad combination: column names but no data")
    }

    # Early exit for integer values
    if (!nrow(data)) {
        return(columns)
    }

    names <- names(data)

    names <- if (reverse) {
        offset <- FALSE
        rev(names)
    } else {
        names
    }

    # DT treats rownames as separate column -> work with offset
    offset <- ifelse(!offset, 0, ifelse(is.null(data %>% rownames()), 0, 1))

    # index_left <- which(names == left)
    index <- columns %>% purrr::map(~which(names == .x)) %>%
        unlist()

    index_full <- 1:length(names)


    out <- as.integer(
        if (length(index)) {
            index <- if (negate) {
                index_full[!(index_full %in% index)]
            } else {
                index
            }
            index + offset
        } else {
            columns
        }
    )
}
