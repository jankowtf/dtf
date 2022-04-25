#' Apply DT bundle configuration
#'
#' @param data [?]
#' @param verbose [[logical]] Print args for call to [DT::datatable]
#' @param bundles [?]
#'
#' @return
#' @export
datatable2 <- function(
    data,
    bundles = character(),
    verbose = FALSE,
    ...
) {
    # Get args from bundles
    args <- bundles %>% dt_process_bundles()

    # Tracing
    if (verbose) {
        print(args)
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

# Prepare -----------------------------------------------------------------

#' Process DT config bundle
#'
#' @param bundle
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dt_process_bundle <- function(bundle = character(), ...) {
    # Early exit for evaluated bundle fn input
    if (is.list(bundle)) {
        return(bundle)
    }

    # Function dispatch for bundle
    fn <- if (!length(bundle)) {
        "dt_bundle_default"
    } else {
        "dt_bundle_{tolower(bundle)}"
    }

    # Get bundle values
    bundle_values <- fn %>%
        stringr::str_glue() %>%
        rlang::call2(.) %>%
        rlang::eval_tidy()
}

#' Prepare DT bundle
#'
#' @param bundles ([character] or [list]) Combined specification of `extensions`
#'   and `options` as expected by [DT::datatable()].
#' @param ... [?]
#'
#' @return
#' @export
dt_process_bundles <- function(bundles = character(), ...) {
    if (inherits(bundles, "list")) {
        nms <- bundles %>% names()
        if ( !is.null(nms) && (nms %in% c("extensions", "options") %>% all()) ) {
            bundles <- bundles %>% list()
        }

        if ( bundles %>%
                purrr::map_lgl(
                    ~names(.x) %in%
                        c("extensions", "options") %>%
                        all()
                ) %>%
                all()
        ) {
            return(bundles %>% dt_process_bundles_list())
        } else {
            msg <- "Invalid value for 'bundles'. Expecting (list of) named list or character vec"
            mgs %>% logger::log_error()
            msg %>% stop()
        }
    }

    bundle_values <- bundles %>%
        # purrr::map(~.x %>% dt_process_bundle)
        purrr::map(function(.x) {
            if (is.character(.x)) {
                .x %>% dt_process_bundle()
            } else {
                .x
            }
        })

    merge <- function(x) {
        if (length(x) > 1) {
            x_ <- confx::conf_merge(x[[1]], x[[2]])
            x[[2]] <- x_
            Recall(x[-1])
        } else {
            x
        }
    }

    postprocess <- function(.x) {
        .x %>% purrr::imap(function(.opt, .name) {
            if (.name == "dom") {
                .opt[which.max(nchar(.opt))]
            } else {
                if (!is.list(.opt)) {
                    unique(.opt)
                } else {
                    postprocess(.opt)
                }
            }
        })
    }

    bundle_values %>%
        # Merge
        merge() %>%
        # Flatten
        purrr::flatten() %>%
        # Postprocess
        purrr::imap(function(.x, .y) {
            if (!is.list(.x)) {
                return(.x)
            }

            .x %>% postprocess()
        })
}

dt_process_bundles_list <- function(
    bundles = list(extensions = NULL, options = NULL),
    ...
) {
    extensions <- bundles %>% purrr::map("extensions")
    options <- bundles %>% purrr::map("options")

    merge <- function(x) {
        if (length(x) > 1) {
            x_ <- confx::conf_merge(x[[1]], x[[2]])
            x[[2]] <- x_
            Recall(x[-1])
        } else {
            x
        }
    }

    postprocess <- function(.x) {
        .x %>% purrr::imap(function(.opt, .name) {
            if (.name == "dom") {
                .opt[which.max(nchar(.opt))]
            } else {
                if (!is.list(.opt)) {
                    unique(.opt)
                } else {
                    postprocess(.opt)
                }
            }
        })
    }

    # Merge extensions
    extensions <- extensions %>% unlist() %>% list(.) %>%
        purrr::set_names("extensions")

    # Merge options
    options <- options %>%
        # Merge
        merge() %>%
        # Flatten
        purrr::flatten() %>%
        # Postprocess
        purrr::imap(function(.x, .y) {
            if (!is.list(.x)) {
                return(.x)
            }

            .x %>% postprocess()
        }) %>%
        list(.) %>%
        purrr::set_names("options")

    # Combine
    c(extensions, options)
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

# Lookup column positions -------------------------------------------------

#' Lookup column positions
#'
#' @param columns ([character] but also handles [integer] gracefully) Column
#'   names (or positions)
#' @param data ([tibble::tibble]) Data in which to look for column names
#' @param reverse [[logical]] Reverse column names yes/no. Necessary for
#'   "right-hand" logic
#' @param no_offset
#'
#' @return ([integer])
#'
#' @examples
#' lookup_column_positions("cyl", mtcars)
#' lookup_column_positions("carb", mtcars, reverse = TRUE)
#' lookup_column_positions("cyl", mtcars, no_offset = TRUE)
lookup_column_positions <- function(
    columns,
    data,
    reverse = FALSE,
    no_offset = FALSE
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
        no_offset <- TRUE
        rev(names)
    } else {
        names
    }

    # DT treats rownames as separate column -> work with offset
    offset <- ifelse(no_offset, 0, ifelse(is.null(data %>% rownames()), 0, 1))

    # index_left <- which(names == left)
    index_left <- columns %>% purrr::map(~which(names == .x)) %>%
        unlist()

    as.integer(
        if (length(index_left)) {
            index_left + offset
        } else {
            columns
        }
    )
}
