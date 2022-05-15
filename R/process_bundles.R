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
        if ( !is.null(nms) &&
                (nms %in% c("extensions", "options") %>% any()) ) {
            bundles <- bundles %>% list()
        }

        if ( bundles %>%
                purrr::map_lgl(
                    ~names(.x) %in%
                        c("extensions", "options") %>%
                        any()
                ) %>%
                any()
        ) {
            return(bundles %>% dt_process_bundles_list())
        } else {
            msg <- "Invalid value for 'bundles'. Expecting (list of) named list or character vec"
            msg %>% logger::log_error()
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

    # merge <- function(x) {
    #     if (length(x) > 1) {
    #         x_ <- confx::conf_merge(x[[1]], x[[2]])
    #         x[[2]] <- x_
    #         Recall(x[-1])
    #     } else {
    #         x
    #     }
    # }

    # postprocess <- function(.x) {
    #     .x %>% purrr::imap(function(.opt, .name) {
    #         if (.name == "dom") {
    #             .opt[which.max(nchar(.opt))]
    #         } else {
    #             if (!is.list(.opt)) {
    #                 unique(.opt)
    #             } else {
    #                 postprocess(.opt)
    #             }
    #         }
    #     })
    # }

    bundle_values %>%
        # Merge
        dt_process_bundles_merge() %>%
        # Flatten
        purrr::flatten() %>%
        # Postprocess
        purrr::imap(function(.x, .y) {
            if (!is.list(.x)) {
                return(.x)
            }

            .x %>% dt_process_bundles_postprocess()
        })
}

dt_process_bundles_list <- function(
    bundles = list(extensions = NULL, options = NULL),
    ...
) {
    extensions <- bundles %>% purrr::map("extensions")
    options <- bundles %>% purrr::map("options")

    # Merge extensions
    extensions <- extensions %>% unlist() %>% list(.) %>%
        purrr::set_names("extensions")

    # Merge options
    options <- options %>%
        # Merge
        dt_process_bundles_list_merge() %>%
        # Flatten
        purrr::flatten() %>%
        # Postprocess
        dt_process_bundles_list_postprocess() %>%
        list(.) %>%
        purrr::set_names("options")

    # Combine
    c(extensions, options)
}

# Utils -------------------------------------------------------------------

dt_process_bundles_merge <- function(x) {
    if (length(x) > 1) {
        x_ <- confx::conf_merge(x[[1]], x[[2]])
        x[[2]] <- x_
        Recall(x[-1])
    } else {
        x
    }
}

dt_process_bundles_postprocess <- function(.x) {
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

#' Align multiple `dom` inputs
#'
#' @param x
#'
#' @return
#'
#' @examples
dt_process_bundles_list_postprocess_align_dom <- function(x) {
    tmp <- x %>%
        stringr::str_split("") %>%
        unlist() %>%
        unique()

    c(
        tmp %>% stringr::str_subset("[A-Z]"), #%>% sort(),
        tmp %>% stringr::str_subset("[a-z]") #%>% sort()
    ) %>%
        stringr::str_c(collapse = "")
}

dt_process_bundles_list_postprocess <- function(.x) {
    .x %>% purrr::imap(function(.opt, .name) {
        if (.name == "dom") {
            # .opt[which.max(nchar(.opt))]
            .opt %>% dt_process_bundles_list_postprocess_align_dom()
        } else {
            if (!is.list(.opt)) {
                unique(.opt)
            } else {
                dt_process_bundles_list_postprocess(.opt)
            }
        }
    })
}

dt_process_bundles_list_merge <- function(x) {
    if (length(x) > 1) {
        x_ <- confx::conf_merge(x[[1]], x[[2]])
        x[[2]] <- x_
        Recall(x[-1])
    } else {
        x
    }
}
