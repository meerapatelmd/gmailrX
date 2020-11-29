#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[rubix]{filter_for}},\code{\link[rubix]{map_names_set}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{distinct}}
#'  \code{\link[gmailr]{gm_threads}}
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}},\code{\link[purrr]{transpose}},\code{\link[purrr]{reduce}}
#'  \code{\link[tibble]{as_tibble}}
#' @rdname search_labels
#' @export
#' @importFrom rlang list2
#' @importFrom rubix filter_for map_names_set
#' @importFrom dplyr select distinct
#' @importFrom gmailr gm_threads
#' @importFrom purrr pluck map transpose reduce
#' @importFrom tibble as_tibble_col
#' @importFrom magrittr %>%


search_labels <-
        function(...) {

                label_map <- get_label_map()

                Args <- rlang::list2(...)
                Args <- unlist(Args)

                searchGmailQA <-
                        Args[!(Args %in% label_map$label)]

                if (length(searchGmailQA)) {
                        warning("labels not recognized: ", paste(searchGmailQA, collapse = ", "))
                }

                label_ids <-
                        label_map %>%
                        rubix::filter_for(col = label,
                                          vector = Args) %>%
                        dplyr::select(label_id, label) %>%
                        dplyr::distinct()

                output <- list()
                for (i in 1:nrow(label_ids)) {
                        label_id <- label_ids$label_id[i]
                        label <- label_ids$label[i]

                        output[[i]] <-
                                gmailr::gm_threads(label_ids = label_id)

                        names(output)[i] <- label

                }


                output %>%
                        rubix::map_names_set(function(x) purrr::pluck(x, 1, "threads")) %>%
                        purrr::map(function(x) purrr::transpose(x)) %>%
                        purrr::map(function(x) purrr::pluck(x, "id")) %>%
                        purrr::map(unlist) %>%
                        purrr::map(tibble::as_tibble_col, "thread_id") %>%
                        purrr::reduce(inner_join, by = "thread_id")

        }
