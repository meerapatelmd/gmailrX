#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[gmailr]{gm_labels}}
#'  \code{\link[purrr]{transpose}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{distinct}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{bind}}
#'  \code{\link[tidyr]{extract}},\code{\link[tidyr]{pivot_longer}}
#'  \code{\link[rubix]{filter_for}}
#' @export
#' @importFrom gmailr gm_labels
#' @importFrom purrr transpose
#' @importFrom tibble tibble
#' @importFrom dplyr distinct mutate select filter filter_at left_join bind_rows
#' @importFrom tidyr extract pivot_longer
#' @importFrom rubix filter_for
#' @importFrom magrittr %>%


get_standard_labels <-
        function() {

                label_map <- get_label_map()

                gmail_labels <-
                        gmailr::gm_labels()$labels

                gmail_labels_id <-
                        gmail_labels %>%
                        purrr::transpose()
                gmail_labels_id <- gmail_labels_id$id

                gmail_labels_name <-
                        gmail_labels %>%
                        purrr::transpose()
                gmail_labels_name <- gmail_labels_name$name

                tibble::tibble(label_id = unlist(gmail_labels_id),
                                       label = unlist(gmail_labels_name)) %>%
                        rubix::filter_for(label_id,
                                          inclusion_vector = label_map$label_id,
                                          invert = TRUE)

        }
