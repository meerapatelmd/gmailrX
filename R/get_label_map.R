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


get_label_map <-
        function() {
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

                gmail_labels_map <-
                        tibble::tibble(label_id = unlist(gmail_labels_id),
                                       label = unlist(gmail_labels_name)) %>%
                        dplyr::distinct() %>%
                        tidyr::extract(label,
                                       into = c("parent_label", "child_label"),
                                       regex = "(^.*?)[/]{1}(.*$)",
                                       remove = FALSE) %>%
                        tidyr::extract(col = child_label,
                                       into = c("child_label2", "grandchild_label"),
                                       regex = "(^.*)[/]{1}(.*$)",
                                       remove = FALSE) %>%
                        dplyr::mutate(child_label = coalesce(child_label2, child_label)) %>%
                        dplyr::select(-child_label2) %>%
                        dplyr::distinct()



                unique_parent_labels <- gmail_labels_map %>%
                        dplyr::select(parent_label) %>%
                        dplyr::filter(!is.na(parent_label)) %>%
                        dplyr::distinct() %>%
                        unlist() %>%
                        unname()

                parent_label_map <-
                        gmail_labels_map %>%
                        rubix::filter_for(col = label,
                                          vector = unique_parent_labels) %>%
                        dplyr::filter_at(vars(parent_label,
                                              child_label,
                                              grandchild_label),
                                         all_vars(is.na(.))) %>%
                        dplyr::select(parent_label_id = label_id,
                                      parent_label = label)


                child_labels <-
                        dplyr::left_join(parent_label_map,
                                         gmail_labels_map,
                                         by = "parent_label") %>%
                        dplyr::select(label_path = label,
                                      parent_label_id,
                                      parent_label,
                                      child_label_id = label_id,
                                      child_label,
                                      grandchild_label)


                long_labels <-
                        child_labels %>%
                        tidyr::pivot_longer(cols = contains("label_id"),
                                            names_to = c("label_id_type"),
                                            values_to = "label_id",
                                            names_pattern = "(^.*?)_label_id",
                                            values_drop_na = TRUE) %>%
                        tidyr::pivot_longer(cols = ends_with("label"),
                                            names_to = "label_type",
                                            values_to = "label",
                                            names_pattern = "(^.*?)_label",
                                            values_drop_na = TRUE) %>%
                        dplyr::filter(label_id_type == label_type) %>%
                        dplyr::select(label_path,
                                      label_type,
                                      label_id,
                                      label) %>%
                        dplyr::distinct()

                long_labels_b <-
                        long_labels %>%
                        dplyr::filter(label_type == "child") %>%
                        dplyr::mutate(label = label_path) %>%
                        dplyr::mutate(label_type = "path") %>%
                        dplyr::distinct()


                output_part_1 <-
                dplyr::bind_rows(long_labels,
                                 long_labels_b)

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

                output_part_2 <-
                tibble::tibble(label_id = unlist(gmail_labels_id),
                               label = unlist(gmail_labels_name)) %>%
                        rubix::filter_for(col = label_id,
                                          vector = label_map$label_id,
                                          invert = TRUE) %>%
                        dplyr::transmute(label_path = label,
                                         label_type = "standard",
                                         label_id,
                                         label)

                dplyr::bind_rows(output_part_2,
                                 output_part_1)

        }
