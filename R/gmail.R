suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(tidyverse))
gm_auth_configure(path = "/Users/meerapatel/Lockbox/credentials.json")


gmailr::gm_labels() %>%
        purrr::map(function(x) x %>%
                                purrr::map(function(y) y %>%
                                                   purrr::map(unlist)))

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
        rubix::filter_for(filter_col = label,
                          inclusion_vector = unique_parent_labels) %>%
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

label_map <-
        dplyr::bind_rows(long_labels,
                         long_labels_b)

labelCross <-
        function(...) {

                Args <- rlang::list2(...)
                Args <- unlist(Args)

                searchGmailQA <-
                        Args[!(Args %in% label_map$label)]

                if (length(searchGmailQA)) {
                        warning("labels not recognized: ", paste(searchGmailQA, collapse = ", "))
                }

                label_ids <-
                label_map %>%
                        rubix::filter_for(filter_col = label,
                                          inclusion_vector = Args) %>%
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




label_id <-
lapply(gm_labels()[[1]], function(x) tibble(id = x$id,
                                            name = x$name)) %>%
        dplyr::bind_rows() %>%
        arrange(name) %>%
                dplyr::filter(name == "News/Shopping") %>%
                dplyr::select(id) %>%
                unlist()

if (!is.null(gm_messages(label_ids = label_id)[[1]]$messages)) {
        thread_ids <- (gm_messages(label_ids = label_id)[[1]]$messages %>%
                               purrr::transpose())$threadId %>%
                unlist()
        for (i in 1:length(thread_ids)) {
                gmail_thread <- gm_thread(thread_ids[i])

                thread_date <-
                        lapply(gmail_thread$messages[[1]][7]$payload$headers, function(x) tibble(name = x$name,
                                                                                                 value = x$value)
                        ) %>%
                        dplyr::bind_rows() %>%
                        dplyr::filter(name == "Date") %>%
                        dplyr::select(value) %>%
                        unlist()

                if ((Sys.time() - lubridate::dmy_hms(thread_date) > 3)) {
                        gm_delete_thread(thread_ids[i])
                }
        }
}


label_id <-
        lapply(gm_labels()[[1]], function(x) tibble(id = x$id,
                                                    name = x$name)) %>%
        dplyr::bind_rows() %>%
        arrange(name) %>%
        dplyr::filter(name == "News/Travel") %>%
        dplyr::select(id) %>%
        unlist()

if (!is.null(gm_messages(label_ids = label_id)[[1]]$messages)) {
        thread_ids <- (gm_messages(label_ids = label_id)[[1]]$messages %>%
                               purrr::transpose())$threadId %>%
                unlist()

        for (i in 1:length(thread_ids)) {
                test <- gm_thread(thread_ids[i])

                secret_flying <-
                        lapply(test$messages[[1]][7]$payload$headers, function(x) tibble(name = x$name,
                                                                                         value = x$value)
                        ) %>%
                        dplyr::bind_rows() %>%
                        dplyr::filter(name == "From") %>%
                        dplyr::filter(value == "Secret Flying <newsletter@secretflying.com>")

                if (nrow(secret_flying) == 1) {
                        thread_date <-
                                lapply(test$messages[[1]][7]$payload$headers, function(x) tibble(name = x$name,
                                                                                                 value = x$value)
                                ) %>%
                                dplyr::bind_rows() %>%
                                dplyr::filter(name == "Date") %>%
                                dplyr::select(value) %>%
                                unname() %>%
                                unlist()

                        if ((Sys.time() - lubridate::dmy_hms(thread_date) > 3)) {
                                gm_delete_thread(thread_ids[i])
                        }
                }
        }
}
