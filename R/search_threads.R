




gmailr::gm_auth_configure(path = "/Users/meerapatel/Lockbox/credentials.json")

search_threads <-
        function(search = NULL,
                 num_results = NULL,
                 page_token = NULL,
                 labels = NULL,
                 include_spam_trash = NULL,
                 user_id = "me")  {

                label_map <- get_label_map()


                if (!is.null(labels)) {

                                Args <- labels

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

                                label_ids$label_id %>%
                                        purrr::map(~gmailr::gm_threads(
                                                search = search,
                                                num_results = num_results,
                                                page_token = page_token,
                                                include_spam_trash = include_spam_trash,
                                                user_id = user_id,
                                                label_ids = .)) %>%
                                        purrr::set_names(label_ids$label)

                } else {

                        gmailr::gm_threads(
                                search = search,
                                num_results = num_results,
                                page_token = page_token,
                                include_spam_trash = include_spam_trash,
                                user_id = user_id)


                }

        }
