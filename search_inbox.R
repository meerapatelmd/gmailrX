






search_inbox <-
        function() {
                output <- search_threads(labels = "INBOX")
                output <- output$INBOX

                thread_ids <-
                purrr::pluck(output,
                             1,
                             "threads") %>%
                        purrr::transpose() %>%
                        purrr::pluck("id")

                output <-
                thread_ids %>%
                        purrr::map(gmailr::gm_thread)

                inbox <-
                output %>%
                        purrr::map(~purrr::pluck(.,
                                                 "messages",
                                                 1,
                                                 "labelIds")) %>%
                        purrr::map(unlist) %>%
                        purrr::set_names(thread_ids) %>%
                        purrr::map(as_tibble_col, "Label") %>%
                        purrr::map(function(x) x %>%
                                                dplyr::mutate(PRESENT = TRUE)) %>%
                        purrr::map(function(x) x %>%
                                                tidyr::pivot_wider(names_from = Label,
                                                                   values_from = PRESENT)) %>%
                        dplyr::bind_rows(.id = "threadId") %>%
                        dplyr::select(!starts_with("Label"),
                                      starts_with("Label"))

                unlabeled <-
                        inbox %>%
                        dplyr::filter_at(vars(starts_with("Label")),
                                         all_vars(is.na(.)))

                output_a <-
                unlabeled$threadId %>%
                        rubix::map_names_set(gmailr::gm_thread) %>%
                        purrr::map(~purrr::pluck(., "messages", 1))

                for (i in 1:length(output_a)) {

                        secretary::typewrite_bold(names(output_a)[i])
                        secretary::typewrite(output_a[[i]])

                        secretary::press_enter()

                }
                purrr::pluck(output[[1]]$messages %>% str()
        }
