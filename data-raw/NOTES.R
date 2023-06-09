library(ready4)
library(ready4use)
library(ready4fun)
X <- ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                               gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
abbreviations_lup <- procure(procureSlot(Y,
                                         "b_Ready4useIngest"),
                             "abbreviations_lup")
# Y@b_Ready4useIngest@objects_ls$treat_as_words_chr <- c(Y@b_Ready4useIngest@objects_ls$treat_as_words_chr,
#                                                        "blog") %>% sort()
get_abbrs("condition",abbreviations_lup) # from ready4fun
# abbreviations_lup <- ready4fun::renew.ready4fun_abbreviations(abbreviations_lup,
#                                                               short_name_chr = c("org","orgs"),
#                                                               long_name_chr = c("organisation","organisations"),
#                                                               plural_lgl = c(F,T))
# abbreviations_lup <- abbreviations_lup %>%
#   dplyr::arrange(short_name_chr)
# abbreviations_lup <-  abbreviations_lup %>%
#   dplyr::mutate(short_name_chr = dplyr::case_when(short_name_chr == "RI" ~ "ri",
#                                                   short_name_chr == "RIs" ~ "ris",
#                                                   T ~ short_name_chr)) %>%
#   dplyr::mutate(long_name_chr = dplyr::case_when(long_name_chr == "Rand Indexs" ~ "Rand Indices",
#                                                  T ~ long_name_chr))
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(abbreviations_lup = abbreviations_lup,
                                                              treat_as_words_chr = Y@b_Ready4useIngest@objects_ls$treat_as_words_chr)),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")


