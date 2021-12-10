library(scorz)
SpecificParameters <- methods::setClass("SpecificParameters",
                                    contains = "Ready4Module",
                                    slots = c(a_ScorzProfile = "ScorzProfile",
                                              candidate_mdls_chr = "character",
                                              candidate_mdls_lup = "tbl_df",
                                              candidate_mdl_pfcs_chr = "character",
                                              candidate_predrs_chr = "character",
                                              candidate_covars_chr = "character",
                                              depnt_var_nm_1L_chr = "character",
                                              depnt_var_max_val_1L_dbl = "numeric",
                                              folds_1L_int = "integer",
                                              max_mdl_runs_1L_int = "integer",
                                              seed_1L_int = "integer",
                                              predictors_lup = "TTU_predictors_lup"),
                                    prototype =  list(a_ScorzProfile = ScorzProfile(),
                                                      candidate_mdls_chr = NA_character_,
                                                      candidate_mdls_lup = tibble::tibble(),
                                                      candidate_mdl_pfcs_chr = NA_character_,
                                                      candidate_predrs_chr = NA_character_,
                                                      candidate_covars_chr = NA_character_,
                                                      depnt_var_nm_1L_chr = NA_character_,
                                                      depnt_var_max_val_1L_dbl = Inf,
                                                      folds_1L_int = NA_integer_,
                                                      max_mdl_runs_1L_int = NA_integer_,
                                                      seed_1L_int = 12345L,
                                                      predictors_lup = TTU::TTU_predictors_lup()))

methods::setMethod("exhibit",
                   methods::className("SpecificParameters"#, package = "ready4use"
                   ),
                   exhibit_SpecificParameters)
x <- ready4use::Ready4useRepos(dv_nm_1L_chr = "fakes",
                               dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/W95KED",
                               dv_server_1L_chr = "dataverse.harvard.edu") %>%
  ingest(fls_to_ingest_chr = "ymh_clinical_dyad_r4",
         metadata_1L_lgl = F)
y <- youthvars::YouthvarsSeries(a_Ready4useDyad = x,
                                id_var_nm_1L_chr = "fkClientID",
                                timepoint_var_nm_1L_chr = "round",
                                timepoint_vals_chr = levels(x@ds_tb$round))
z <- ScorzAqol6Adol(a_YouthvarsProfile = y)
z <- renew(z)
a <- SpecificParameters(a_ScorzProfile = z,
                                   candidate_predrs_chr = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
                                   depnt_var_nm_1L_chr = "aqol6d_total_w",
                                   depnt_var_max_val_1L_dbl = Inf)
exhibit(a,
        type_1L_chr = "correlation" # captions_chr = NULL or ....
        )
# y <- ratify(y,
#             type_1L_chr = "two_timepoints")
# y <- renewSlot(y,
#                profiled_vars_ls = list(temporal = c("d_age","d_sexual_ori_s","d_ATSI","d_studying_working","d_relation_s"),
#                                        temporal_tested = c("k6_total", "phq9_total", "bads_total", "gad7_total"),
#                                        participation_tested = c("k6_total", "phq9_total", "bads_total", "gad7_total")
#                ),
#                slot_nm_1L_chr = "descriptives_ls")
# y <- renew(y,
#            type_1L_chr = "characterize")
# z <- scorz::ScorzAqol6Adol(a_YouthvarsProfile = y)
# z <- renew(z)
