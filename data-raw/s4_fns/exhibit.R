exhibit_SpecificProject <- function(x,
                                    captions_chr = character(0),
                                    method_chr = c("pearson", "spearman"),
                                    mkdn_tbl_refs_chr = NULL,
                                    output_type_1L_chr = "HTML",
                                    timepoints_int = NA_integer_,
                                    type_1L_chr = "dataset",
                                    what_1L_chr = "correlation",
                                    ...
                                    ){
  if(type_1L_chr == "dataset" & what_1L_chr == "correlation"){
    if(is.na(timepoints_int)){
      if("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile)){
        timepoints_int <- 1:length(x@a_YouthvarsProfile@timepoint_vals_chr) %>% as.integer()
      }else{
        timepoints_int <- 1
      }
    }
    if(identical(character(0), captions_chr)){
      captions_chr <- paste0("Correlations",
                             ifelse("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile),
                                    paste0(" at ",x@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int]),
                                    "")
                             )
    }
    1:length(timepoints_int) %>%
      purrr::map(~
                   youthvars::transform_ds_for_tstng(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                     depnt_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr,
                                                     depnt_var_max_val_1L_dbl = x@b_SpecificParameters@depnt_var_max_val_1L_dbl,
                                                     candidate_predrs_chr = x@b_SpecificParameters@candidate_predrs_chr,
                                                     round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% slotNames(x@a_YouthvarsProfile),
                                                                                  x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                                                                                  NA_character_),
                                                     round_val_1L_chr = ifelse("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile),
                                                                               x@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int[.x]],
                                                                               NA_character_))  %>%
                   youthvars::make_corstars_tbl_xx(caption_1L_chr = captions_chr[.x],
                                                   mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[.x],
                                                   method_chr = method_chr,
                                                   result_chr = output_type_1L_chr
                   ))
  }else{
    heading_grps_chr <- NULL
    if(!identical(what_1L_chr,""))
      object_xx <- procure(x,
                           what_1L_chr = what_1L_chr)
    if(type_1L_chr == "results"){
      if(what_1L_chr == "mdl_cmprsn"){
        heading_grps_chr <- c(1,3,3) %>% stats::setNames(" ",
                                                         paste0("Training model fit (averaged over ",
                                                                x@b_SpecificParameters@folds_1L_int,
                                                                " folds)"),
                                                         paste0("Testing model fit (averaged over ",
                                                                x@b_SpecificParameters@folds_1L_int,
                                                                " folds)"))
        if(identical(captions_chr,character(0)))
          captions_chr <- "Comparison of candidate models using highest correlated predictor"
      }
      if(what_1L_chr == "predr_cmprsn"){
        if(identical(captions_chr,character(0)))
          captions_chr <- "Comparison of all candidate predictors using preferred model"
      }

    }
    object_xx %>%
      ready4show::print_table(output_type_1L_chr = output_type_1L_chr,
                              caption_1L_chr = captions_chr,
                              heading_grps_chr = heading_grps_chr,
                              mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr,
                              ...)
  }
}
