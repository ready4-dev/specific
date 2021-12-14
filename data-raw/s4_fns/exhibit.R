exhibit_SpecificParameters <- function(x,
                                       captions_chr = character(0),
                                       method_chr = c("pearson", "spearman"),
                                       mkdn_tbl_refs_chr = NULL,
                                       output_type_1L_chr = "HTML",
                                       type_1L_chr = "correlation",
                                       timepoints_int = NA_integer_){
  if(type_1L_chr == "correlation"){
    if(is.na(timepoints_int)){
      if("timepoint_vals_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile)){
        timepoints_int <- 1:length(x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr) %>% as.integer()
      }else{
        timepoints_int <- 1
      }
    }
    if(identical(character(0), captions_chr)){
      captions_chr <- paste0("Correlations at ",
                             x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int])
    }
    1:length(timepoints_int) %>%
      purrr::map(~
                   youthvars::transform_ds_for_tstng(x@a_ScorzProfile@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                     depnt_var_nm_1L_chr = x@depnt_var_nm_1L_chr,
                                                     depnt_var_max_val_1L_dbl = x@depnt_var_max_val_1L_dbl,
                                                     candidate_predrs_chr = x@candidate_predrs_chr,
                                                     round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile),
                                                                                  x@a_ScorzProfile@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                                                                                  NA_character_),
                                                     round_val_1L_chr = ifelse("timepoint_vals_chr" %in% slotNames(x@a_ScorzProfile@a_YouthvarsProfile),
                                                                               x@a_ScorzProfile@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int[.x]],
                                                                               NA_character_))  %>%
                   youthvars::make_corstars_tbl_xx(caption_1L_chr = captions_chr[.x],
                                                   mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[.x],
                                                   method_chr = method_chr,
                                                   result_chr = output_type_1L_chr
                   ))
  }
}