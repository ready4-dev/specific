manufacture_SpecificProject <- function(x,
                                        what_1L_chr = "ds_descvs_ls",
                                        scndry_anlys_params_ls = NULL,
                                        ...){
  series_1L_lgl <- x@a_YouthvarsProfile %>% inherits("YouthvarsSeries")
  if(what_1L_chr %in% c("ds_descvs_ls","ds_smry_ls","input_params_ls")){
    ds_descvs_ls <- make_ds_descvs_ls(candidate_predrs_chr =  x@b_SpecificParameters@candidate_predrs_chr,
                                      candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr,
                                      cohort_descv_var_nms_chr = x@b_SpecificParameters@descv_var_nms_chr,
                                      dictionary_tb = x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3,
                                      id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                      is_fake_1L_lgl = x@b_SpecificParameters@fake_1L_lgl,
                                      msrmnt_date_var_nm_1L_chr = if(!series_1L_lgl){character(0)}else{x@b_SpecificParameters@msrmnt_date_var_nm_1L_chr},
                                      round_var_nm_1L_chr = if(!series_1L_lgl){character(0)}else{x@a_YouthvarsProfile@timepoint_var_nm_1L_chr},
                                      round_vals_chr = if(!series_1L_lgl){"Overall"}else{x@a_YouthvarsProfile@timepoint_vals_chr},
                                      utl_wtd_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr,
                                      maui_item_pfx_1L_chr = x@b_SpecificParameters@itm_prefix_1L_chr,
                                      utl_unwtd_var_nm_1L_chr = x@b_SpecificParameters@total_unwtd_var_nm_1L_chr)
    ds_descvs_ls$nbr_obs_in_raw_ds_1L_dbl <- nrow(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb)
    ds_descvs_ls$nbr_participants_1L_int <- length(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb %>%
                                                     dplyr::pull(ds_descvs_ls$id_var_nm_1L_chr) %>%
                                                     unique())
    object_xx <- ds_descvs_ls
  }
  if(what_1L_chr %in% c("ds_smry_ls","input_params_ls")){
    ds_smry_ls <- ds_descvs_ls %>%
      make_analysis_ds_smry_ls(candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr,
                               predictors_lup = x@b_SpecificParameters@predictors_lup)
    object_xx <- ds_smry_ls
  }
  if(what_1L_chr %in% c("mdl_smry_ls","input_params_ls")){
    if(is.na(x@b_SpecificParameters@candidate_mdls_chr)){
      mdl_types_chr <- NULL
    }else{
      mdl_types_chr <- x@b_SpecificParameters@candidate_mdls_chr
    }
    if(is.na(x@b_SpecificParameters@candidate_mdl_pfcs_chr)){
      choose_from_pfx_chr <- NULL
    }else{
      choose_from_pfx_chr <- x@b_SpecificParameters@candidate_mdl_pfcs_chr
    }
    mdl_smry_ls <- make_mdl_smry_ls(mdl_types_lup = x@b_SpecificParameters@candidate_mdls_lup,
                                    mdl_types_chr = mdl_types_chr,
                                    choose_from_pfx_chr = choose_from_pfx_chr,
                                    folds_1L_int = x@b_SpecificParameters@folds_1L_int,
                                    max_nbr_of_boruta_mdl_runs_int = x@b_SpecificParameters@max_mdl_runs_1L_int)
    object_xx <- mdl_smry_ls
  }
  if(what_1L_chr == "input_params_ls"){
    y <- SpecificSynopsis()
    header_yaml_args_ls <- ready4show::make_header_yaml_args_ls(authors_tb = y@authors_r3,
                                                                institutes_tb = y@institutes_r3,
                                                                title_1L_chr = y@title_1L_chr,
                                                                keywords_chr = y@keywords_chr)
    maui_params_ls <- make_maui_params_ls(maui_domains_pfxs_1L_chr = x@b_SpecificParameters@itm_prefix_1L_chr,
                                          maui_itm_short_nms_chr = x@b_SpecificParameters@itm_labels_chr,
                                          maui_scoring_fn = NULL)
    output_format_ls <- ready4show::make_output_format_ls(manuscript_outp_1L_chr = y@outp_formats_chr[1],
                                                          manuscript_digits_1L_int = y@digits_int[1],
                                                          supplementary_outp_1L_chr = ifelse(length(y@outp_formats_chr)>1,y@outp_formats_chr[2],y@outp_formats_chr[1]),
                                                          supplementary_digits_1L_int = ifelse(length(y@digits_int)>1,y@digits_int[2],y@digits_int[1]))
    if(is.null(procure(x,
                       what = "prefd_covars"))){
      prefd_covars_chr <- NA_character_
    }else{
      prefd_covars_chr <- procure(x,
                                  what = "prefd_covars")
    }
    object_xx <- make_input_params(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                   control_ls = NULL,
                                   ds_descvs_ls = ds_descvs_ls,
                                   dv_ds_nm_and_url_chr = NULL,
                                   header_yaml_args_ls = header_yaml_args_ls,
                                   maui_params_ls = maui_params_ls,
                                   output_format_ls = output_format_ls,
                                   predictors_lup = x@b_SpecificParameters@predictors_lup,
                                   prefd_covars_chr = prefd_covars_chr,
                                   prefd_mdl_types_chr = procure(x, what = "prefd_mdls"),
                                   scndry_anlys_params_ls = scndry_anlys_params_ls,
                                   write_new_dir_1L_lgl = F)
  }
  return(object_xx)
}
manufacture_SpecificSynopsis <- function(x,
                                         consent_1L_chr = "",
                                         depnt_var_min_val_1L_dbl = numeric(0),
                                         depnt_var_nms_chr = NA_character_,
                                         make_cmpst_plt_1L_lgl = F,
                                         scndry_anlys_params_ls = NULL,
                                         version_1L_chr = "",
                                         what_1L_chr = "input_params_ls",
                                         ...){
  if(what_1L_chr %in% c("abstract_args_ls","ds_descvs_ls","ds_smry_ls","input_params_ls","results_ls","mdl_smry_ls")){
    y_SpecificMixed <- SpecificMixed(a_YouthvarsProfile = x@d_YouthvarsProfile,
                                     b_SpecificParameters = x@c_SpecificParameters,
                                     c_SpecificResults = x@b_SpecificResults,
                                     paths_chr = x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls$path_to_write_to_1L_chr)
    if(what_1L_chr %in% c("ds_descvs_ls","ds_smry_ls","mdl_smry_ls")){ # Could add input_params_ls to this logic once corresponding SpecificProject methd is updated
      # Would then need to pass scndry_anlys_params_ls to mthd
      object_xx <- manufacture(y_SpecificMixed,
                               depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                               what_1L_chr = what_1L_chr)
    }
    if(what_1L_chr %in% c("abstract_args_ls","input_params_ls","results_ls")){
      header_yaml_args_ls <- ready4show::make_header_yaml_args_ls(authors_tb = x@authors_r3,
                                                                  institutes_tb = x@institutes_r3,
                                                                  title_1L_chr = x@title_1L_chr,
                                                                  keywords_chr = x@keywords_chr)
      maui_params_ls <- make_maui_params_ls(maui_domains_pfxs_1L_chr = y_SpecificMixed@b_SpecificParameters@itm_prefix_1L_chr,
                                            maui_itm_short_nms_chr = y_SpecificMixed@b_SpecificParameters@itm_labels_chr,
                                            maui_scoring_fn = NULL)
      output_format_ls <- ready4show::make_output_format_ls(manuscript_outp_1L_chr = x@outp_formats_chr[1],
                                                            manuscript_digits_1L_int = x@digits_int[1],
                                                            supplementary_outp_1L_chr = ifelse(length(x@outp_formats_chr)>1,x@outp_formats_chr[2],x@outp_formats_chr[1]),
                                                            supplementary_digits_1L_int = ifelse(length(x@digits_int)>1,x@digits_int[2],x@digits_int[1]))
      object_xx <- make_input_params(y_SpecificMixed@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                     consent_1L_chr = consent_1L_chr,
                                     control_ls = y_SpecificMixed@b_SpecificParameters@control_ls,
                                     ds_descvs_ls = manufacture(y_SpecificMixed, what_1L_chr = "ds_descvs_ls"),
                                     dv_ds_nm_and_url_chr = c(x@e_Ready4useRepos@dv_nm_1L_chr, x@e_Ready4useRepos@dv_ds_nm_1L_chr),
                                     header_yaml_args_ls = header_yaml_args_ls,
                                     maui_params_ls = maui_params_ls,
                                     output_format_ls = output_format_ls,
                                     predictors_lup = y_SpecificMixed@b_SpecificParameters@predictors_lup,
                                     prefd_covars_chr = ifelse(is.null(procure(y_SpecificMixed, what = "prefd_covars")),
                                                               NA_character_,
                                                               procure(y_SpecificMixed, what = "prefd_covars")),
                                     prefd_mdl_types_chr = procure(y_SpecificMixed, what = "prefd_mdls"),
                                     scndry_anlys_params_ls = scndry_anlys_params_ls,
                                     write_new_dir_1L_lgl = F)
      if(is.na(depnt_var_nms_chr[1]))
        depnt_var_nms_chr <- c(y_SpecificMixed@b_SpecificParameters@depnt_var_nm_1L_chr,
                               y_SpecificMixed@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 %>%
                                 ready4::get_from_lup_obj(match_value_xx = y_SpecificMixed@b_SpecificParameters@depnt_var_nm_1L_chr,
                                                          match_var_nm_1L_chr = "var_nm_chr",
                                                          target_var_nm_1L_chr = "var_desc_chr"))
      object_xx$short_and_long_nm <- depnt_var_nms_chr
      object_xx <- object_xx %>%
        make_study_descs_ls(time_btwn_bl_and_fup_1L_chr = x@interval_chr,
                            background_1L_chr = x@background_1L_chr,
                            coi_1L_chr = x@coi_1L_chr,
                            conclusion_1L_chr = x@conclusion_1L_chr,
                            ethics_1L_chr = x@ethics_1L_chr,
                            funding_1L_chr = x@funding_1L_chr,
                            sample_desc_1L_chr = x@sample_desc_1L_chr,
                            var_nm_change_lup = x@correspondences_r3)
      if(what_1L_chr %in% c("abstract_args_ls","results_ls")){
        object_xx$study_descs_ls$predr_ctgs_ls <- make_predr_ctgs_ls(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls)
        object_xx <- make_results_ls(consent_1L_chr = consent_1L_chr,
                                     dv_ds_nm_and_url_chr = object_xx$path_params_ls$dv_ds_nm_and_url_chr,
                                     depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                     make_cmpst_plt_1L_lgl = make_cmpst_plt_1L_lgl,
                                     outp_smry_ls = x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                                     output_format_ls = object_xx$output_format_ls,
                                     params_ls_ls = object_xx,
                                     path_params_ls = list(paths_ls = list(output_data_dir_1L_chr = paste0(x@a_Ready4showPaths@outp_data_dir_1L_chr,"/Output"))),
                                     study_descs_ls = object_xx$study_descs_ls,
                                     var_nm_change_lup = object_xx$study_descs_ls$var_nm_change_lup,
                                     version_1L_chr = version_1L_chr)
        object_xx$abstract_args_ls$abstract_ls$Conclusions <- x@conclusion_1L_chr
        object_xx$study_descs_ls$background_1L_chr <- x@background_1L_chr
        object_xx$study_descs_ls$conclusion_1L_chr <- x@conclusion_1L_chr
      }
      if(what_1L_chr == "abstract_args_ls"){
        object_xx <- make_abstract_args_ls(object_xx)
      }
    }
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
manufacture_SpecificResults<- function(x,
                                      what_1L_chr = "indexed_shareable",
                                      ...){
  if(what_1L_chr == "indexed_shareable"){
    shareable_outp_ls <- procureSlot(x,
                                     "a_SpecificShareable@shareable_outp_ls")
    secondary_chr <- names(shareable_outp_ls)[startsWith(names(shareable_outp_ls),"secondary_")]
    if(!identical(secondary_chr,character(0))){
      primary_ls <- shareable_outp_ls[setdiff(names(shareable_outp_ls),secondary_chr)]
      secondary_ls <- shareable_outp_ls[secondary_chr]
      object_xx <- append(list(primary_ls = primary_ls[(names(primary_ls))[!names(primary_ls) %>% duplicated()]]),
                           secondary_ls %>%
                             purrr::map(~.x[(.x %>% names())[!.x %>% names() %>% duplicated()]]))
    }else{
      object_xx <- list(primary_ls = shareable_outp_ls)
    }
  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
