manufacture_SpecificMixed <- function(x,
                                      a_Ready4useRepos = NULL,
                                      scndry_anlys_params_ls = NULL,
                                      what_1L_chr = "input_params_ls"){
  if(what_1L_chr == "input_params_ls"){
    header_yaml_args_ls <- ready4show::make_header_yaml_args_ls(authors_tb = Z@authors_r3,
                                                                institutes_tb = Z@institutes_r3,
                                                                title_1L_chr = Z@title_1L_chr,
                                                                keywords_chr = Z@keywords_chr)
    maui_params_ls <- make_maui_params_ls(maui_domains_pfxs_1L_chr = X@b_SpecificParameters@itm_prefix_1L_chr,
                                          maui_itm_short_nms_chr = X@b_SpecificParameters@itm_labels_chr,
                                          maui_scoring_fn = NULL)
    output_format_ls <- ready4show::make_output_format_ls(manuscript_outp_1L_chr = Z@outp_formats_chr[1],
                                                          manuscript_digits_1L_int = Z@digits_int[1],
                                                          supplementary_outp_1L_chr = ifelse(length(Z@outp_formats_chr)>1,Z@outp_formats_chr[2],Z@outp_formats_chr[1]),
                                                          supplementary_digits_1L_int = ifelse(length(Z@digits_int)>1,Z@digits_int[2],Z@digits_int[1]))
    # scndry_anlys_params_ls <- make_scndry_anlys_params(candidate_predrs_chr = c("SOFAS"),
    #                                                    prefd_covars_chr = NA_character_)
    object_xx <- make_input_params(X@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                         control_ls = X@b_SpecificParameters@control_ls,
                                         ds_descvs_ls = manufacture(X,
                                                                    what_1L_chr = "ds_descvs_ls"),
                                         dv_ds_nm_and_url_chr = c(a_Ready4useRepos@dv_nm_1L_chr,
                                                                  a_Ready4useRepos@dv_ds_nm_1L_chr),
                                         header_yaml_args_ls = header_yaml_args_ls,
                                         maui_params_ls = maui_params_ls,
                                         output_format_ls = output_format_ls,
                                         predictors_lup = X@b_SpecificParameters@predictors_lup,
                                         prefd_covars_chr = ifelse(is.null(procure(X,
                                                                                   what = "prefd_covars")),
                                                                   NA_character_,
                                                                   procure(X,what = "prefd_covars")),
                                         prefd_mdl_types_chr = procure(X,
                                                                       what = "prefd_mdls"),
                                         scndry_anlys_params_ls = scndry_anlys_params_ls)#

  }else{
    object_xx <- methods::callNextMethod()
  }
  return(object_xx)
}
manufacture_SpecificProject <- function(x,
                                       what_1L_chr = "ds_descvs_ls"){
  if(what_1L_chr %in% c("ds_descvs_ls","ds_smry_ls"))
    object_xx <- make_ds_descvs_ls(candidate_predrs_chr =  x@b_SpecificParameters@candidate_predrs_chr,
                                   candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr,
                                   cohort_descv_var_nms_chr = x@b_SpecificParameters@descv_var_nms_chr,
                                   dictionary_tb = x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3,
                                   id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                   is_fake_1L_lgl = x@b_SpecificParameters@fake_1L_lgl,
                                   msrmnt_date_var_nm_1L_chr = x@b_SpecificParameters@msrmnt_date_var_nm_1L_chr,
                                   round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr,
                                   round_vals_chr = x@a_YouthvarsProfile@timepoint_vals_chr,
                                   utl_wtd_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr,
                                   maui_item_pfx_1L_chr = x@b_SpecificParameters@itm_prefix_1L_chr,
                                   utl_unwtd_var_nm_1L_chr = x@b_SpecificParameters@total_unwtd_var_nm_1L_chr)

  if(what_1L_chr == ("ds_smry_ls"))
    object_xx <- object_xx %>%
      make_analysis_ds_smry_ls(candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr,
                               predictors_lup = x@b_SpecificParameters@predictors_lup)
  if(what_1L_chr == "mdl_smry_ls"){
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
    object_xx <- make_mdl_smry_ls(mdl_types_lup = x@b_SpecificParameters@candidate_mdls_lup,
                                  mdl_types_chr = mdl_types_chr,
                                  choose_from_pfx_chr = choose_from_pfx_chr,
                                  folds_1L_int = x@b_SpecificParameters@folds_1L_int,
                                  max_nbr_of_boruta_mdl_runs_int = x@b_SpecificParameters@max_mdl_runs_1L_int)
  }
  return(object_xx)
}
