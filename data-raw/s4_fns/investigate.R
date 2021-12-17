investigate_SpecificFixed <- function(x,
                                      backend_1L_chr = "cmdstanr",
                                      new_dir_nm_1L_chr = "F_TS_Mdls"){
  if(identical(x@b_SpecificParameters@prior_ls,list(list()))){
    prior_ls <- NULL
  }else{
    prior_ls <- x@b_SpecificParameters@prior_ls
  }
  if(identical(x@b_SpecificParameters@control_ls,list(list()))){
    control_ls <- NULL
  }else{
    control_ls <- x@b_SpecificParameters@control_ls
  }
  results_ls <- write_ts_mdls_from_alg_outp(outp_smry_ls = append(x@c_SpecificResults@b_SpecificPrivate@private_outp_ls,
                                                                  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls),
                                            predictors_lup = x@b_SpecificParameters@predictors_lup ,
                                            utl_min_val_1L_dbl = x@b_SpecificParameters@depnt_var_min_max_dbl[1],# Change
                                            backend_1L_chr = backend_1L_chr,
                                            new_dir_nm_1L_chr = new_dir_nm_1L_chr, # Method Arg
                                            iters_1L_int = x@b_SpecificParameters@iters_1L_int,
                                            prior_ls = prior_ls,
                                            control_ls = control_ls)
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- results_ls[-1] # EDIT TO REMOVE INPUTS
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  x_SpecificMixed <- SpecificMixed(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                   b_SpecificParameters = x@b_SpecificParameters,
                                   c_SpecificResults = x@c_SpecificResults,
                                   paths_chr = x@paths_chr,
                                   dissemination_1L_chr = x@dissemination_1L_chr)
  return(x_SpecificMixed)
}
investigate_SpecificInitiator <- function(x){
  if(!dir.exists(x@paths_chr[1]))
    dir.create(x@paths_chr[1])#      ready4::write_new_dirs(x@paths_chr[1])
  if(!is.na(x@b_SpecificParameters@candidate_mdls_chr[1])){
    mdl_types_chr <- x@b_SpecificParameters@candidate_mdls_chr
  }else{
    mdl_types_chr <- NULL
    if(!is.na(x@b_SpecificParameters@candidate_mdl_pfcs_chr[1])){
      choose_from_pfx_chr <- x@b_SpecificParameters@candidate_mdl_pfcs_chr
    }else{
      choose_from_pfx_chr <- NULL
    }
    if("timepoint_vals_chr" %in% methods::slotNames(x@a_YouthvarsProfile)){
      round_var_nm_1L_chr <- x@a_YouthvarsProfile@timepoint_var_nm_1L_chr
      round_bl_val_1L_chr <- x@a_YouthvarsProfile@timepoint_vals_chr[1]
    }
    ds_smry_ls <- make_ds_smry_ls(candidate_predrs_chr = x@b_SpecificParameters@candidate_predrs_chr,
                                  candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr,
                                  depnt_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr,
                                  dictionary_tb = x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3,
                                  id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr,
                                  round_var_nm_1L_chr = round_var_nm_1L_chr,
                                  round_bl_val_1L_chr = round_bl_val_1L_chr,
                                  predictors_lup = x@b_SpecificParameters@predictors_lup)
    mdl_smry_ls <- make_mdl_smry_ls(mdl_types_lup = x@b_SpecificParameters@candidate_mdls_lup,
                                    mdl_types_chr = mdl_types_chr,
                                    choose_from_pfx_chr = choose_from_pfx_chr,
                                    folds_1L_int = x@b_SpecificParameters@folds_1L_int,
                                    max_nbr_of_boruta_mdl_runs_int = x@b_SpecificParameters@max_mdl_runs_1L_int)
    results_ls <- write_mdl_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                   ds_smry_ls = ds_smry_ls,
                                   mdl_smry_ls = mdl_smry_ls,
                                   output_data_dir_1L_chr = x@paths_chr[1],
                                   seed_1L_int = x@b_SpecificParameters@seed_1L_int)
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- results_ls[-1]
    x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]

    x_SpecificModels <- SpecificModels(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                       b_SpecificParameters = x@b_SpecificParameters,
                                       c_SpecificResults = x@c_SpecificResults,
                                       paths_chr = x@paths_chr,
                                       dissemination_1L_chr = x@dissemination_1L_chr)
    return(x_SpecificModels)
  }
}
investigate_SpecificModels <- function(x){
  results_ls <- write_predr_and_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                              bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb,
                                              ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls,
                                              mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls,
                                              output_data_dir_1L_chr = x@paths_chr[1],
                                              seed_1L_int = x@b_SpecificParameters@seed_1L_int)
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- results_ls[-1]
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  x_SpecificPredictors <- SpecificPredictors(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                             b_SpecificParameters = x@b_SpecificParameters,
                                             c_SpecificResults = x@c_SpecificResults,
                                             paths_chr = x@paths_chr,
                                             dissemination_1L_chr = x@dissemination_1L_chr)
  return(x_SpecificPredictors)
}
investigate_SpecificPredictors <- function(x){
  results_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                              bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb,
                                              ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls,
                                              mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls,
                                              output_data_dir_1L_chr = x@paths_chr[1],
                                              seed_1L_int = x@b_SpecificParameters@seed_1L_int)
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- results_ls[-1] # EDIT TO REMOVE INPUTS
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  x_SpecificFixed <- SpecificFixed(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                   b_SpecificParameters = x@b_SpecificParameters,
                                   c_SpecificResults = x@c_SpecificResults,
                                   paths_chr = x@paths_chr,
                                   dissemination_1L_chr = x@dissemination_1L_chr)
  return(x_SpecificFixed)
}
