investigate_SpecificMixed <- function(x,
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
  rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
  session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1],
                                                                      list(rename_lup = rename_lup,
                                                                           session_ls = session_ls)) # EDIT TO REMOVE INPUTS
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  return(x)
}
investigate_SpecificModels <- function(x,
                                       depnt_var_max_val_1L_dbl = Inf,
                                       session_ls = NULL){
    results_ls <- write_mdl_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                     ds_smry_ls = manufacture(x,
                                              what_1L_chr = "ds_smry_ls"),
                     mdl_smry_ls = manufacture(x,
                                               what_1L_chr = "mdl_smry_ls"),
                     output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr,
                     depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl,
                     seed_1L_int = x@b_SpecificParameters@seed_1L_int)
    rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1],
                                                                        list(rename_lup = rename_lup,
                                                                             session_ls = session_ls))
    x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]

    x_SpecificPredictors <- SpecificPredictors(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                               b_SpecificParameters = x@b_SpecificParameters,
                                               c_SpecificResults = x@c_SpecificResults,
                                               paths_chr = x@paths_chr,
                                               dissemination_1L_chr = x@dissemination_1L_chr)
    return(x_SpecificPredictors)
  # }
}
investigate_SpecificPredictors <- function(x){
  results_ls <- write_predr_and_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                              bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb,
                                              ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls,
                                              mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls,
                                              output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr,
                                              seed_1L_int = x@b_SpecificParameters@seed_1L_int)
  rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
  session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1],
                                                                      list(rename_lup = rename_lup,
                                                                           session_ls = session_ls))
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  x_SpecificFixed <- SpecificFixed(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                   b_SpecificParameters = x@b_SpecificParameters,
                                   c_SpecificResults = x@c_SpecificResults,
                                   paths_chr = x@paths_chr,
                                   dissemination_1L_chr = x@dissemination_1L_chr)
  return(x_SpecificFixed)
}
investigate_SpecificFixed <- function(x){
  results_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                              bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb,
                                              ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls,
                                              mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls,
                                              output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr,
                                              seed_1L_int = x@b_SpecificParameters@seed_1L_int)
  rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
  session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1],
                                                                      list(rename_lup = rename_lup,
                                                                           session_ls = session_ls)) # EDIT TO REMOVE INPUTS
  x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
  x_SpecificMixed <- SpecificMixed(a_YouthvarsProfile = x@a_YouthvarsProfile,
                                   b_SpecificParameters = x@b_SpecificParameters,
                                   c_SpecificResults = x@c_SpecificResults,
                                   paths_chr = x@paths_chr,
                                   dissemination_1L_chr = x@dissemination_1L_chr)
  return(x_SpecificMixed)
}
