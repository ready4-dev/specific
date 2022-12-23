renew.specific_models <- function(x,
                                  short_name_chr = NA_character_,
                                  long_name_chr = NA_character_,
                                  control_chr = NA_character_,
                                  family_chr = NA_character_,
                                  fn_chr = NA_character_,
                                  start_chr = NA_character_,
                                  predn_type_chr = NA_character_,
                                  tfmn_chr = NA_character_,
                                  tfmn_for_bnml_lgl = NA,
                                  fixed_acronym_chr = NA_character_,
                                  mixed_acronym_chr = NA_character_,
                                  mixed_type_chr = NA_character_,
                                  with_chr = NA_character_,
                                  filter_cdn_1L_chr = NA_character_,
                                  new_cases_r3 = NULL,
                                  new_ready4_dict_r3 = deprecated(),
                                  slice_indcs_int = NA_integer_){
  if(lifecycle::is_present(new_ready4_dict_r3)) {
    lifecycle::deprecate_warn("0.0.0.9211",
                              "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)",
                              details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
  }
  fn_env_ls <- as.list(rlang::current_env())[-1]
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            fn = renew.specific_models, ## ## ##
                            fn_env_ls = fn_env_ls,
                            slice_indcs_int = slice_indcs_int)
  if(!is.null(new_cases_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_cases_r3,
                          key_var_nm_1L_chr = "short_name_chr")
  }
  return(x)
}
renew.specific_predictors <- function(x,
                                      short_name_chr = NA_character_,
                                      long_name_chr = NA_character_,
                                      min_val_dbl = NA_real_,
                                      max_val_dbl = NA_real_,
                                      class_chr = NA_character_,
                                      increment_dbl = NA_real_,
                                      class_fn_chr = NA_character_,
                                      mdl_scaling_dbl = NA_real_,
                                      covariate_lgl = NA,
                                      filter_cdn_1L_chr = NA_character_,
                                      new_cases_r3 = NULL,
                                      new_ready4_dict_r3 = deprecated(),
                                      slice_indcs_int = NA_integer_){
  if(lifecycle::is_present(new_ready4_dict_r3)) {
    lifecycle::deprecate_warn("0.0.0.9211",
                              "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)",
                              details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
  }
  fn_env_ls <- as.list(rlang::current_env())[-1]
  x <- ready4::update_tb_r3(x,
                            filter_cdn_1L_chr = filter_cdn_1L_chr,
                            fn = renew.specific_predictors, ## ## ##
                            fn_env_ls = fn_env_ls,
                            slice_indcs_int = slice_indcs_int)
  if(!is.null(new_cases_r3)){
    x <- ready4::add_lups(x,
                          new_lup = new_cases_r3,
                          key_var_nm_1L_chr = "short_name_chr")
  }
  return(x)
}
