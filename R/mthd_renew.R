#' Renew values in a dataset
#' @description renew.specific_models() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Candidate models lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Candidate models lookup table
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param control_chr Control (a character vector), Default: 'NA'
#' @param family_chr Family (a character vector), Default: 'NA'
#' @param fn_chr Function (a character vector), Default: 'NA'
#' @param start_chr Start (a character vector), Default: 'NA'
#' @param predn_type_chr Prediction type (a character vector), Default: 'NA'
#' @param tfmn_chr Transformation (a character vector), Default: 'NA'
#' @param tfmn_for_bnml_lgl Transformation for binomial (a logical vector), Default: NA
#' @param fixed_acronym_chr Fixed acronym (a character vector), Default: 'NA'
#' @param mixed_acronym_chr Mixed acronym (a character vector), Default: 'NA'
#' @param mixed_type_chr Mixed type (a character vector), Default: 'NA'
#' @param with_chr With (a character vector), Default: 'NA'
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3), Default: deprecated()
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 add_lups renew
renew.specific_models <- function (x, short_name_chr = NA_character_, long_name_chr = NA_character_, 
    control_chr = NA_character_, family_chr = NA_character_, 
    fn_chr = NA_character_, start_chr = NA_character_, predn_type_chr = NA_character_, 
    tfmn_chr = NA_character_, tfmn_for_bnml_lgl = NA, fixed_acronym_chr = NA_character_, 
    mixed_acronym_chr = NA_character_, mixed_type_chr = NA_character_, 
    with_chr = NA_character_, filter_cdn_1L_chr = NA_character_, 
    new_cases_r3 = NULL, new_ready4_dict_r3 = deprecated(), slice_idxs_int = NA_integer_) 
{
    if (lifecycle::is_present(new_ready4_dict_r3)) {
        lifecycle::deprecate_warn("0.0.0.9211", "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)", 
            details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
    }
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        fn = renew.specific_models, fn_env_ls = fn_env_ls, slice_idxs_int = slice_idxs_int)
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "short_name_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,specific_models-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("specific_models", package = "specific"), renew.specific_models)
#' Renew values in a dataset
#' @description renew.specific_predictors() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the Candidate predictors lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Candidate predictors lookup table
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param min_val_dbl Minimum value (a double vector), Default: NA
#' @param max_val_dbl Maximum value (a double vector), Default: NA
#' @param class_chr Class (a character vector), Default: 'NA'
#' @param increment_dbl Increment (a double vector), Default: NA
#' @param class_fn_chr Class function (a character vector), Default: 'NA'
#' @param mdl_scaling_dbl Model scaling (a double vector), Default: NA
#' @param covariate_lgl Covariate (a logical vector), Default: NA
#' @param filter_cdn_1L_chr Filter condition (a character vector of length one), Default: 'NA'
#' @param new_cases_r3 New cases (a ready4 S3), Default: NULL
#' @param new_ready4_dict_r3 New ready4 dictionary (a ready4 S3), Default: deprecated()
#' @param slice_idxs_int Slice indices (an integer vector), Default: NA
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 add_lups renew
renew.specific_predictors <- function (x, short_name_chr = NA_character_, long_name_chr = NA_character_, 
    min_val_dbl = NA_real_, max_val_dbl = NA_real_, class_chr = NA_character_, 
    increment_dbl = NA_real_, class_fn_chr = NA_character_, mdl_scaling_dbl = NA_real_, 
    covariate_lgl = NA, filter_cdn_1L_chr = NA_character_, new_cases_r3 = NULL, 
    new_ready4_dict_r3 = deprecated(), slice_idxs_int = NA_integer_) 
{
    if (lifecycle::is_present(new_ready4_dict_r3)) {
        lifecycle::deprecate_warn("0.0.0.9211", "ready4use::renew.ready4use_dictionary(new_ready4_dict_r3)", 
            details = "Please use `ready4use::renew.ready4use_dictionary(new_cases_r3)` instead.")
    }
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x, filter_cdn_1L_chr = filter_cdn_1L_chr, 
        fn = renew.specific_predictors, fn_env_ls = fn_env_ls, 
        slice_idxs_int = slice_idxs_int)
    if (!is.null(new_cases_r3)) {
        x <- ready4::add_lups(x, new_lup = new_cases_r3, key_var_nm_1L_chr = "short_name_chr")
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,specific_predictors-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("specific_predictors", package = "specific"), renew.specific_predictors)
#' 
#' Renew values in a dataset
#' @name renew-SpecificProject
#' @description renew method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param new_val_xx New value (an output object of multiple potential types)
#' @param type_1L_chr Type (a character vector of length one), Default: 'results'
#' @param what_1L_chr What (a character vector of length one), Default: 'prefd_mdls'
#' @return x (An object of class SpecificProject)
#' @rdname renew-methods
#' @aliases renew,SpecificProject-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "SpecificProject", function (x, new_val_xx, type_1L_chr = "results", what_1L_chr = "prefd_mdls") 
{
    if (type_1L_chr == "results") {
        if (what_1L_chr == "prefd_mdls") 
            x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_mdl_types_chr <- new_val_xx
    }
    if (what_1L_chr == "prefd_covars") 
        x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_covars_chr <- new_val_xx
    return(x)
})
#' 
#' Renew values in a dataset
#' @name renew-SpecificMixed
#' @description renew method applied to SpecificMixed
#' @param x An object of class SpecificMixed
#' @param new_val_xx New value (an output object of multiple potential types), Default: NULL
#' @param a_Ready4useRepos PARAM_DESCRIPTION
#' @param type_1L_chr Type (a character vector of length one), Default: 'results'
#' @param what_1L_chr What (a character vector of length one), Default: 'dv_ls'
#' @return x (An object of class SpecificMixed)
#' @rdname renew-methods
#' @aliases renew,SpecificMixed-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "SpecificMixed", function (x, new_val_xx = NULL, a_Ready4useRepos, type_1L_chr = "results", 
    what_1L_chr = "dv_ls") 
{
    dv_ls <- list(dv_nm_1L_chr = procureSlot(a_Ready4useRepos, 
        "dv_nm_1L_chr"), ds_url_1L_chr = procureSlot(a_Ready4useRepos, 
        "dv_ds_nm_1L_chr"), parent_dv_dir_1L_chr = paste0(x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        "/H_Dataverse"))
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$dv_ls <- dv_ls
    return(x)
})
