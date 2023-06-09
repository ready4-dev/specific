#' 
#' Investigate solutions to an inverse problem
#' @name investigate-SpecificModels
#' @description investigate method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param depnt_var_max_val_1L_dbl Dependent variable maximum value (a double vector of length one), Default: Inf
#' @param depnt_var_min_val_1L_dbl Dependent variable minimum value (a double vector of length one), Default: 1e-05
#' @param session_ls Session (a list), Default: NULL
#' @param ... Additional arguments
#' @return X (Modelling project dataset, input parameters and predictor comparison results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificModels-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificModels", function (x, consent_1L_chr = "", depnt_var_max_val_1L_dbl = Inf, 
    depnt_var_min_val_1L_dbl = 1e-05, session_ls = NULL, ...) 
{
    results_ls <- write_mdl_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        consent_1L_chr = consent_1L_chr, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
        depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl, 
        ds_smry_ls = manufacture(x, what_1L_chr = "ds_smry_ls"), 
        mdl_smry_ls = manufacture(x, what_1L_chr = "mdl_smry_ls"), 
        output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        seed_1L_int = x@b_SpecificParameters@seed_1L_int)
    rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1], 
        list(rename_lup = rename_lup, session_ls = session_ls))
    x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
    x_SpecificPredictors <- SpecificPredictors(a_YouthvarsProfile = x@a_YouthvarsProfile, 
        b_SpecificParameters = x@b_SpecificParameters, c_SpecificResults = x@c_SpecificResults, 
        paths_chr = x@paths_chr, dissemination_1L_chr = x@dissemination_1L_chr)
    return(x_SpecificPredictors)
})
#' 
#' Investigate solutions to an inverse problem
#' @name investigate-SpecificMixed
#' @description investigate method applied to SpecificMixed
#' @param x An object of class SpecificMixed
#' @param backend_1L_chr Backend (a character vector of length one), Default: 'cmdstanr'
#' @param combinations_1L_lgl Combinations (a logical vector of length one), Default: F
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param cores_1L_int Cores (an integer vector of length one), Default: 1
#' @param depnt_var_min_val_1L_dbl Dependent variable minimum value (a double vector of length one), Default: numeric(0)
#' @param existing_predrs_ls Existing predictors (a list), Default: NULL
#' @param max_nbr_of_covars_1L_int Maximum number of covariates (an integer vector of length one), Default: integer(0)
#' @param new_dir_nm_1L_chr New directory name (a character vector of length one), Default: 'F_TS_Mdls'
#' @param scndry_anlys_params_ls Secondary analysis parameters (a list), Default: NULL
#' @param ... Additional arguments
#' @return x (An object of class SpecificMixed)
#' @rdname investigate-methods
#' @aliases investigate,SpecificMixed-method
#' @export 
#' @importFrom stringr str_sub
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificMixed", function (x, backend_1L_chr = "cmdstanr", combinations_1L_lgl = F, 
    consent_1L_chr = "", cores_1L_int = 1L, depnt_var_min_val_1L_dbl = numeric(0), 
    existing_predrs_ls = NULL, max_nbr_of_covars_1L_int = integer(0), 
    new_dir_nm_1L_chr = "F_TS_Mdls", scndry_anlys_params_ls = NULL, 
    ...) 
{
    if (identical(x@b_SpecificParameters@prior_ls, list(list()))) {
        prior_ls <- NULL
    }
    else {
        prior_ls <- x@b_SpecificParameters@prior_ls
    }
    if (identical(x@b_SpecificParameters@control_ls, list(list()))) {
        control_ls <- NULL
    }
    else {
        control_ls <- x@b_SpecificParameters@control_ls
    }
    if (is.null(scndry_anlys_params_ls)) {
        results_ls <- write_ts_mdls_from_alg_outp(outp_smry_ls = append(x@c_SpecificResults@b_SpecificPrivate@private_outp_ls, 
            x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls), 
            combinations_1L_lgl = combinations_1L_lgl, consent_1L_chr = consent_1L_chr, 
            cores_1L_int = cores_1L_int, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
            existing_predrs_ls = existing_predrs_ls, max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int, 
            predictors_lup = x@b_SpecificParameters@predictors_lup, 
            utl_min_val_1L_dbl = x@b_SpecificParameters@depnt_var_min_max_dbl[1], 
            backend_1L_chr = backend_1L_chr, new_dir_nm_1L_chr = new_dir_nm_1L_chr, 
            iters_1L_int = x@b_SpecificParameters@iters_1L_int, 
            path_to_write_to_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
            prior_ls = prior_ls, control_ls = control_ls)
        rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
        session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
        x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1], 
            list(rename_lup = rename_lup, session_ls = session_ls))
        x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
    }
    else {
        input_params_ls <- manufacture(x, what_1L_chr = "input_params_ls")
        input_params_ls$rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
        input_params_ls$scndry_anlys_params_ls <- scndry_anlys_params_ls
        input_params_ls$path_params_ls$paths_ls <- list(write_to_dir_nm_1L_chr = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$path_to_write_to_1L_chr %>% 
            stringr::str_sub(end = -8))
        input_params_ls$outp_smry_ls <- append(x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls, 
            x@c_SpecificResults@b_SpecificPrivate@private_outp_ls)
        input_params_ls$params_ls$control_ls <- control_ls
        input_params_ls$params_ls$prior_ls <- prior_ls
        input_params_ls$params_ls$iters_1L_int <- x@b_SpecificParameters@iters_1L_int
        results_ls_ls <- write_secondary_analyses(input_params_ls, 
            backend_1L_chr = backend_1L_chr, combinations_1L_lgl = combinations_1L_lgl, 
            consent_1L_chr = consent_1L_chr, cores_1L_int = cores_1L_int, 
            existing_predrs_ls = existing_predrs_ls, max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int, 
            depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
            new_dir_nm_1L_chr = new_dir_nm_1L_chr) %>% stats::setNames(names(scndry_anlys_params_ls))
        x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls, 
            results_ls_ls %>% purrr::map(~.x[-1]))
    }
    return(x)
})
#' 
#' Investigate solutions to an inverse problem
#' @name investigate-SpecificPredictors
#' @description investigate method applied to SpecificPredictors
#' @param x An object of class SpecificPredictors
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param depnt_var_min_val_1L_dbl Dependent variable minimum value (a double vector of length one), Default: numeric(0)
#' @param signft_covars_cdn_1L_chr Significant covariates condition (a character vector of length one), Default: 'any'
#' @param ... Additional arguments
#' @return X (Modelling project dataset, input parameters and complete fixed models results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificPredictors-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificPredictors", function (x, consent_1L_chr = "", depnt_var_min_val_1L_dbl = numeric(0), 
    signft_covars_cdn_1L_chr = "any", ...) 
{
    results_ls <- write_predr_and_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb, 
        consent_1L_chr = consent_1L_chr, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
        ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls, 
        mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls, 
        output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        seed_1L_int = x@b_SpecificParameters@seed_1L_int, signft_covars_cdn_1L_chr = signft_covars_cdn_1L_chr)
    rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
    session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1], 
        list(rename_lup = rename_lup, session_ls = session_ls))
    x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
    x_SpecificFixed <- SpecificFixed(a_YouthvarsProfile = x@a_YouthvarsProfile, 
        b_SpecificParameters = x@b_SpecificParameters, c_SpecificResults = x@c_SpecificResults, 
        paths_chr = x@paths_chr, dissemination_1L_chr = x@dissemination_1L_chr)
    return(x_SpecificFixed)
})
#' 
#' Investigate solutions to an inverse problem
#' @name investigate-SpecificFixed
#' @description investigate method applied to SpecificFixed
#' @param x An object of class SpecificFixed
#' @param combinations_1L_lgl Combinations (a logical vector of length one), Default: F
#' @param consent_1L_chr Consent (a character vector of length one), Default: ''
#' @param depnt_var_min_val_1L_dbl Dependent variable minimum value (a double vector of length one), Default: numeric(0)
#' @param existing_predrs_ls Existing predictors (a list), Default: NULL
#' @param max_nbr_of_covars_1L_int Maximum number of covariates (an integer vector of length one), Default: integer(0)
#' @param ... Additional arguments
#' @return X (Modelling project dataset, input parameters and complete mixed models results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificFixed-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificFixed", function (x, combinations_1L_lgl = F, consent_1L_chr = "", depnt_var_min_val_1L_dbl = numeric(0), 
    existing_predrs_ls = NULL, max_nbr_of_covars_1L_int = integer(0), 
    ...) 
{
    results_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb, 
        combinations_1L_lgl = combinations_1L_lgl, consent_1L_chr = consent_1L_chr, 
        depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
        ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls, 
        mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls, 
        existing_predrs_ls = existing_predrs_ls, max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int, 
        output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        seed_1L_int = x@b_SpecificParameters@seed_1L_int)
    rename_lup <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$rename_lup
    session_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_ls
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls <- append(results_ls[-1], 
        list(rename_lup = rename_lup, session_ls = session_ls))
    x@c_SpecificResults@b_SpecificPrivate@private_outp_ls <- results_ls[1]
    x_SpecificMixed <- SpecificMixed(a_YouthvarsProfile = x@a_YouthvarsProfile, 
        b_SpecificParameters = x@b_SpecificParameters, c_SpecificResults = x@c_SpecificResults, 
        paths_chr = x@paths_chr, dissemination_1L_chr = x@dissemination_1L_chr)
    return(x_SpecificMixed)
})
