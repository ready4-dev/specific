#' 
#' Investigate solutions to an inverse problem
#' @name investigate-SpecificModels
#' @description investigate method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param depnt_var_max_val_1L_dbl Dependent variable maximum value (a double vector of length one), Default: Inf
#' @param session_ls Session (a list), Default: NULL
#' @return X (Modelling project dataset, input parameters and predictor comparison results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificModels-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificModels", function (x, depnt_var_max_val_1L_dbl = Inf, session_ls = NULL) 
{
    results_ls <- write_mdl_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        ds_smry_ls = manufacture(x, what_1L_chr = "ds_smry_ls"), 
        mdl_smry_ls = manufacture(x, what_1L_chr = "mdl_smry_ls"), 
        output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl, 
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
#' @param new_dir_nm_1L_chr New directory name (a character vector of length one), Default: 'F_TS_Mdls'
#' @param scndry_anlys_params_ls Secondary analysis parameters (a list), Default: NULL
#' @return x (An object of class SpecificMixed)
#' @rdname investigate-methods
#' @aliases investigate,SpecificMixed-method
#' @export 
#' @importFrom stringr str_sub
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificMixed", function (x, backend_1L_chr = "cmdstanr", new_dir_nm_1L_chr = "F_TS_Mdls", 
    scndry_anlys_params_ls = NULL) 
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
            backend_1L_chr = backend_1L_chr, new_dir_nm_1L_chr = new_dir_nm_1L_chr) %>% 
            stats::setNames(names(scndry_anlys_params_ls))
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
#' @return X (Modelling project dataset, input parameters and complete fixed models results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificPredictors-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificPredictors", function (x) 
{
    results_ls <- write_predr_and_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb, 
        ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls, 
        mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls, 
        output_data_dir_1L_chr = x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr, 
        seed_1L_int = x@b_SpecificParameters@seed_1L_int)
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
#' @return X (Modelling project dataset, input parameters and complete mixed models results.)
#' @rdname investigate-methods
#' @aliases investigate,SpecificFixed-method
#' @export 
#' @importFrom ready4 investigate
methods::setMethod("investigate", "SpecificFixed", function (x) 
{
    results_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
        bl_tb = x@c_SpecificResults@b_SpecificPrivate@private_outp_ls$bl_tb, 
        ds_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$ds_smry_ls, 
        mdl_smry_ls = x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls, 
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
