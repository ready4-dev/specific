#' Fit time series model with bayesian regression model
#' @description fit_ts_model_with_brm() is a Fit function that fits a model of a specified type to a dataset Specifically, this function implements an algorithm to fit time series model with bayesian regression model. The function returns Model list (a list of models).
#' @param data_tb Data (a tibble)
#' @param depnt_var_nm_1L_chr Dependent variable name (a character vector of length one)
#' @param family_fn_1L_chr Family function (a character vector of length one)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one)
#' @param predr_vars_nms_chr Predictor variables names (a character vector)
#' @param backend_1L_chr Backend (a character vector of length one), Default: getOption("brms.backend", "rstan")
#' @param control_ls Control (a list), Default: NULL
#' @param is_csnl_1L_lgl Is cross-sectional (a logical vector of length one), Default: F
#' @param iters_1L_int Iterations (an integer vector of length one), Default: 4000
#' @param seed_1L_int Seed (an integer vector of length one), Default: 1000
#' @param prior_ls Prior (a list), Default: NULL
#' @return Model list (a list of models)
#' @rdname fit_ts_model_with_brm
#' @export 
#' @importFrom brms brm
#' @importFrom stats as.formula
#' @importFrom purrr map_chr
#' @keywords internal
fit_ts_model_with_brm <- function (data_tb, depnt_var_nm_1L_chr, family_fn_1L_chr, id_var_nm_1L_chr, 
    predr_vars_nms_chr, backend_1L_chr = getOption("brms.backend", 
        "rstan"), control_ls = NULL, is_csnl_1L_lgl = F, iters_1L_int = 4000L, 
    seed_1L_int = 1000L, prior_ls = NULL) 
{
    mdl_ls <- brms::brm(formula = stats::as.formula(paste0(depnt_var_nm_1L_chr, 
        " ~ ", purrr::map_chr(predr_vars_nms_chr, ~paste0(.x, 
            ifelse(is_csnl_1L_lgl, ifelse(paste0(.x, "_scaled") %in% 
                names(data_tb), "_scaled + ", " + "), "_baseline + "), 
            ifelse(is_csnl_1L_lgl, "", paste0(.x, "_change + ")))) %>% 
            paste0(collapse = ""), "(1|", id_var_nm_1L_chr, ")")), 
        backend = backend_1L_chr, data = data_tb, family = eval(parse(text = family_fn_1L_chr)), 
        iter = iters_1L_int, seed = seed_1L_int, prior = prior_ls, 
        control = control_ls)
    return(mdl_ls)
}
