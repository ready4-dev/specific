#' SpecificParameters
#' 
#' Input parameters that specify candidate models to be explored.
#' 
#' @slot candidate_mdls_chr Candidate models (a character vector)
#' @slot candidate_mdls_lup Candidate models (a lookup table)
#' @slot candidate_mdl_pfcs_chr Candidate model pfcs (a character vector)
#' @slot candidate_predrs_chr Candidate predictors (a character vector)
#' @slot candidate_covars_chr Candidate covariates (a character vector)
#' @slot control_ls Control (a list)
#' @slot depnt_var_nm_1L_chr Dependent variable name (a character vector of length one)
#' @slot depnt_var_min_max_dbl Dependent variable minimum maximum (a double vector)
#' @slot folds_1L_int Folds (an integer vector of length one)
#' @slot iters_1L_int Iterations (an integer vector of length one)
#' @slot max_mdl_runs_1L_int Maximum model runs (an integer vector of length one)
#' @slot prior_ls Prior (a list)
#' @slot seed_1L_int Seed (an integer vector of length one)
#' @slot predictors_lup Predictors (a lookup table)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificParameters-class
#' @rdname SpecificParameters-class
#' @export SpecificParameters
#' @exportClass SpecificParameters
SpecificParameters <- methods::setClass("SpecificParameters",
contains = "Ready4Module",
slots = c(candidate_mdls_chr = "character",candidate_mdls_lup = "specific_models",candidate_mdl_pfcs_chr = "character",candidate_predrs_chr = "character",candidate_covars_chr = "character",control_ls = "list",depnt_var_nm_1L_chr = "character",depnt_var_min_max_dbl = "numeric",folds_1L_int = "integer",iters_1L_int = "integer",max_mdl_runs_1L_int = "integer",prior_ls = "list",seed_1L_int = "integer",predictors_lup = "specific_predictors",dissemination_1L_chr = "character"),
prototype =  list(candidate_mdls_chr = NA_character_,candidate_mdls_lup = specific_models(),candidate_mdl_pfcs_chr = NA_character_,candidate_predrs_chr = NA_character_,candidate_covars_chr = NA_character_,control_ls = list(list()),depnt_var_nm_1L_chr = NA_character_,depnt_var_min_max_dbl = NA_real_,folds_1L_int = NA_integer_,iters_1L_int = NA_integer_,max_mdl_runs_1L_int = NA_integer_,prior_ls = list(list()),seed_1L_int = NA_integer_,predictors_lup = specific_predictors()))


methods::setValidity(methods::className("SpecificParameters"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})