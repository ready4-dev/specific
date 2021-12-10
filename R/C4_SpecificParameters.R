#' SpecificParameters
#' 
#' Input parameters that specify candidate models to be explored.
#' 
#' @slot a_ScorzProfile  (an instance of the ScorzProfile class)
#' @slot candidate_mdls_chr Candidate models (a character vector)
#' @slot candidate_mdls_lup Candidate models (a lookup table)
#' @slot candidate_mdl_pfcs_chr Candidate model pfcs (a character vector)
#' @slot candidate_predrs_chr Candidate predictors (a character vector)
#' @slot candidate_covars_chr Candidate covariates (a character vector)
#' @slot depnt_var_nm_1L_chr Dependent variable name (a character vector of length one)
#' @slot depnt_var_max_val_1L_dbl Dependent variable maximum value (a double vector of length one)
#' @slot folds_1L_int Folds (an integer vector of length one)
#' @slot max_mdl_runs_1L_int Maximum model runs (an integer vector of length one)
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
slots = c(a_ScorzProfile = "ScorzProfile",candidate_mdls_chr = "character",candidate_mdls_lup = "specific_models",candidate_mdl_pfcs_chr = "character",candidate_predrs_chr = "character",candidate_covars_chr = "character",depnt_var_nm_1L_chr = "character",depnt_var_max_val_1L_dbl = "numeric",folds_1L_int = "integer",max_mdl_runs_1L_int = "integer",seed_1L_int = "integer",predictors_lup = "specific_predictors",dissemination_1L_chr = "character"),
prototype =  list(a_ScorzProfile = scorz::ScorzProfile(),candidate_mdls_chr = NA_character_,candidate_mdls_lup = specific_models(),candidate_mdl_pfcs_chr = NA_character_,candidate_predrs_chr = NA_character_,candidate_covars_chr = NA_character_,depnt_var_nm_1L_chr = NA_character_,depnt_var_max_val_1L_dbl = NA_real_,folds_1L_int = NA_integer_,max_mdl_runs_1L_int = NA_integer_,seed_1L_int = NA_integer_,predictors_lup = specific_predictors()))


methods::setValidity(methods::className("SpecificParameters"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
