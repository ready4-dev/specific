#' SpecificParameters
#' 
#' Input parameters that specify candidate models to be explored.
#' 
#' @include fn_get.R
#' @slot candidate_mdls_chr Candidate models (a character vector)
#' @slot candidate_mdls_lup Candidate models (a lookup table)
#' @slot candidate_mdl_pfcs_chr Candidate model pfcs (a character vector)
#' @slot candidate_predrs_chr Candidate predictors (a character vector)
#' @slot candidate_covars_chr Candidate covariates (a character vector)
#' @slot control_ls Control (a list)
#' @slot depnt_var_nm_1L_chr Dependent variable name (a character vector of length one)
#' @slot depnt_var_min_max_dbl Dependent variable minimum maximum (a double vector)
#' @slot descv_var_nms_chr Descriptive variable names (a character vector)
#' @slot fake_1L_lgl Fake (a logical vector of length one)
#' @slot folds_1L_int Folds (an integer vector of length one)
#' @slot itm_labels_chr Item labels (a character vector)
#' @slot itm_prefix_1L_chr Item prefix (a character vector of length one)
#' @slot iters_1L_int Iterations (an integer vector of length one)
#' @slot max_mdl_runs_1L_int Maximum model runs (an integer vector of length one)
#' @slot msrmnt_date_var_nm_1L_chr Measurement date variable name (a character vector of length one)
#' @slot paths_ls Paths (a list)
#' @slot prior_ls Prior (a list)
#' @slot seed_1L_int Seed (an integer vector of length one)
#' @slot total_unwtd_var_nm_1L_chr Total unweighted variable name (a character vector of length one)
#' @slot predictors_lup Predictors (a lookup table)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificParameters-class
#' @rdname SpecificParameters-class
#' @export SpecificParameters
#' @exportClass SpecificParameters
SpecificParameters <- methods::setClass("SpecificParameters",
contains = "Ready4Module",
slots = c(candidate_mdls_chr = "character",candidate_mdls_lup = "specific_models",candidate_mdl_pfcs_chr = "character",candidate_predrs_chr = "character",candidate_covars_chr = "character",control_ls = "list",depnt_var_nm_1L_chr = "character",depnt_var_min_max_dbl = "numeric",descv_var_nms_chr = "character",fake_1L_lgl = "logical",folds_1L_int = "integer",itm_labels_chr = "character",itm_prefix_1L_chr = "character",iters_1L_int = "integer",max_mdl_runs_1L_int = "integer",msrmnt_date_var_nm_1L_chr = "character",paths_ls = "list",prior_ls = "list",seed_1L_int = "integer",total_unwtd_var_nm_1L_chr = "character",predictors_lup = "specific_predictors",dissemination_1L_chr = "character"),
prototype =  list(candidate_mdls_chr = NA_character_,candidate_mdls_lup = get_cndt_mdls(),candidate_mdl_pfcs_chr = NA_character_,candidate_predrs_chr = NA_character_,candidate_covars_chr = NA_character_,control_ls = list(list()),depnt_var_nm_1L_chr = NA_character_,depnt_var_min_max_dbl = NA_real_,descv_var_nms_chr = NA_character_,fake_1L_lgl = F,folds_1L_int = 10L,itm_labels_chr = NA_character_,itm_prefix_1L_chr = NA_character_,iters_1L_int = 4000L,max_mdl_runs_1L_int = 300L,msrmnt_date_var_nm_1L_chr = NA_character_,paths_ls = list(list()),prior_ls = list(list()),seed_1L_int = 1234L,total_unwtd_var_nm_1L_chr = NA_character_,predictors_lup = specific_predictors()))


methods::setValidity(methods::className("SpecificParameters"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
