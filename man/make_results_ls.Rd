% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_results_ls}
\alias{make_results_ls}
\title{Make results list}
\usage{
make_results_ls(
  spine_of_results_ls = NULL,
  abstract_args_ls = NULL,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  depnt_var_min_val_1L_dbl = numeric(0),
  dv_ds_nm_and_url_chr = NULL,
  options_chr = c("Y", "N"),
  output_format_ls = NULL,
  params_ls_ls = NULL,
  path_params_ls = NULL,
  study_descs_ls = NULL,
  fn_ls = NULL,
  include_idx_int = NULL,
  var_nm_change_lup = NULL,
  ctgl_vars_regrouping_ls = NULL,
  make_cmpst_plt_1L_lgl = T,
  outp_smry_ls = NULL,
  sig_covars_some_predrs_mdls_tb = NULL,
  sig_thresh_covars_1L_chr = NULL,
  version_1L_chr = NULL
)
}
\arguments{
\item{spine_of_results_ls}{Spine of results (a list), Default: NULL}

\item{abstract_args_ls}{Abstract arguments (a list), Default: NULL}

\item{consent_1L_chr}{Consent (a character vector of length one), Default: ''}

\item{consent_indcs_int}{Consent indices (an integer vector), Default: 1}

\item{depnt_var_min_val_1L_dbl}{Dependent variable minimum value (a double vector of length one), Default: numeric(0)}

\item{dv_ds_nm_and_url_chr}{Dataverse dataset name and url (a character vector), Default: NULL}

\item{options_chr}{Options (a character vector), Default: c("Y", "N")}

\item{output_format_ls}{Output format (a list), Default: NULL}

\item{params_ls_ls}{Parameters (a list of lists), Default: NULL}

\item{path_params_ls}{Path parameters (a list), Default: NULL}

\item{study_descs_ls}{Study descriptions (a list), Default: NULL}

\item{fn_ls}{Function list (a list of functions), Default: NULL}

\item{include_idx_int}{Include index (an integer vector), Default: NULL}

\item{var_nm_change_lup}{Variable name change (a lookup table), Default: NULL}

\item{ctgl_vars_regrouping_ls}{Categorical variables regrouping (a list), Default: NULL}

\item{make_cmpst_plt_1L_lgl}{Make composite plot (a logical vector of length one), Default: T}

\item{outp_smry_ls}{Output summary (a list), Default: NULL}

\item{sig_covars_some_predrs_mdls_tb}{Sig covariates some predictors models (a tibble), Default: NULL}

\item{sig_thresh_covars_1L_chr}{Sig thresh covariates (a character vector of length one), Default: NULL}

\item{version_1L_chr}{Version (a character vector of length one), Default: NULL}
}
\value{
Results (a list)
}
\description{
make_results_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make results list. The function returns Results (a list).
}
