% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_mdl}
\alias{make_mdl}
\title{Make model}
\usage{
make_mdl(
  data_tb,
  depnt_var_min_val_1L_dbl = numeric(0),
  depnt_var_nm_1L_chr = "utl_total_w",
  tfmn_1L_chr = "NTF",
  predr_var_nm_1L_chr,
  covar_var_nms_chr = NA_character_,
  mdl_type_1L_chr = "OLS_NTF",
  mdl_types_lup = NULL,
  control_1L_chr = NA_character_,
  start_1L_chr = NULL
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{depnt_var_min_val_1L_dbl}{Dependent variable minimum value (a double vector of length one), Default: numeric(0)}

\item{depnt_var_nm_1L_chr}{Dependent variable name (a character vector of length one), Default: 'utl_total_w'}

\item{tfmn_1L_chr}{Transformation (a character vector of length one), Default: 'NTF'}

\item{predr_var_nm_1L_chr}{Predictor variable name (a character vector of length one)}

\item{covar_var_nms_chr}{Covariate variable names (a character vector), Default: 'NA'}

\item{mdl_type_1L_chr}{Model type (a character vector of length one), Default: 'OLS_NTF'}

\item{mdl_types_lup}{Model types (a lookup table), Default: NULL}

\item{control_1L_chr}{Control (a character vector of length one), Default: 'NA'}

\item{start_1L_chr}{Start (a character vector of length one), Default: NULL}
}
\value{
Model (a model)
}
\description{
make_mdl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make model. The function returns Model (a model).
}
