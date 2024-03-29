% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_ds_smry_ls}
\alias{make_ds_smry_ls}
\title{Make dataset summary list}
\usage{
make_ds_smry_ls(
  candidate_predrs_chr,
  candidate_covar_nms_chr,
  depnt_var_nm_1L_chr,
  dictionary_tb,
  id_var_nm_1L_chr,
  round_var_nm_1L_chr,
  round_bl_val_1L_chr,
  predictors_lup
)
}
\arguments{
\item{candidate_predrs_chr}{Candidate predictors (a character vector)}

\item{candidate_covar_nms_chr}{Candidate covariate names (a character vector)}

\item{depnt_var_nm_1L_chr}{Dependent variable name (a character vector of length one)}

\item{dictionary_tb}{Dictionary (a tibble)}

\item{id_var_nm_1L_chr}{Identity variable name (a character vector of length one)}

\item{round_var_nm_1L_chr}{Round variable name (a character vector of length one)}

\item{round_bl_val_1L_chr}{Round baseline value (a character vector of length one)}

\item{predictors_lup}{Predictors (a lookup table)}
}
\value{
Dataset summary (a list)
}
\description{
make_ds_smry_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make dataset summary list. The function returns Dataset summary (a list).
}
