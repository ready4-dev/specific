% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_predr_cmprsn_outps}
\alias{write_predr_cmprsn_outps}
\title{Write predictor comparison outputs}
\usage{
write_predr_cmprsn_outps(
  data_tb,
  path_to_write_to_1L_chr,
  new_dir_nm_1L_chr = "B_Candidate_Predrs_Cmprsn",
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  depnt_var_min_val_1L_dbl = numeric(0),
  depnt_var_nm_1L_chr = "utl_total_w",
  candidate_predrs_chr,
  max_nbr_of_boruta_mdl_runs_int = 300L,
  options_chr = c("Y", "N")
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{path_to_write_to_1L_chr}{Path to write to (a character vector of length one)}

\item{new_dir_nm_1L_chr}{New directory name (a character vector of length one), Default: 'B_Candidate_Predrs_Cmprsn'}

\item{consent_1L_chr}{Consent (a character vector of length one), Default: ''}

\item{consent_indcs_int}{Consent indices (an integer vector), Default: 1}

\item{depnt_var_min_val_1L_dbl}{Dependent variable minimum value (a double vector of length one), Default: numeric(0)}

\item{depnt_var_nm_1L_chr}{Dependent variable name (a character vector of length one), Default: 'utl_total_w'}

\item{candidate_predrs_chr}{Candidate predictors (a character vector)}

\item{max_nbr_of_boruta_mdl_runs_int}{Maximum number of boruta model runs (an integer vector), Default: 300}

\item{options_chr}{Options (a character vector), Default: c("Y", "N")}
}
\value{
Confirmed predictors (a tibble)
}
\description{
write_predr_cmprsn_outps() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write predictor comparison outputs. The function returns Confirmed predictors (a tibble).
}
