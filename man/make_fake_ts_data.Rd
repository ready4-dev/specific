% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_fake_ts_data}
\alias{make_fake_ts_data}
\title{Make fake time series data}
\usage{
make_fake_ts_data(
  outp_smry_ls,
  depnt_var_min_val_1L_dbl = numeric(0),
  depnt_vars_are_NA_1L_lgl = T
)
}
\arguments{
\item{outp_smry_ls}{Output summary (a list)}

\item{depnt_var_min_val_1L_dbl}{Dependent variable minimum value (a double vector of length one), Default: numeric(0)}

\item{depnt_vars_are_NA_1L_lgl}{Dependent variables are NA (a logical vector of length one), Default: T}
}
\value{
Fk data (a tibble)
}
\description{
make_fake_ts_data() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make fake time series data. The function returns Fk data (a tibble).
}
