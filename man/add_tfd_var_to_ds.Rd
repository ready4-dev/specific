% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_add.R
\name{add_tfd_var_to_ds}
\alias{add_tfd_var_to_ds}
\title{Add transformed variable to dataset}
\usage{
add_tfd_var_to_ds(
  data_tb,
  depnt_var_nm_1L_chr,
  tfmn_1L_chr,
  depnt_var_max_val_1L_dbl = NULL
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{depnt_var_nm_1L_chr}{Dependent variable name (a character vector of length one)}

\item{tfmn_1L_chr}{Transformation (a character vector of length one)}

\item{depnt_var_max_val_1L_dbl}{Dependent variable maximum value (a double vector of length one), Default: NULL}
}
\value{
Data (a tibble)
}
\description{
add_tfd_var_to_ds() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add transformed variable to dataset. Function argument data_tb specifies the object to be updated. The function returns Data (a tibble).
}
\keyword{internal}
