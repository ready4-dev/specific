% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_make.R
\name{make_brms_mdl_plt}
\alias{make_brms_mdl_plt}
\title{Make bayesian regression models model plot}
\usage{
make_brms_mdl_plt(
  outp_smry_ls,
  depnt_var_min_val_1L_dbl = numeric(0),
  depnt_var_desc_1L_chr,
  mdl_nm_1L_chr,
  type_1L_chr,
  base_size_1L_dbl = 8,
  brms_mdl = NULL,
  correspondences_lup = NULL,
  new_var_nm_1L_chr = "Predicted",
  predn_type_1L_chr = NULL,
  x_lbl_1L_chr = NA_character_,
  y_lbl_1L_chr = NA_character_
)
}
\arguments{
\item{outp_smry_ls}{Output summary (a list)}

\item{depnt_var_min_val_1L_dbl}{Dependent variable minimum value (a double vector of length one), Default: numeric(0)}

\item{depnt_var_desc_1L_chr}{Dependent variable description (a character vector of length one)}

\item{mdl_nm_1L_chr}{Model name (a character vector of length one)}

\item{type_1L_chr}{Type (a character vector of length one)}

\item{base_size_1L_dbl}{Base size (a double vector of length one), Default: 8}

\item{brms_mdl}{Bayesian regression models (a model), Default: NULL}

\item{correspondences_lup}{Correspondences (a lookup table), Default: NULL}

\item{new_var_nm_1L_chr}{New variable name (a character vector of length one), Default: 'Predicted'}

\item{predn_type_1L_chr}{Prediction type (a character vector of length one), Default: NULL}

\item{x_lbl_1L_chr}{X label (a character vector of length one), Default: 'NA'}

\item{y_lbl_1L_chr}{Y label (a character vector of length one), Default: 'NA'}
}
\value{
Plot (a plot)
}
\description{
make_brms_mdl_plt() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make bayesian regression models model plot. The function returns Plot (a plot).
}
\keyword{internal}
