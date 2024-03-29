% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_predict.R
\name{predict_vals}
\alias{predict_vals}
\title{Predict values}
\usage{
predict_vals(
  data_tb,
  model_mdl,
  family_1L_chr = NA_character_,
  force_new_data_1L_lgl = F,
  min_max_vals_dbl = numeric(0),
  is_brms_mdl_1L_lgl = T,
  impute_1L_lgl = T,
  new_data_is_1L_chr = "Predicted",
  predn_type_1L_chr = NULL,
  sd_dbl = NA_real_,
  tfmn_1L_chr = "NTF",
  tfmn_for_bnml_1L_lgl = F,
  var_cls_fn = NULL
)
}
\arguments{
\item{data_tb}{Data (a tibble)}

\item{model_mdl}{Model (a model)}

\item{family_1L_chr}{Family (a character vector of length one), Default: 'NA'}

\item{force_new_data_1L_lgl}{Force new data (a logical vector of length one), Default: F}

\item{min_max_vals_dbl}{Minimum maximum values (a double vector), Default: numeric(0)}

\item{is_brms_mdl_1L_lgl}{Is bayesian regression models model (a logical vector of length one), Default: T}

\item{impute_1L_lgl}{Impute (a logical vector of length one), Default: T}

\item{new_data_is_1L_chr}{New data is (a character vector of length one), Default: 'Predicted'}

\item{predn_type_1L_chr}{Prediction type (a character vector of length one), Default: NULL}

\item{sd_dbl}{Standard deviation (a double vector), Default: NA}

\item{tfmn_1L_chr}{Transformation (a character vector of length one), Default: 'NTF'}

\item{tfmn_for_bnml_1L_lgl}{Transformation for binomial (a logical vector of length one), Default: F}

\item{var_cls_fn}{Variable class (a function), Default: NULL}
}
\value{
Predicted values (a double vector)
}
\description{
predict_vals() is a Predict function that makes predictions from data using a specified statistical model. Specifically, this function implements an algorithm to predict values. The function returns Predicted values (a double vector).
}
