% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_predict.R
\name{predict_shrble_lm}
\alias{predict_shrble_lm}
\title{Predict shareable linear model}
\usage{
predict_shrble_lm(
  object,
  newdata,
  se.fit = FALSE,
  scale = NULL,
  df = Inf,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  type = c("response", "terms"),
  terms = NULL,
  na.action = na.pass,
  pred.var = res.var/weights,
  weights = 1,
  sd_1L_dbl,
  ...
)
}
\arguments{
\item{object}{PARAM_DESCRIPTION}

\item{newdata}{New dataset (for prediction) (a new dataset (for prediction))}

\item{se.fit}{PARAM_DESCRIPTION, Default: FALSE}

\item{scale}{PARAM_DESCRIPTION, Default: NULL}

\item{df}{Data.frame (a data.frame), Default: Inf}

\item{interval}{PARAM_DESCRIPTION, Default: c("none", "confidence", "prediction")}

\item{level}{PARAM_DESCRIPTION, Default: 0.95}

\item{type}{PARAM_DESCRIPTION, Default: c("response", "terms")}

\item{terms}{PARAM_DESCRIPTION, Default: NULL}

\item{na.action}{PARAM_DESCRIPTION, Default: na.pass}

\item{pred.var}{PARAM_DESCRIPTION, Default: res.var/weights}

\item{weights}{PARAM_DESCRIPTION, Default: 1}

\item{sd_1L_dbl}{Standard deviation (a double vector of length one)}

\item{...}{Additional arguments}
}
\description{
predict_shrble_lm() is a Predict function that makes predictions from data using a specified statistical model. Specifically, this function implements an algorithm to predict shareable linear model. The function is called for its side effects and does not return a value.
}
\keyword{internal}
