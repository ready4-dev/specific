% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_transform.R
\name{transform_rprt_lup}
\alias{transform_rprt_lup}
\title{Transform report lookup table}
\usage{
transform_rprt_lup(
  rprt_lup,
  add_suplry_rprt_1L_lgl = T,
  add_sharing_rprt_1L_lgl = F,
  start_at_int = NULL,
  reference_1L_int = NULL
)
}
\arguments{
\item{rprt_lup}{Report (a lookup table)}

\item{add_suplry_rprt_1L_lgl}{Add supplementary report (a logical vector of length one), Default: T}

\item{add_sharing_rprt_1L_lgl}{Add sharing report (a logical vector of length one), Default: F}

\item{start_at_int}{Start at (an integer vector), Default: NULL}

\item{reference_1L_int}{Reference (an integer vector of length one), Default: NULL}
}
\value{
Report (a lookup table)
}
\description{
transform_rprt_lup() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform report lookup table. Function argument rprt_lup specifies the object to be updated. Argument add_suplry_rprt_1L_lgl provides the object to be updated. The function returns Report (a lookup table).
}
\keyword{internal}
