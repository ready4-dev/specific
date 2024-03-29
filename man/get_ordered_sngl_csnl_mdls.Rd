% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_get.R
\name{get_ordered_sngl_csnl_mdls}
\alias{get_ordered_sngl_csnl_mdls}
\title{Get ordered single cross-sectional models}
\usage{
get_ordered_sngl_csnl_mdls(results_ls, select_int = NULL, collapse_1L_lgl = F)
}
\arguments{
\item{results_ls}{Results (a list)}

\item{select_int}{Select (an integer vector), Default: NULL}

\item{collapse_1L_lgl}{Collapse (a logical vector of length one), Default: F}
}
\value{
Ordered single cross-sectional models (a character vector)
}
\description{
get_ordered_sngl_csnl_mdls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get ordered single cross-sectional models. Function argument results_ls specifies the where to look for the required object. The function returns Ordered single cross-sectional models (a character vector).
}
