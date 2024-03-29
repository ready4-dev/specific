% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mthd_share.R
\name{share-SpecificProject}
\alias{share-SpecificProject}
\alias{share,SpecificProject-method}
\alias{share-SpecificSynopsis}
\alias{share,SpecificSynopsis-method}
\title{Share data via an online repository}
\usage{
\S4method{share}{SpecificProject}(x, fl_nm_1L_chr = "mdl_ingredients", repos_Ready4useRepos, ...)

\S4method{share}{SpecificSynopsis}(
  x,
  consolidate_1L_lgl = T,
  consent_local_1L_chr = "",
  fl_nm_1L_chr = "mdl_ingredients",
  type_1L_chr = "Models",
  what_1L_chr = "ingredients",
  ...
)
}
\arguments{
\item{x}{An object of class SpecificSynopsis}

\item{fl_nm_1L_chr}{File name (a character vector of length one), Default: 'mdl_ingredients'}

\item{repos_Ready4useRepos}{PARAM_DESCRIPTION}

\item{...}{Additional arguments}

\item{consolidate_1L_lgl}{Consolidate (a logical vector of length one), Default: T}

\item{consent_local_1L_chr}{Consent local (a character vector of length one), Default: ''}

\item{type_1L_chr}{Type (a character vector of length one), Default: 'Models'}

\item{what_1L_chr}{What (a character vector of length one), Default: 'ingredients'}
}
\description{
share method applied to SpecificProject

share method applied to SpecificSynopsis
}
