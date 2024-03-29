% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_mdls_to_dv}
\alias{write_mdls_to_dv}
\title{Write models to dataverse}
\usage{
write_mdls_to_dv(
  outp_smry_ls,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  new_dir_nm_1L_chr = "G_Shareable",
  options_chr = c("Y", "N"),
  shareable_title_detail_1L_chr = "",
  output_dir_chr = NA_character_
)
}
\arguments{
\item{outp_smry_ls}{Output summary (a list)}

\item{consent_1L_chr}{Consent (a character vector of length one), Default: ''}

\item{consent_indcs_int}{Consent indices (an integer vector), Default: 1}

\item{new_dir_nm_1L_chr}{New directory name (a character vector of length one), Default: 'G_Shareable'}

\item{options_chr}{Options (a character vector), Default: c("Y", "N")}

\item{shareable_title_detail_1L_chr}{Shareable title detail (a character vector of length one), Default: ''}

\item{output_dir_chr}{Output directory (a character vector), Default: 'NA'}
}
\value{
Output summary (a list)
}
\description{
write_mdls_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write models to dataverse. The function returns Output summary (a list).
}
\keyword{internal}
