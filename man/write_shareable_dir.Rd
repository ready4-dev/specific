% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_write.R
\name{write_shareable_dir}
\alias{write_shareable_dir}
\title{Write shareable directory}
\usage{
write_shareable_dir(
  outp_smry_ls,
  consent_1L_chr = "",
  consent_indcs_int = 1L,
  new_dir_nm_1L_chr = "G_Shareable",
  options_chr = c("Y", "N"),
  sub_dirs_chr = c("Ingredients", "Models", "Table_Predn_Tools")
)
}
\arguments{
\item{outp_smry_ls}{Output summary (a list)}

\item{consent_1L_chr}{Consent (a character vector of length one), Default: ''}

\item{consent_indcs_int}{Consent indices (an integer vector), Default: 1}

\item{new_dir_nm_1L_chr}{New directory name (a character vector of length one), Default: 'G_Shareable'}

\item{options_chr}{Options (a character vector), Default: c("Y", "N")}

\item{sub_dirs_chr}{Sub directories (a character vector), Default: c("Ingredients", "Models", "Table_Predn_Tools")}
}
\value{
Output directory (a character vector)
}
\description{
write_shareable_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write shareable directory. The function returns Output directory (a character vector).
}
\keyword{internal}
