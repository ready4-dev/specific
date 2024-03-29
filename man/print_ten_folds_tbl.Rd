% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_print.R
\name{print_ten_folds_tbl}
\alias{print_ten_folds_tbl}
\title{Print ten folds table}
\usage{
print_ten_folds_tbl(
  params_ls,
  caption_1L_chr,
  mkdn_tbl_ref_1L_chr,
  column_1_width_1L_chr = "20em",
  ref_1L_int = 1
)
}
\arguments{
\item{params_ls}{Parameters (a list)}

\item{caption_1L_chr}{Caption (a character vector of length one)}

\item{mkdn_tbl_ref_1L_chr}{Markdown table reference (a character vector of length one)}

\item{column_1_width_1L_chr}{Column 1 width (a character vector of length one), Default: '20em'}

\item{ref_1L_int}{Reference (an integer vector of length one), Default: 1}
}
\description{
print_ten_folds_tbl() is a Print function that prints output to console Specifically, this function implements an algorithm to print ten folds table. The function is called for its side effects and does not return a value.
}
