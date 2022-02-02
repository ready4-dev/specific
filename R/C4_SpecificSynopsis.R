#' SpecificSynopsis
#' 
#' Results and manuscript metadata.
#' 
#' @include C4_SpecificResults.R
#' @slot b_SpecificResults  (an instance of the SpecificResults class)
#' @slot a_Ready4showPaths  (an instance of the Ready4showPaths class)
#' @slot abstract_args_ls Abstract arguments (a list)
#' @slot authors_r3 Authors (a ready4 S3)
#' @slot background_1L_chr Background (a character vector of length one)
#' @slot coi_1L_chr Conflict of interest (a character vector of length one)
#' @slot conclusion_1L_chr Conclusion (a character vector of length one)
#' @slot correspondences_r3 Correspondences (a ready4 S3)
#' @slot digits_int Digits (an integer vector)
#' @slot ethics_1L_chr Ethics (a character vector of length one)
#' @slot fl_nm_1L_chr File name (a character vector of length one)
#' @slot figures_in_body_lgl Figures in body (a logical vector)
#' @slot funding_1L_chr Funding (a character vector of length one)
#' @slot institutes_r3 Institutes (a ready4 S3)
#' @slot interval_chr Interval (a character vector)
#' @slot keywords_chr Keywords (a character vector)
#' @slot outp_formats_chr Output formats (a character vector)
#' @slot rmd_fl_nms_ls R Markdown file names (a list)
#' @slot sample_desc_1L_chr Sample description (a character vector of length one)
#' @slot tables_in_body_lgl Tables in body (a logical vector)
#' @slot title_1L_chr Title (a character vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4show
#' @name SpecificSynopsis-class
#' @rdname SpecificSynopsis-class
#' @export SpecificSynopsis
#' @exportClass SpecificSynopsis
SpecificSynopsis <- methods::setClass("SpecificSynopsis",
contains = "Ready4showSynopsis",
slots = c(b_SpecificResults = "SpecificResults",a_Ready4showPaths = "Ready4showPaths",abstract_args_ls = "list",authors_r3 = "ready4show_authors",background_1L_chr = "character",coi_1L_chr = "character",conclusion_1L_chr = "character",correspondences_r3 = "ready4show_correspondences",digits_int = "integer",ethics_1L_chr = "character",fl_nm_1L_chr = "character",figures_in_body_lgl = "logical",funding_1L_chr = "character",institutes_r3 = "ready4show_institutes",interval_chr = "character",keywords_chr = "character",outp_formats_chr = "character",rmd_fl_nms_ls = "list",sample_desc_1L_chr = "character",tables_in_body_lgl = "logical",title_1L_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(b_SpecificResults = SpecificResults()))


methods::setValidity(methods::className("SpecificSynopsis"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
