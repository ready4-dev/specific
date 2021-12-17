#' SpecificProject
#' 
#' Modelling project dataset, parameters and results.
#' 
#' @include C4_SpecificResults.R C4_SpecificParameters.R
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot b_SpecificParameters  (an instance of the SpecificParameters class)
#' @slot c_SpecificResults  (an instance of the SpecificResults class)
#' @slot paths_chr Paths (a character vector)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificProject-class
#' @rdname SpecificProject-class
#' @export SpecificProject
#' @exportClass SpecificProject
SpecificProject <- methods::setClass("SpecificProject",
contains = "Ready4Module",
slots = c(a_YouthvarsProfile = "YouthvarsProfile",b_SpecificParameters = "SpecificParameters",c_SpecificResults = "SpecificResults",paths_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_YouthvarsProfile = youthvars::YouthvarsProfile(),b_SpecificParameters = SpecificParameters(),c_SpecificResults = SpecificResults(),paths_chr = NA_character_))


methods::setValidity(methods::className("SpecificProject"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
