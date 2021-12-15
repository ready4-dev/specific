#' SpecificProject
#' 
#' Modelling project input parameters and results output.
#' 
#' @include C4_SpecificResults.R
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot b_SpecificParameters  (an instance of the SpecificParameters class)
#' @slot c_SpecificResults  (an instance of the SpecificResults class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificProject-class
#' @rdname SpecificProject-class
#' @export SpecificProject
#' @exportClass SpecificProject
SpecificProject <- methods::setClass("SpecificProject",
contains = "Ready4Module",
slots = c(a_YouthvarsProfile = "YouthvarsProfile",b_SpecificParameters = "SpecificParameters",c_SpecificResults = "SpecificResults",dissemination_1L_chr = "character"),
prototype =  list(a_YouthvarsProfile = youthvars::YouthvarsProfile(),b_SpecificParameters = SpecificParameters(),c_SpecificResults = SpecificResults()))


methods::setValidity(methods::className("SpecificProject"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
