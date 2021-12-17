#' SpecificMixed
#' 
#' Modelling project dataset, input parameters and complete mixed models results.
#' 
#' @include C4_SpecificProject.R C4_SpecificResults.R C4_SpecificParameters.R
#' @slot a_YouthvarsProfile  (an instance of the YouthvarsProfile class)
#' @slot b_SpecificParameters  (an instance of the SpecificParameters class)
#' @slot c_SpecificResults  (an instance of the SpecificResults class)
#' @slot paths_chr Paths (a character vector)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name SpecificMixed-class
#' @rdname SpecificMixed-class
#' @export SpecificMixed
#' @exportClass SpecificMixed
SpecificMixed <- methods::setClass("SpecificMixed",
contains = "SpecificProject",
slots = c(a_YouthvarsProfile = "YouthvarsProfile",b_SpecificParameters = "SpecificParameters",c_SpecificResults = "SpecificResults",paths_chr = "character",dissemination_1L_chr = "character"),
prototype =  list(a_YouthvarsProfile = youthvars::YouthvarsProfile(),b_SpecificParameters = SpecificParameters(),c_SpecificResults = SpecificResults(),paths_chr = NA_character_))


methods::setValidity(methods::className("SpecificMixed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
