#' SpecificResults
#' 
#' Analysis results.
#' 
#' @include C4_SpecificShareable.R C4_SpecificPrivate.R
#' @slot a_SpecificShareable  (an instance of the SpecificShareable class)
#' @slot b_SpecificPrivate  (an instance of the SpecificPrivate class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificResults-class
#' @rdname SpecificResults-class
#' @export SpecificResults
#' @exportClass SpecificResults
SpecificResults <- methods::setClass("SpecificResults",
contains = "Ready4Module",
slots = c(a_SpecificShareable = "SpecificShareable",b_SpecificPrivate = "SpecificPrivate",dissemination_1L_chr = "character"),
prototype =  list(a_SpecificShareable = SpecificShareable(),b_SpecificPrivate = SpecificPrivate()))


methods::setValidity(methods::className("SpecificResults"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
