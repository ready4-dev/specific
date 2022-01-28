#' SpecificConverter
#' 
#' Container for seed objects used for creating SpecificModels modules.
#' 
#' @slot a_ScorzProfile  (an instance of the ScorzProfile class)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificConverter-class
#' @rdname SpecificConverter-class
#' @export SpecificConverter
#' @exportClass SpecificConverter
SpecificConverter <- methods::setClass("SpecificConverter",
contains = "Ready4Module",
slots = c(a_ScorzProfile = "ScorzProfile",dissemination_1L_chr = "character"),
prototype =  list(a_ScorzProfile = scorz::ScorzProfile()))


methods::setValidity(methods::className("SpecificConverter"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
