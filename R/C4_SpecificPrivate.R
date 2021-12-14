#' SpecificPrivate
#' 
#' Analysis outputs not intended for public dissemination.
#' 
#' @slot private_outp_ls Private output (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificPrivate-class
#' @rdname SpecificPrivate-class
#' @export SpecificPrivate
#' @exportClass SpecificPrivate
SpecificPrivate <- methods::setClass("SpecificPrivate",
contains = "Ready4Private",
slots = c(private_outp_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(private_outp_ls = list(list())))


methods::setValidity(methods::className("SpecificPrivate"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
