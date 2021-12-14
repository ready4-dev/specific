#' SpecificShareable
#' 
#' Analysis outputs intended for public dissemination.
#' 
#' @slot shareable_outp_ls Shareable output (a list)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name SpecificShareable-class
#' @rdname SpecificShareable-class
#' @export SpecificShareable
#' @exportClass SpecificShareable
SpecificShareable <- methods::setClass("SpecificShareable",
contains = "Ready4Public",
slots = c(shareable_outp_ls = "list",dissemination_1L_chr = "character"),
prototype =  list(shareable_outp_ls = list(list())))


methods::setValidity(methods::className("SpecificShareable"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
