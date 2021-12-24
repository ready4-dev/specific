#' 
#' Renew (update) an object
#' @name renew-SpecificProject
#' @description renew method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param new_val_xx New value (an output object of multiple potential types)
#' @param type_1L_chr Type (a character vector of length one), Default: 'results'
#' @param what_1L_chr What (a character vector of length one), Default: 'prefd_mdls'
#' @return x (An object of class SpecificProject)
#' @rdname renew-methods
#' @aliases renew,SpecificProject-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "SpecificProject", function (x, new_val_xx, type_1L_chr = "results", what_1L_chr = "prefd_mdls") 
{
    if (type_1L_chr == "results") {
        if (what_1L_chr == "prefd_mdls") 
            x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_mdl_types_chr <- new_val_xx
    }
    if (what_1L_chr == "prefd_covars") 
        x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_covars_chr <- new_val_xx
    return(x)
})
