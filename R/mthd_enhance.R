#' 
#' Enhance a dataset by adding new elements
#' @name enhance-SpecificSynopsis
#' @description enhance method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param depnt_var_nms_chr Dependent variable names (a character vector), Default: 'NA'
#' @param depnt_var_min_val_1L_dbl Dependent variable minimum value (a double vector of length one), Default: numeric(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'shareable_outp_ls'
#' @param with_1L_chr With (a character vector of length one), Default: 'results_ls'
#' @param ... Additional arguments
#' @return x (An object of class SpecificSynopsis)
#' @rdname enhance-methods
#' @aliases enhance,SpecificSynopsis-method
#' @export 
#' @importFrom ready4 enhance
methods::setMethod("enhance", "SpecificSynopsis", function (x, depnt_var_nms_chr = NA_character_, depnt_var_min_val_1L_dbl = numeric(0), 
    what_1L_chr = "shareable_outp_ls", with_1L_chr = "results_ls", 
    ...) 
{
    if (what_1L_chr == "shareable_outp_ls") {
        outp_smry_ls <- x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls
        if (with_1L_chr == "results_ls") {
            outp_smry_ls$results_ls <- manufacture(x, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, 
                depnt_var_nms_chr = depnt_var_nms_chr, what_1L_chr = "results_ls")
        }
        x <- renewSlot(x, "b_SpecificResults@a_SpecificShareable@shareable_outp_ls", 
            outp_smry_ls)
    }
    return(x)
})
