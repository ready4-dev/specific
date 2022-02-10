#' 
#' Enhance a ready4 framework module (or sub-module) with new data items
#' @name enhance-SpecificSynopsis
#' @description enhance method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param y_SpecificMixed PARAM_DESCRIPTION
#' @param z_Ready4useRepos PARAM_DESCRIPTION
#' @param depnt_var_nms_chr Dependent variable names (a character vector), Default: 'NA'
#' @param what_1L_chr What (a character vector of length one), Default: 'shareable_outp_ls'
#' @param with_1L_chr With (a character vector of length one), Default: 'results_ls'
#' @return x (An object of class SpecificSynopsis)
#' @rdname enhance-methods
#' @aliases enhance,SpecificSynopsis-method
#' @export 
#' @importFrom ready4 enhance
methods::setMethod("enhance", "SpecificSynopsis", function (x, y_SpecificMixed, z_Ready4useRepos, depnt_var_nms_chr = NA_character_, 
    what_1L_chr = "shareable_outp_ls", with_1L_chr = "results_ls") 
{
    if (what_1L_chr == "shareable_outp_ls") {
        outp_smry_ls <- x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls
        if (with_1L_chr == "results_ls") {
            outp_smry_ls$results_ls <- manufacture(x, depnt_var_nms_chr = depnt_var_nms_chr, 
                what_1L_chr = "results_ls")
        }
        x <- renewSlot(x, "b_SpecificResults@a_SpecificShareable@shareable_outp_ls", 
            outp_smry_ls)
    }
    return(x)
})
