#' 
#' Metamorphose from one ready4 framework module (or sub-module) class to another
#' @name metamorphose-SpecificConverter
#' @description metamorphose method applied to SpecificConverter
#' @param x An object of class SpecificConverter
#' @param paths_chr Paths (a character vector), Default: 'NA'
#' @return X (Modelling project dataset, input parameters and model comparison results.)
#' @rdname metamorphose-methods
#' @aliases metamorphose,SpecificConverter-method
#' @export 
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", "SpecificConverter", function (x, paths_chr = NA_character_) 
{
    x_SpecificModels <- SpecificModels(a_YouthvarsProfile = procureSlot(x@a_ScorzProfile, 
        "a_YouthvarsProfile"), b_SpecificParameters = SpecificParameters(depnt_var_nm_1L_chr = x@a_ScorzProfile@total_wtd_var_nm_1L_chr, 
        itm_labels_chr = x@a_ScorzProfile@itm_labels_chr, itm_prefix_1L_chr = x@a_ScorzProfile@itm_prefix_1L_chr, 
        total_unwtd_var_nm_1L_chr = x@a_ScorzProfile@total_unwtd_var_nm_1L_chr), 
        paths_chr = paths_chr)
    return(x_SpecificModels)
})
