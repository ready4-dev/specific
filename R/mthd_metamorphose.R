#' 
#' Metamorphose from one ready4 framework module (or sub-module) class to another
#' @name metamorphose-SpecificMixed
#' @description metamorphose method applied to SpecificMixed
#' @param x An object of class SpecificMixed
#' @param to_1L_chr To (a character vector of length one), Default: 'SpecificSynopsis'
#' @param rmd_fl_nms_ls R Markdown file names (a list), Default: NULL
#' @return Y (a ready4 S4)
#' @rdname metamorphose-methods
#' @aliases metamorphose,SpecificMixed-method
#' @export 
#' @importFrom ready4show make_rmd_fl_nms_ls
#' @importFrom stringi stri_replace_last_regex
#' @importFrom ready4 metamorphose
methods::setMethod("metamorphose", "SpecificMixed", function (x, to_1L_chr = "SpecificSynopsis", rmd_fl_nms_ls = NULL) 
{
    if (is.null(rmd_fl_nms_ls)) {
        rmd_fl_nms_ls <- ready4show::make_rmd_fl_nms_ls("Lngl_Mdls_HTML", 
            pdf_fl_nm_1L_chr = "Lngl_Mdls_PDF", word_fl_nm_1L_chr = "Lngl_Mdls_Word")
    }
    if (to_1L_chr == "SpecificSynopsis") {
        y_r4 <- SpecificSynopsis(b_SpecificResults = x@c_SpecificResults, 
            c_SpecificParameters = x@b_SpecificParameters, d_YouthvarsProfile = x@a_YouthvarsProfile)
        y_r4 <- renewSlot(y_r4, "a_Ready4showPaths@outp_data_dir_1L_chr", 
            x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr %>% 
                stringi::stri_replace_last_regex("/Output", ""))
        y_r4 <- renewSlot(y_r4, "rmd_fl_nms_ls", rmd_fl_nms_ls)
    }
    return(y_r4)
})
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
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj metamorphose
#' @importFrom stringi stri_replace_first_fixed stri_replace_all_fixed stri_replace_last_fixed
methods::setMethod("metamorphose", "SpecificConverter", function (x, paths_chr = NA_character_) 
{
    domain_labels_chr <- x@a_ScorzProfile@domain_wtd_var_nms_chr %>% 
        purrr::map_chr(~ready4::get_from_lup_obj(x@a_ScorzProfile@instrument_dict_r3, 
            match_var_nm_1L_chr = "var_nm_chr", match_value_xx = .x, 
            target_var_nm_1L_chr = "var_desc_chr") %>% stringi::stri_replace_first_fixed("EuroQol (EQ-5D) - ", 
            "") %>% stringi::stri_replace_all_fixed("Adult Score Dimension 1 - ", 
            "") %>% stringi::stri_replace_last_fixed(" item", 
            ""))
    x_SpecificModels <- SpecificModels(a_YouthvarsProfile = procureSlot(x@a_ScorzProfile, 
        "a_YouthvarsProfile"), b_SpecificParameters = SpecificParameters(depnt_var_nm_1L_chr = x@a_ScorzProfile@total_wtd_var_nm_1L_chr, 
        domain_labels_chr = domain_labels_chr, itm_labels_chr = x@a_ScorzProfile@itm_labels_chr, 
        itm_prefix_1L_chr = x@a_ScorzProfile@itm_prefix_1L_chr, 
        total_unwtd_var_nm_1L_chr = x@a_ScorzProfile@total_unwtd_var_nm_1L_chr), 
        paths_chr = paths_chr)
    return(x_SpecificModels)
})
