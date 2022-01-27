#' 
#' Manufacture a (non ready4 framework) object
#' @name manufacture-SpecificModels
#' @description manufacture method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param what_1L_chr What (a character vector of length one), Default: 'ds_descvs_ls'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @aliases manufacture,SpecificModels-method
#' @export 
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", "SpecificModels", function (x, what_1L_chr = "ds_descvs_ls") 
{
    if (what_1L_chr %in% c("ds_descvs_ls", "ds_smry_ls")) 
        object_xx <- make_ds_descvs_ls(candidate_predrs_chr = x@b_SpecificParameters@candidate_predrs_chr, 
            candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr, 
            cohort_descv_var_nms_chr = x@b_SpecificParameters@descv_var_nms_chr, 
            dictionary_tb = x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3, 
            id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr, 
            is_fake_1L_lgl = x@b_SpecificParameters@fake_1L_lgl, 
            msrmnt_date_var_nm_1L_chr = x@b_SpecificParameters@msrmnt_date_var_nm_1L_chr, 
            round_var_nm_1L_chr = x@a_YouthvarsProfile@timepoint_var_nm_1L_chr, 
            round_vals_chr = x@a_YouthvarsProfile@timepoint_vals_chr, 
            utl_wtd_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr, 
            maui_item_pfx_1L_chr = x@b_SpecificParameters@itm_prefix_1L_chr, 
            utl_unwtd_var_nm_1L_chr = x@b_SpecificParameters@total_unwtd_var_nm_1L_chr)
    if (what_1L_chr == ("ds_smry_ls")) 
        object_xx <- object_xx %>% make_analysis_ds_smry_ls(candidate_covar_nms_chr = x@b_SpecificParameters@candidate_covars_chr, 
            predictors_lup = x@b_SpecificParameters@predictors_lup)
    if (what_1L_chr == "mdl_smry_ls") {
        if (is.na(x@b_SpecificParameters@candidate_mdls_chr)) {
            mdl_types_chr <- NULL
        }
        else {
            mdl_types_chr <- x@b_SpecificParameters@candidate_mdls_chr
        }
        if (is.na(x@b_SpecificParameters@candidate_mdl_pfcs_chr)) {
            choose_from_pfx_chr <- NULL
        }
        else {
            choose_from_pfx_chr <- x@b_SpecificParameters@candidate_mdl_pfcs_chr
        }
        object_xx <- make_mdl_smry_ls(mdl_types_lup = x@b_SpecificParameters@candidate_mdls_lup, 
            mdl_types_chr = mdl_types_chr, choose_from_pfx_chr = choose_from_pfx_chr, 
            folds_1L_int = x@b_SpecificParameters@folds_1L_int, 
            max_nbr_of_boruta_mdl_runs_int = x@b_SpecificParameters@max_mdl_runs_1L_int)
    }
    return(object_xx)
})
