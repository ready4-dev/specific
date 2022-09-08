#' 
#' Ratify that a dataset meets validity criteria
#' @name ratify-SpecificModels
#' @description ratify method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param class_fn_1L_chr Class function (a character vector of length one), Default: 'as.numeric'
#' @param prototype_lup Prototype (a lookup table), Default: NULL
#' @param scndry_anlys_params_ls Secondary analysis parameters (a list), Default: NULL
#' @return x (An object of class SpecificModels)
#' @rdname ratify-methods
#' @aliases ratify,SpecificModels-method
#' @export 
#' @importFrom youthvars add_interval_var add_participation_var
#' @importFrom ready4 renew ratify
methods::setMethod("ratify", "SpecificModels", function (x, class_fn_1L_chr = "as.numeric", prototype_lup = NULL, 
    scndry_anlys_params_ls = NULL) 
{
    x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb %>% 
        youthvars::add_interval_var(id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr, 
            msrmnt_date_var_nm_1L_chr = ifelse(!is.na(x@b_SpecificParameters@msrmnt_date_var_nm_1L_chr), 
                x@b_SpecificParameters@msrmnt_date_var_nm_1L_chr, 
                "d_interview_date")) %>% youthvars::add_participation_var(id_var_nm_1L_chr = x@a_YouthvarsProfile@id_var_nm_1L_chr, 
        fup_round_nbr_1L_int = length(x@a_YouthvarsProfile@timepoint_vals_chr))
    if (is.na(x@a_YouthvarsProfile@participation_var_1L_chr)) {
        x@a_YouthvarsProfile@participation_var_1L_chr <- "participation"
    }
    x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 <- x@a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3 %>% 
        ready4::renew(var_nm_chr = c("bl_date_dtm", "interval_dbl", 
            x@a_YouthvarsProfile@participation_var_1L_chr), var_ctg_chr = c("Temporal", 
            "Temporal", "Temporal"), var_desc_chr = c("Date of baseline assessment", 
            "Interval between baseline and follow-up assessments", 
            "Rounds participated in"), var_type_chr = c("date", 
            "interval", "character"))
    x <- renewSlot(x, "a_YouthvarsProfile@a_Ready4useDyad", type_1L_chr = "label")
    x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb %>% 
        transform_mdl_vars_with_clss(predictors_lup = x@b_SpecificParameters@predictors_lup, 
            depnt_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr, 
            prototype_lup = prototype_lup, class_fn_1L_chr = class_fn_1L_chr)
    input_params_ls <- manufacture(x, what_1L_chr = "input_params_ls", 
        scndry_anlys_params_ls = scndry_anlys_params_ls)
    x <- renewSlot(x, "a_YouthvarsProfile@a_Ready4useDyad@ds_tb", 
        input_params_ls$params_ls$ds_tb) %>% renewSlot("a_YouthvarsProfile@a_Ready4useDyad@dictionary_r3", 
        input_params_ls$params_ls$ds_descvs_ls$dictionary_tb) %>% 
        renewSlot("b_SpecificParameters@candidate_covars_chr", 
            input_params_ls$params_ls$candidate_covar_nms_chr) %>% 
        renewSlot("b_SpecificParameters@descv_var_nms_chr", input_params_ls$params_ls$ds_descvs_ls$cohort_descv_var_nms_chr) %>% 
        renewSlot("b_SpecificParameters@candidate_predrs_chr", 
            input_params_ls$params_ls$ds_descvs_ls$candidate_predrs_chr) %>% 
        renewSlot("b_SpecificParameters@predictors_lup", input_params_ls$params_ls$predictors_lup) %>% 
        renewSlot("b_SpecificParameters@depnt_var_nm_1L_chr", 
            input_params_ls$params_ls$ds_descvs_ls$utl_wtd_var_nm_1L_chr) %>% 
        renewSlot("c_SpecificResults@a_SpecificShareable@shareable_outp_ls", 
            list(rename_lup = input_params_ls$rename_lup))
    return(x)
})
