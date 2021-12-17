#' 
#' Exhibit features of a class instance by printing to console
#' @name exhibit-SpecificProject
#' @description exhibit method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param captions_chr Captions (a character vector), Default: character(0)
#' @param method_chr Method (a character vector), Default: c("pearson", "spearman")
#' @param mkdn_tbl_refs_chr Markdown table references (a character vector), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param timepoints_int Timepoints (an integer vector), Default: NA
#' @param type_1L_chr Type (a character vector of length one), Default: 'dataset'
#' @param what_1L_chr What (a character vector of length one), Default: 'correlation'
#' @param ... Additional arguments
#' @return NULL
#' @rdname exhibit-methods
#' @aliases exhibit,SpecificProject-method
#' @export 
#' @importFrom purrr map
#' @importFrom youthvars transform_ds_for_tstng make_corstars_tbl_xx
#' @importFrom ready4show print_table
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", "SpecificProject", function (x, captions_chr = character(0), method_chr = c("pearson", 
    "spearman"), mkdn_tbl_refs_chr = NULL, output_type_1L_chr = "HTML", 
    timepoints_int = NA_integer_, type_1L_chr = "dataset", what_1L_chr = "correlation", 
    ...) 
{
    if (type_1L_chr == "dataset" & what_1L_chr == "correlation") {
        if (is.na(timepoints_int)) {
            if ("timepoint_vals_chr" %in% slotNames(x@a_YouthvarsProfile)) {
                timepoints_int <- 1:length(x@a_YouthvarsProfile@timepoint_vals_chr) %>% 
                  as.integer()
            }
            else {
                timepoints_int <- 1
            }
        }
        if (identical(character(0), captions_chr)) {
            captions_chr <- paste0("Correlations", ifelse("timepoint_vals_chr" %in% 
                slotNames(x@a_YouthvarsProfile), paste0(" at ", 
                x@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int]), 
                ""))
        }
        1:length(timepoints_int) %>% purrr::map(~youthvars::transform_ds_for_tstng(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
            depnt_var_nm_1L_chr = x@b_SpecificParameters@depnt_var_nm_1L_chr, 
            depnt_var_max_val_1L_dbl = x@b_SpecificParameters@depnt_var_max_val_1L_dbl, 
            candidate_predrs_chr = x@b_SpecificParameters@candidate_predrs_chr, 
            round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% 
                slotNames(x@a_YouthvarsProfile), x@a_YouthvarsProfile@timepoint_var_nm_1L_chr, 
                NA_character_), round_val_1L_chr = ifelse("timepoint_vals_chr" %in% 
                slotNames(x@a_YouthvarsProfile), x@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int[.x]], 
                NA_character_)) %>% youthvars::make_corstars_tbl_xx(caption_1L_chr = captions_chr[.x], 
            mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[.x], method_chr = method_chr, 
            result_chr = output_type_1L_chr))
    }
    else {
        if (!identical(what_1L_chr, "")) 
            object_xx <- procure(x, what_1L_chr = what_1L_chr)
        if (type_1L_chr == "results") {
            if (what_1L_chr == "mdl_cmprsn") {
                if (identical(captions_chr, character(0))) 
                  captions_chr <- "Comparison of candidate models using highest correlated predictor"
            }
            if (what_1L_chr == "predr_cmprsn") {
                if (identical(captions_chr, character(0))) 
                  captions_chr <- "Comparison of all candidate predictors using preferred model"
            }
        }
        object_xx %>% ready4show::print_table(output_type_1L_chr = output_type_1L_chr, 
            caption_1L_chr = captions_chr, mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr, 
            ...)
    }
})
