#' Exhibit features of a dataset by printing them to the R console
#' @description exhibit.specific_models() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Candidate models lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Candidate models lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @return NULL
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
exhibit.specific_models <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T) 
{
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = c("Reference", "Name", "Control", "Familty", 
            "Function", "Start", "Predict", "Transformation", 
            "Binomial", "Acronym (Fixed)", "Acronymy (Mixed)", 
            "Type (Mixed)", "With"))
}
#' @rdname exhibit-methods
#' @aliases exhibit,specific_models-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("specific_models", package = "specific"), exhibit.specific_models)
#' Exhibit features of a dataset by printing them to the R console
#' @description exhibit.specific_predictors() is an exhibit method that exhibits features of a class instance by printing to console. This method is implemented for the Candidate predictors lookup table The function is called for its side effects and does not return a value.
#' @param x An instance of Candidate predictors lookup table
#' @param caption_1L_chr Caption (a character vector of length one), Default: NULL
#' @param mkdn_tbl_ref_1L_chr Markdown table reference (a character vector of length one), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param use_lbls_as_col_nms_1L_lgl Use labels as column names (a logical vector of length one), Default: T
#' @return NULL
#' @rdname exhibit-methods
#' @export 
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
exhibit.specific_predictors <- function (x, caption_1L_chr = NULL, mkdn_tbl_ref_1L_chr = NULL, 
    output_type_1L_chr = "HTML", use_lbls_as_col_nms_1L_lgl = T) 
{
    x %>% ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr, 
        mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr, output_type_1L_chr = output_type_1L_chr, 
        use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl, 
        var_desc_chr = c("Variable", "Description", "Minimum", 
            "Maximum", "Class", "Increment", "Function", "Scaling", 
            "Covariate"))
}
#' @rdname exhibit-methods
#' @aliases exhibit,specific_predictors-method
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", methods::className("specific_predictors", package = "specific"), exhibit.specific_predictors)
#' 
#' Exhibit features of a dataset by printing them to the R console
#' @name exhibit-SpecificProject
#' @description exhibit method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param captions_chr Captions (a character vector), Default: character(0)
#' @param method_chr Method (a character vector), Default: c("pearson", "spearman")
#' @param mkdn_tbl_refs_chr Markdown table references (a character vector), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param timepoints_int Timepoints (an integer vector), Default: NA
#' @param type_1L_chr Type (a character vector of length one), Default: 'data'
#' @param what_1L_chr What (a character vector of length one), Default: 'correlation'
#' @param ... Additional arguments
#' @return NULL
#' @rdname exhibit-methods
#' @aliases exhibit,SpecificProject-method
#' @export 
#' @importFrom purrr map
#' @importFrom youthvars transform_ds_for_tstng make_corstars_tbl_xx
#' @importFrom stats setNames
#' @importFrom ready4show print_from_chunk
#' @importFrom ready4 exhibit
methods::setMethod("exhibit", "SpecificProject", function (x, captions_chr = character(0), method_chr = c("pearson", 
    "spearman"), mkdn_tbl_refs_chr = NULL, output_type_1L_chr = "HTML", 
    timepoints_int = NA_integer_, type_1L_chr = "data", what_1L_chr = "correlation", 
    ...) 
{
    if (type_1L_chr == "data") {
        if (what_1L_chr == "correlation") {
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
                depnt_var_max_val_1L_dbl = x@b_SpecificParameters@depnt_var_min_max_dbl[2], 
                candidate_predrs_chr = x@b_SpecificParameters@candidate_predrs_chr, 
                round_var_nm_1L_chr = ifelse("timepoint_var_nm_1L_chr" %in% 
                  slotNames(x@a_YouthvarsProfile), x@a_YouthvarsProfile@timepoint_var_nm_1L_chr, 
                  NA_character_), round_val_1L_chr = ifelse("timepoint_vals_chr" %in% 
                  slotNames(x@a_YouthvarsProfile), x@a_YouthvarsProfile@timepoint_vals_chr[timepoints_int[.x]], 
                  NA_character_)) %>% youthvars::make_corstars_tbl_xx(caption_1L_chr = captions_chr[.x], 
                mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr[.x], 
                method_chr = method_chr, result_chr = output_type_1L_chr))
        }
    }
    else {
        heading_grps_chr <- NULL
        if (!identical(what_1L_chr, "")) 
            object_xx <- procure(x, what_1L_chr = what_1L_chr)
        if (type_1L_chr == "results") {
            if (what_1L_chr %in% c("mdl_cmprsn", "fxd_sngl_cmprsn", 
                "fxd_full_cmprsn")) 
                object_xx <- as.data.frame(object_xx)
            if (what_1L_chr %in% c("mdl_cmprsn", "fxd_sngl_cmprsn")) 
                names(object_xx) <- c("Model", "R-Squared", "RMSE", 
                  "MAE", "R-Squared", "RMSE", "MAE")
            if (what_1L_chr %in% c("fxd_full_cmprsn")) 
                names(object_xx) <- c("Model", "R-Squared", "AIC", 
                  "BIC", "Significant terms")
            if (what_1L_chr %in% c("mdl_cmprsn", "fxd_sngl_cmprsn")) {
                heading_grps_chr <- c(1, 3, 3) %>% stats::setNames(c(" ", 
                  paste0("Training model fit (averaged over ", 
                    x@b_SpecificParameters@folds_1L_int, " folds)"), 
                  paste0("Testing model fit (averaged over ", 
                    x@b_SpecificParameters@folds_1L_int, " folds)")))
                if (identical(captions_chr, character(0))) 
                  captions_chr <- NULL
            }
            if (what_1L_chr == "predr_cmprsn") {
                if (identical(captions_chr, character(0))) 
                  captions_chr <- NULL
            }
        }
        object_xx %>% ready4show::print_from_chunk(output_type_1L_chr = output_type_1L_chr, 
            caption_1L_chr = captions_chr, heading_grps_chr = heading_grps_chr, 
            mkdn_tbl_ref_1L_chr = mkdn_tbl_refs_chr, ...)
    }
})
