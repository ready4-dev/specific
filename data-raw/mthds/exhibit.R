exhibit.specific_models <- function(x,
                                    caption_1L_chr = NULL,
                                    mkdn_tbl_ref_1L_chr = NULL,
                                    output_type_1L_chr = "HTML",
                                    use_lbls_as_col_nms_1L_lgl = T,
                                    ...){
 x %>% #
    ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr,
                                 mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                                 output_type_1L_chr = output_type_1L_chr,
                                 use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                                 var_desc_chr = c("Reference",
                                                  "Name",
                                                  "Control",
                                                  "Familty",
                                                  "Function",
                                                  "Start",
                                                  "Predict",
                                                  "Transformation",
                                                  "Binomial",
                                                  "Acronym (Fixed)",
                                                  "Acronymy (Mixed)",
                                                  "Type (Mixed)",
                                                  "With"),
                                 ...)
}
exhibit.specific_predictors <- function(x,
                                    caption_1L_chr = NULL,
                                    mkdn_tbl_ref_1L_chr = NULL,
                                    output_type_1L_chr = "HTML",
                                    use_lbls_as_col_nms_1L_lgl = T,
                                    ...){
  x %>% #
    ready4show::print_from_chunk(caption_1L_chr = caption_1L_chr,
                                 mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                                 output_type_1L_chr = output_type_1L_chr,
                                 use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                                 var_desc_chr = c("Variable",
                                                  "Description",
                                                  "Minimum",
                                                  "Maximum",
                                                  "Class",
                                                  "Increment",
                                                  "Function",
                                                  "Scaling",
                                                  "Covariate"),
                                 ...)
}
