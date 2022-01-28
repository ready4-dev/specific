metamorphose_SpecificConverter <- function(x,
                                           paths_chr = NA_character_){
  x_SpecificModels <- SpecificModels(a_YouthvarsProfile = procureSlot(x@a_ScorzProfile,"a_YouthvarsProfile"),
                                     b_SpecificParameters = SpecificParameters(depnt_var_nm_1L_chr = x@a_ScorzProfile@total_wtd_var_nm_1L_chr,
                                                                               itm_labels_chr = x@a_ScorzProfile@itm_labels_chr,
                                                                               itm_prefix_1L_chr = x@a_ScorzProfile@itm_prefix_1L_chr,
                                                                               total_unwtd_var_nm_1L_chr = x@a_ScorzProfile@total_unwtd_var_nm_1L_chr),
                                     paths_chr = paths_chr)
  return(x_SpecificModels)
}
