metamorphose_SpecificConverter <- function(x,
                                           paths_chr = NA_character_,
                                           ...){
  domain_labels_chr <- x@a_ScorzProfile@domain_wtd_var_nms_chr
  x_SpecificModels <- SpecificModels(a_YouthvarsProfile = procureSlot(x@a_ScorzProfile,"a_YouthvarsProfile"),
                                     b_SpecificParameters = SpecificParameters(depnt_var_nm_1L_chr = x@a_ScorzProfile@total_wtd_var_nm_1L_chr,
                                                                               domain_labels_chr = domain_labels_chr,
                                                                               itm_labels_chr = x@a_ScorzProfile@itm_labels_chr,
                                                                               itm_prefix_1L_chr = x@a_ScorzProfile@itm_prefix_1L_chr,
                                                                               total_unwtd_var_nm_1L_chr = x@a_ScorzProfile@total_unwtd_var_nm_1L_chr),
                                     paths_chr = paths_chr)
  return(x_SpecificModels)
}
metamorphose_SpecificMixed <- function(x,
                                       to_1L_chr = "SpecificSynopsis",
                                       rmd_fl_nms_ls = NULL,
                                       ...){
  if(is.null(rmd_fl_nms_ls)){
    rmd_fl_nms_ls <- ready4show::make_rmd_fl_nms_ls("Lngl_Mdls_HTML",
                                                    pdf_fl_nm_1L_chr = "Lngl_Mdls_PDF",
                                                    word_fl_nm_1L_chr = "Lngl_Mdls_Word")
  }
  if(to_1L_chr == "SpecificSynopsis"){
    y_r4 <- SpecificSynopsis(b_SpecificResults = x@c_SpecificResults,
                             c_SpecificParameters = x@b_SpecificParameters,
                             d_YouthvarsProfile = x@a_YouthvarsProfile)
    y_r4 <- renewSlot(y_r4,
                      "a_Ready4showPaths@outp_data_dir_1L_chr",
                      x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr %>% stringi::stri_replace_last_regex("/Output",""))#X@paths_chr
    y_r4 <- renewSlot(y_r4,
                      "rmd_fl_nms_ls",
                      rmd_fl_nms_ls
    )
  }
  return(y_r4)
}
