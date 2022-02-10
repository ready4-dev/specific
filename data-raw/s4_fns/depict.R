depict_SpecificProject <- function(x,
                                  mdl_idxs_int = NULL,
                                  output_type_1L_chr = "HTML",
                                  plt_indcs_int = NULL){
  if(is.null(mdl_idxs_int)){
    mdl_idxs_int <- 1
  }
  if(is.null(plt_indcs_int))
    plt_indcs_int <- 1:5
  # Add logic for SpecificFixed and above clss or add slot for predr_var_nm_1L_chr
  predr_var_nm_1L_chr <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$predr_var_nm_1L_chr
  plt_paths_chr <- list.files(x@paths_chr, recursive = T)[list.files(x@paths_chr, recursive = T) %>%
                                                            purrr::map_lgl(~endsWith(.x,".png"))]
  plt_paths_ls <- x@b_SpecificParameters@candidate_mdls_lup$short_name_chr[mdl_idxs_int] %>%
    purrr::map(~{
      pfx_1L_chr <- paste0("A_Candidate_Mdls_Cmprsn/A_RT_",
                           predr_var_nm_1L_chr,
                           "_",
                           .x)
      paths_chr <- plt_paths_chr[plt_paths_chr %>%
                                   purrr::map_lgl(~startsWith(.x,pfx_1L_chr))]
      paths_chr <- paste0(x@paths_chr,
                          "/",
                          paths_chr)
      paths_chr[purrr::map_int(c("_LNR_CMPRSN",
                                 "_AUTOPLT",
                                 "_PRED_DNSTY",
                                 "_SIM_DNSTY",
                                 "_PRED_SCTR")[plt_indcs_int],
                               ~ {
                                 idx_1L_int <- which(paths_chr %>% endsWith(paste0(.x,".png")))
                                 idx_1L_int <- ifelse(identical(idx_1L_int,integer(0)),
                                                      NA_integer_,
                                                      idx_1L_int)
                                 idx_1L_int
                               }) %>% purrr::discard(is.na)]
    })
  knitr::include_graphics({{plt_paths_ls[[1]]}})
}
depict_SpecificSynopsis <- function(x,
                                     base_height = 13,
                                     base_size_1L_dbl = 30,
                                     depnt_var_desc_1L_chr = NA_character_,
                                     labels_chr = c("A","B","C","D"),
                                     label_x_1L_dbl = 0.2,
                                     label_y_1L_dbl = 0.9,
                                     label_size_1L_dbl = 30,
                                     mdl_idxs_int = 1:2,
                                     timepoint_old_nms_chr = NA_character_,
                                     timepoint_new_nms_chr = NA_character_,
                                     use_png_fls_1L_lgl = F,
                                     what_1L_chr = "composite_mdl",
                                     write_1L_lgl = F){
  plt <- NULL
  outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                         x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
  if(!is.na(timepoint_new_nms_chr[1])){
    correspondences_lup <- ready4show::ready4show_correspondences() %>%
      renew(old_nms_chr = timepoint_old_nms_chr,
            new_nms_chr = timepoint_new_nms_chr)
  }else{
    correspondences_lup
  }
  if(what_1L_chr == "composite_mdl"){
    plt <- make_cmpst_sctr_and_dnst_plt(outp_smry_ls,
                                        base_size_1L_dbl =  base_size_1L_dbl,
                                        correspondences_lup = correspondences_lup,
                                        depnt_var_desc_1L_chr = depnt_var_desc_1L_chr,
                                        labels_chr = labels_chr,
                                        label_x_1L_dbl = label_x_1L_dbl,
                                        label_y_1L_dbl = label_y_1L_dbl,
                                        label_size_1L_dbl = label_size_1L_dbl,
                                        mdl_idxs_int = mdl_idxs_int,
                                        use_png_fls_1L_lgl = use_png_fls_1L_lgl)
    if(write_1L_lgl){
      cowplot::save_plot(paste0(outp_smry_ls$path_to_write_to_1L_chr,
                                "/dens_and_sctr.png"),
                         plt,
                         base_height = base_height)
    }
  }
  return(plt)
}
