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
