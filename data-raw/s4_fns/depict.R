depict_SpecificProject <- function(x,
                                  mdl_indcs_int = NULL,
                                  output_type_1L_chr = "HTML",
                                  plt_indcs_int = NULL,
                                  ...){
  if(is.null(mdl_indcs_int)){
    mdl_indcs_int <- 1
  }
  if(is.null(plt_indcs_int))
    plt_indcs_int <- 1:5
  # Add logic for SpecificFixed and above clss or add slot for predr_var_nm_1L_chr
  predr_var_nm_1L_chr <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$predr_var_nm_1L_chr
  plt_paths_chr <- list.files(x@paths_chr, recursive = T)[list.files(x@paths_chr, recursive = T) %>%
                                                            purrr::map_lgl(~endsWith(.x,".png"))]
  plt_paths_ls <- x@b_SpecificParameters@candidate_mdls_lup$short_name_chr[mdl_indcs_int] %>%
    purrr::map(~{
      pfx_1L_chr <- paste0("A_Candidate_Mdls_Cmprsn/A_RT_",
                           predr_var_nm_1L_chr,
                           "_",
                           .x)
      paths_chr <- plt_paths_chr[plt_paths_chr %>%
                                   purrr::map_lgl(~stringr::str_detect(.x,pfx_1L_chr))]
      paths_chr <- paste0(x@paths_chr, "/", paths_chr)
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
                                    axis_text_sclg_1L_dbl = 1.5,
                                    axis_title_sclg_1L_dbl = 2,
                                    base_height_1L_dbl = 13,
                                    base_size_1L_dbl = 30,
                                    consent_1L_chr = "",
                                    depnt_var_desc_1L_chr = NA_character_,
                                    depnt_var_min_val_1L_dbl = numeric(0),
                                    dim_plot_heights_int = c(10L, 1L),
                                    dim_plot_log_log_tfmn_1L_lgl = F,
                                    dim_plot_rows_cols_pair_int = c(3L,2L),
                                    labels_chr = c("A","B","C","D"),
                                    label_x_1L_dbl = 0.2,
                                    label_y_1L_dbl = 0.9,
                                    label_size_1L_dbl = 30,
                                    legend_sclg_1L_dbl = 2,
                                    mdl_indcs_int = 1:2,
                                    rel_heights_dbl = c(4,10,1),
                                    scale_dbl = c(0.9,0.9,0.9),
                                    timepoint_old_nms_chr = NA_character_,
                                    timepoint_new_nms_chr = NA_character_,
                                    use_png_fls_1L_lgl = F,
                                    utl_plot_label_1L_chr = " ",
                                    utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6,
                                                                      height_1L_dbl = 4),
                                    what_1L_chr = "composite_mdl",
                                    write_1L_lgl = F,
                                    x_labels_chr = character(0),
                                    y_label_1L_chr = " ",
                                    ...){
  plt <- NULL
  outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                         x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
  if(!is.na(timepoint_new_nms_chr[1])){
    correspondences_lup <- ready4show::ready4show_correspondences() %>%
      renew(old_nms_chr = timepoint_old_nms_chr,
            new_nms_chr = timepoint_new_nms_chr)
  }else{
    correspondences_lup <- NULL
  }
  if(what_1L_chr == "composite_mdl"){
    plt <- make_cmpst_sctr_and_dnst_plt(outp_smry_ls,
                                        base_size_1L_dbl =  base_size_1L_dbl,
                                        correspondences_lup = correspondences_lup,
                                        depnt_var_desc_1L_chr = depnt_var_desc_1L_chr,
                                        depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                        labels_chr = labels_chr,
                                        label_x_1L_dbl = label_x_1L_dbl,
                                        label_y_1L_dbl = label_y_1L_dbl,
                                        label_size_1L_dbl = label_size_1L_dbl,
                                        mdl_indcs_int = mdl_indcs_int,
                                        use_png_fls_1L_lgl = use_png_fls_1L_lgl)
    write_path_1L_chr <- paste0(outp_smry_ls$path_to_write_to_1L_chr, "/dens_and_sctr.png")
  }
  if(what_1L_chr == "composite_utl"){
    ds_descvs_ls <- manufacture(x,
                                depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                what_1L_chr = "ds_descvs_ls")
    outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                           x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
    maui_domains_col_nms_chr <- x@c_SpecificParameters@domain_labels_chr
    first_plt <- rlang::exec(youthvars::make_var_by_round_plt,
                             !!!list(data_tb = outp_smry_ls$scored_data_tb,
                                     legend_sclg_1L_dbl = legend_sclg_1L_dbl,
                                     var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                     round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr,
                                     x_label_1L_chr = ds_descvs_ls$dictionary_tb %>%
                                       ready4::get_from_lup_obj(match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                                                match_var_nm_1L_chr = "var_nm_chr",
                                                                target_var_nm_1L_chr = "var_desc_chr",
                                                                evaluate_1L_lgl = F) %>% as.vector(),
                                     label_fill_1L_chr = utl_plot_label_1L_chr,
                                     axis_text_sclg_1L_dbl = axis_text_sclg_1L_dbl,
                                     axis_title_sclg_1L_dbl = axis_title_sclg_1L_dbl,
                                     y_label_1L_chr = y_label_1L_chr))
    second_plt <- rlang::exec(youthvars::make_sub_tot_plts,
                              !!!list(data_tb = outp_smry_ls$scored_data_tb,
                                      add_legend_1L_lgl = F,
                                      axis_text_sclg_1L_dbl = axis_text_sclg_1L_dbl,
                                      axis_title_sclg_1L_dbl = axis_title_sclg_1L_dbl,
                                      col_nms_chr = maui_domains_col_nms_chr,
                                      legend_sclg_1L_dbl = legend_sclg_1L_dbl,
                                      plot_rows_cols_pair_int = dim_plot_rows_cols_pair_int,
                                      round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr,
                                      heights_int = dim_plot_heights_int,
                                      make_log_log_tfmn_1L_lgl = dim_plot_log_log_tfmn_1L_lgl,
                                      x_labels_chr = x_labels_chr,
                                      y_label_1L_chr = y_label_1L_chr))
    legend_ls <- cowplot::get_legend(first_plt)
    plt <- cowplot::plot_grid(first_plt +
                                ggplot2::theme(legend.position="none"),
                              second_plt,
                              legend_ls,
                              nrow = 3L,
                              rel_heights = rel_heights_dbl,
                              scale = scale_dbl)
    write_path_1L_chr <- paste0(x@a_Ready4showPaths@outp_data_dir_1L_chr, "/Output/_Descriptives/combined_utl.png")
  }
  if(write_1L_lgl){
    ready4::write_with_consent(consented_fn = cowplot::save_plot,
                               prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                      write_path_1L_chr,
                                                      "?"),
                               consent_1L_chr = consent_1L_chr,
                               consented_args_ls = list(filename = write_path_1L_chr,
                                                        plot = plt,
                                                        base_height = base_height_1L_dbl),
                               consented_msg_1L_chr = paste0("File ",
                                                             write_path_1L_chr,
                                                             " has been written."),
                               declined_msg_1L_chr = "Write request cancelled - no new files have been written.")
  }
  return(plt)
}
