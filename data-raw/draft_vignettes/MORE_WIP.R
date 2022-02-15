axis_text_sclg_1L_dbl = 1.5
axis_title_sclg_1L_dbl = 2
base_height_dbl = 13
dim_plot_heights_int = c(10L, 1L)
dim_plot_log_log_tfmn_1L_lgl = F
dim_plot_rows_cols_pair_int = c(3L,2L)
legend_sclg_1L_dbl = 2
rel_heights_dbl = c(4,10,1)
scale_dbl = c(0.9,0.9,0.9)
utl_plot_label_1L_chr = " "
utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6,
                                  height_1L_dbl = 4)
y_label_1L_chr = " "
ds_descvs_ls <- manufacture_SpecificSynopsis(x,
                                             what_1L_chr = "ds_descvs_ls")
outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                       x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
maui_domains_col_nms_chr <- x@c_SpecificParameters@domain_labels_chr
first_plt <- rlang::exec(make_var_by_round_plt,#youthvars::
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
second_plt <- rlang::exec(make_sub_tot_plts,#youthvars::
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
                                  y_label_1L_chr = y_label_1L_chr))
legend_ls <- cowplot::get_legend(first_plt)

combined_plt <- cowplot::plot_grid(first_plt + theme(legend.position="none"),
                                   second_plt,
                                   legend_ls,
                                   nrow = 3L,
                                   rel_heights = rel_heights_dbl,
                                   scale = scale_dbl)
cowplot::save_plot(paste0(x@a_Ready4showPaths@outp_data_dir_1L_chr,
                          "/Output/_Descriptives/combined_utl.png"),
                   combined_plt,
                   base_height = base_height_dbl)

