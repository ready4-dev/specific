rel_heights_dbl = c(4,10)
scale_dbl = c(0.9,0.9)
base_height_dbl = 13
dim_plot_heights_int = c(10L, 1L)
dim_plot_log_log_tfmn_1L_lgl = F
dim_plot_rows_cols_pair_int = c(3L,2L)
utl_plot_label_1L_chr = "Round"
utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6,
                                  height_1L_dbl = 4)
ds_descvs_ls <- manufacture(x,
                            what_1L_chr = "ds_descvs_ls")
outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                       x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
maui_domains_col_nms_chr <- x@c_SpecificParameters@domain_labels_chr
combined_plt <- cowplot::plot_grid(rlang::exec(youthvars::make_var_by_round_plt,
                                               !!!list(data_tb = outp_smry_ls$scored_data_tb,
                                                            var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                                            round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr,
                                                            x_label_1L_chr = ds_descvs_ls$dictionary_tb %>%
                                                              ready4::get_from_lup_obj(match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr,
                                                                                       match_var_nm_1L_chr = "var_nm_chr",
                                                                                       target_var_nm_1L_chr = "var_desc_chr",
                                                                                       evaluate_1L_lgl = F) %>% as.vector(),
                                                       label_fill_1L_chr = utl_plot_label_1L_chr)) +
                                     ggplot2::theme(legend.position = 'none'),
                                   rlang::exec(youthvars::make_sub_tot_plts,
                                               !!!list(data_tb = outp_smry_ls$scored_data_tb,
                                                       col_nms_chr = maui_domains_col_nms_chr,
                                                       plot_rows_cols_pair_int = dim_plot_rows_cols_pair_int,
                                                       round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr,
                                                       heights_int = dim_plot_heights_int,
                                                       make_log_log_tfmn_1L_lgl = dim_plot_log_log_tfmn_1L_lgl)),
                                   nrow = 2L,
                                   rel_heights = rel_heights_dbl,
                                   scale = scale_dbl
)
descv_plts_paths_ls$combined_utl <- paste0(descv_outp_dir_1L_chr,
                                           "/combined_utl.png")
cowplot::save_plot(descv_plts_paths_ls$combined_utl,
                   combined_plt,
                   base_height = base_height_dbl)

