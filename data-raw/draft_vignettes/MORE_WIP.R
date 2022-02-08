# After results ls have been added
## Depict
outp_smry_ls <- append(Z@b_SpecificResults@a_SpecificShareable@shareable_outp_ls,
                       Z@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
correspondences_lup <- ready4show::ready4show_correspondences() %>%
  renew(old_nms_chr = c("BL","FUP"),
        new_nms_chr = c("Baseline","Follow-up"))
cmpst_plt <- make_cmpst_sctr_and_dnst_plt(outp_smry_ls,
                                          base_size_1L_dbl = 30,
                                          correspondences_lup = correspondences_lup,
                                          labels_chr = c("A","B","C","D"),
                                          label_x_1L_dbl = 0,
                                          label_y_1L_dbl = 0,
                                          label_size_1L_dbl = 30,
                                          mdl_idcs_int = 1:2,
                                          use_png_fls_1L_lgl = F)
##
cowplot::save_plot(paste0(outp_smry_ls$path_to_write_to_1L_chr,
                          "/dens_and_sctrYY.png"),
                   cmpst_plt,
                   base_height = 13)

ggpubr::ggarrange(plt_ls[[1]], plt_ls[[2]], plt_ls[[3]], plt_ls[[4]],
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
# START LOOP


