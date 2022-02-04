# Path modification if scndry

#
outp_smry_ls <- append(private_res_ls,
                       procureSlot(Z,
                                   "b_SpecificResults@a_SpecificShareable@shareable_outp_ls"))
outp_smry_ls <- outp_smry_ls %>%
  write_shareable_mdls(new_dir_nm_1L_chr = "G_Shareable",
                       shareable_title_detail_1L_chr = "An R model object") #params_ls$dv_mdl_desc_1L_chr

outp_smry_ls$dv_ls <- list(dv_nm_1L_chr = A@dv_nm_1L_chr,
                           ds_url_1L_chr = A@dv_ds_nm_1L_chr,
                           parent_dv_dir_1L_chr = "H_Dataverse") #paths_ls$dv_dir_1L_ch
Z <- renewSlot(Z,
               "b_SpecificResults@a_SpecificShareable@shareable_outp_ls",
               outp_smry_ls,
)
