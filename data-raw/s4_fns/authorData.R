authorData_SpecificMixed <- function(x,
                                     title_1L_chr = "An R model object",
                                     what_1L_chr = "Shareable"){
  if(wwhat_1L_chr == "Shareable"){
    outp_smry_ls <- append(procureSlot(x,
                                       "c_SpecificResults@b_SpecificPrivate@private_outp_ls"),
                           procureSlot(x,
                                       "c_SpecificResults@a_SpecificShareable@shareable_outp_ls"))
    outp_smry_ls <- outp_smry_ls %>%
      write_shareable_mdls(new_dir_nm_1L_chr = "G_Shareable",
                           shareable_title_detail_1L_chr = title_1L_chr) #params_ls$dv_mdl_desc_1L_chr
    x <- renewSlot(x,
                   "c_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                   outp_smry_ls[-1])
  }
  return(x)
}
