authorData_SpecificMixed <- function(x,
                                     consent_1L_chr = "",
                                     depnt_var_min_val_1L_dbl = numeric(0),
                                     title_1L_chr = "An R model object",
                                     what_1L_chr = "Shareable",
                                     ...){
  if(what_1L_chr == "Shareable"){
    results_ls <- purrr::map(manufacture(x@c_SpecificResults,
                                         what_1L_chr = "indexed_shareable"),
                             ~{
                               outp_smry_ls <- append(procureSlot(x,
                                                                  "c_SpecificResults@b_SpecificPrivate@private_outp_ls"),
                                                      .x)
                               outp_smry_ls <- outp_smry_ls %>%
                                 write_shareable_mdls(new_dir_nm_1L_chr = "G_Shareable",
                                                      consent_1L_chr = consent_1L_chr,
                                                      depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                      shareable_title_detail_1L_chr = title_1L_chr)

                               outp_smry_ls[-1]
                             })
    x <- renewSlot(x,
                   "c_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                   append(results_ls[[1]],
                          results_ls[-1]))
  }
  return(x)
}
