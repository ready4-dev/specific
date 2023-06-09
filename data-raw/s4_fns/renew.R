renew_SpecificProject <- function(x,
                                  new_val_xx,
                                  type_1L_chr = "results",
                                  what_1L_chr = "prefd_mdls",
                                  ...){
  if(type_1L_chr == "results"){
    if(what_1L_chr == "prefd_mdls") # Make version for SpecificFixed and above
      x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_mdl_types_chr <- new_val_xx
  }
  if(what_1L_chr == "prefd_covars"){
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_covars_chr <- new_val_xx
  }
  if(what_1L_chr %in% c("base","dummys","levels")){
    x@a_YouthvarsProfile@a_Ready4useDyad <- renew(x@a_YouthvarsProfile@a_Ready4useDyad,
                                                  new_val_xx = new_val_xx, type_1L_chr = what_1L_chr)
  }
  return(x)
}
renew_SpecificMixed <- function(x,
                                new_val_xx = NULL,
                                a_Ready4useRepos,
                                type_1L_chr = "results",
                                what_1L_chr = "dv_ls",
                                ...){
  dv_ls <- list(dv_nm_1L_chr = procureSlot(a_Ready4useRepos,
                                           "dv_nm_1L_chr"),
                             ds_url_1L_chr = procureSlot(a_Ready4useRepos,
                                                         "dv_ds_nm_1L_chr"),
                             parent_dv_dir_1L_chr = paste0(x@b_SpecificParameters@paths_ls$output_data_dir_1L_chr,
                                                             "/H_Dataverse"))
  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$dv_ls <- dv_ls
  return(x)
}
