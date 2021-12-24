renew_SpecificProject <- function(x,
                                  new_val_xx,
                                  type_1L_chr = "results",
                                  what_1L_chr = "prefd_mdls"
                                  ){
  if(type_1L_chr == "results"){
    if(what_1L_chr == "prefd_mdls") # Make version for SpecificFixed and above
      x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_mdl_types_chr <- new_val_xx
  }
  if(what_1L_chr == "prefd_covars")
    x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$prefd_covars_chr <- new_val_xx
  return(x)
}
