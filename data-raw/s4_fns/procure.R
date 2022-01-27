procure_SpecificProject <- function(x,
                                    type_1L_chr = "results",
                                    what_1L_chr = "prefd_mdls"){
  if(type_1L_chr == "results"){
    results_ls <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls
    if(!is.null(results_ls$mdl_smry_ls)){
      results_ls <- results_ls$mdl_smry_ls
    }
    if(what_1L_chr == "fxd_full_cmprsn")
      object_xx <- results_ls$mdls_with_covars_smry_tb
    if(what_1L_chr == "fxd_sngl_cmprsn")
      object_xx <- results_ls$smry_of_mdl_sngl_predrs_tb
    if(what_1L_chr == "mdl_cmprsn")
      object_xx <- results_ls$smry_of_sngl_predr_mdls_tb
    if(what_1L_chr == "predr_cmprsn")
      object_xx <- results_ls$predr_cmprsn_tb
    if(what_1L_chr == "prefd_covars")
      object_xx <- results_ls$prefd_covars_chr
    if(what_1L_chr == "prefd_mdls")
      object_xx <- results_ls$prefd_mdl_types_chr
    if(what_1L_chr == "signt_covars")
      object_xx <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$signt_covars_chr
  }
  if(type_1L_chr == "data"){
    #if(what_1L_chr == "correlation")
      object_xx <- x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb
  }
  return(object_xx)
}
