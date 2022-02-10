enhance_SpecificSynopsis <- function(x,
                                     y_SpecificMixed,
                                     z_Ready4useRepos,
                                     depnt_var_nms_chr = NA_character_,
                                     what_1L_chr = "shareable_outp_ls",
                                     with_1L_chr = "results_ls"){
  if(what_1L_chr == "shareable_outp_ls"){
    outp_smry_ls <- x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls
    outp_smry_ls$results_ls <- manufacture(x,
                                           y_SpecificMixed,
                                           z_Ready4useRepos,
                                           depnt_var_nms_chr = depnt_var_nms_chr,
                                           what_1L_chr = "results_ls")
    x <- renewSlot(x,
                   "b_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                   outp_smry_ls)
  }
  return(x)
}