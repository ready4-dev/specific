authorData_SpecificMixed <- function(x,
                                     title_1L_chr = "An R model object",
                                     what_1L_chr = "Shareable"){
  if(what_1L_chr == "Shareable"){
    # shareable_outp_ls <- procureSlot(x,
    #                                  "c_SpecificResults@a_SpecificShareable@shareable_outp_ls")
    # secondary_chr <- names(shareable_outp_ls)[startsWith(names(shareable_outp_ls),"secondary_")]
    # if(!identical(secondary_chr,character(0))){
    #   primary_ls <- shareable_outp_ls[names(shareable_outp_ls)!=secondary_chr]
    #   secondary_ls <- shareable_outp_ls[names(shareable_outp_ls)==secondary_chr]
    #   results_ls <- append(list(primary_ls = primary_ls[(names(primary_ls))[!names(primary_ls) %>% duplicated()]]),
    #                        secondary_ls %>%
    #                          purrr::map(~.x[(.x %>% names())[!.x %>% names() %>% duplicated()]]))
    # }else{
    #   results_ls <- list(primary_ls = primary_ls)
    # }
    results_ls <- purrr::map(manufacture(x@c_SpecificResults,
                                         what_1L_chr = "indexed_shareable"),#results_ls,
               ~{
                 outp_smry_ls <- append(procureSlot(x,
                                                    "c_SpecificResults@b_SpecificPrivate@private_outp_ls"),
                                        .x)
                 outp_smry_ls <- outp_smry_ls %>%
                   write_shareable_mdls(new_dir_nm_1L_chr = "G_Shareable",
                                        shareable_title_detail_1L_chr = title_1L_chr) #params_ls$dv_mdl_desc_1L_chr

                 outp_smry_ls[-1]
                 })


    x <- renewSlot(x,
                   "c_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                   outp_smry_ls[-1])
  }
  return(x)
}
