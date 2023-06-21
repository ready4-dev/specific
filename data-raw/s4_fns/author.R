author_SpecificModels <- function(x,
                                  consent_1L_chr = "",
                                  digits_1L_int = 3L,
                                  prefd_mdl_types_chr = NULL,
                                  reference_1L_int = NULL,
                                  what_1L_chr = "all",
                                  x_labels_chr = character(0),
                                  ...){
  series_1L_lgl <- x@a_YouthvarsProfile %>% inherits("YouthvarsSeries")
  if(what_1L_chr %in% c("all","descriptives","models","workspace")){
    session_data_ls <- sessionInfo()
    if(what_1L_chr %in% c("workspace","all")){
      if(!is.null(reference_1L_int)){
        transform_paths_ls <- list(fn = transform_paths_ls_for_scndry,
                                   args_ls = list(reference_1L_int = reference_1L_int)) # FOR SECONDARY
      }else{
        transform_paths_ls <- NULL
      }
      path_params_ls <- make_path_params_ls()
      path_params_ls$path_from_top_level_1L_chr <- x@paths_chr
      path_params_ls$use_fake_data_1L_lgl <- x@b_SpecificParameters@fake_1L_lgl
      paths_ls <- path_params_ls %>%
        ready4show::make_paths_ls(depth_1L_int = ifelse(is.null(transform_paths_ls),
                                                        1,
                                                        2))
      if(!is.null(transform_paths_ls)){
        paths_ls <- rlang::exec(transform_paths_ls$fn,
                                paths_ls,
                                !!!transform_paths_ls$args_ls)
      }
      paths_ls <- ready4show::write_all_outp_dirs(paths_ls = paths_ls,
                                                  consent_1L_chr = consent_1L_chr)
      x@b_SpecificParameters@paths_ls <- paths_ls
    }
    if(what_1L_chr %in% c("descriptives","all")){
      ds_descvs_ls <- manufacture(x,
                                  what_1L_chr = "ds_descvs_ls")
      descv_tbl_ls <- youthvars::write_descv_tbls(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                  consent_1L_chr = consent_1L_chr,
                                                  descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr,
                                                  ds_descvs_ls = ds_descvs_ls,
                                                  nbr_of_digits_1L_int = digits_1L_int,
                                                  participation_var_1L_chr = if(!series_1L_lgl){character(0)}else{x@a_YouthvarsProfile@participation_var_1L_chr},
                                                  predictors_lup = x@b_SpecificParameters@predictors_lup)
      descv_plts_paths_ls <- youthvars::write_descv_plots(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                                                          consent_1L_chr = consent_1L_chr,
                                                          ds_descvs_ls = ds_descvs_ls,
                                                          descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr,
                                                          lbl_nms_chr = x@b_SpecificParameters@itm_labels_chr,
                                                          x_labels_chr = x_labels_chr,
                                                          maui_domains_pfxs_1L_chr = hutils::longest_prefix(x@b_SpecificParameters@domain_labels_chr))

    }
    if(what_1L_chr %in% c("models","all")){
      x <- investigate(x)
      if(!is.null(prefd_mdl_types_chr))
        x <- renew(x,
                   new_val_xx = prefd_mdl_types_chr,
                   type_1L_chr = "results",
                   what_1L_chr = "prefd_mdls")
      x <- investigate(x)
      if(!is.null(prefd_covars_chr))
        x <- renew(x,
                   new_val_xx = prefd_covars_chr,
                   type_1L_chr = "results",
                   what_1L_chr = "prefd_covars")
      x <- investigate(x)
      x <- investigate(x)
      x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_data_ls <- session_data_ls
      }
    }else{
      x <- methods::callNextMethod()
      }
  return(x)
}
author_SpecificProject <- function(x,
                                   consent_1L_chr = "",
                                   fl_nm_1L_chr = "I_ALL_OUTPUT_",
                                   path_1L_chr = NA_character_,
                                   type_1L_chr = "results",
                                   what_1L_chr = "public",
                                   ...){
  if(type_1L_chr %in% c("purge_all","purge_write")){
    outp_smry_ls_ls <- manufactureSlot(x,
                                       "c_SpecificResults",
                                       what_1L_chr = "indexed_shareable")
    purrr::walk(outp_smry_ls_ls,
                ~ write_to_delete_mdl_fls(.x,
                                          consent_1L_chr = consent_1L_chr))
  }
  if(type_1L_chr != "purge_all"){
    path_1L_chr <- ifelse(is.na(path_1L_chr),
                          x@paths_chr[1],
                          path_1L_chr)
    if(type_1L_chr == "results"){
      if(what_1L_chr %in% c("public", "purge_write"))
        output_xx <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls
      if(what_1L_chr == "private")
        output_xx <- x@c_SpecificResults@b_SpecificPrivate@private_outp_ls
      if(what_1L_chr == "all")
        output_xx <- append(x@c_SpecificResults@b_SpecificPrivate@private_outp_ls,
                            x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls)
    }
    if(type_1L_chr == "parameters")
      output_xx <- x@b_SpecificParameters
    if(type_1L_chr %in% c("project","purge_write")){
      if(what_1L_chr == "public"){
        output_xx <- x
        output_xx@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- output_xx@a_YouthvarsProfile@a_Ready4useDyad@ds_tb[0,]
        output_xx@c_SpecificResults@b_SpecificPrivate <- SpecificPrivate()
        output_xx@paths_chr <- NA_character_
      }
    }
    ready4::write_with_consent(consented_fn = saveRDS,
                               prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                      paste0(fl_nm_1L_chr, ".RDS"),
                                                      " to ",
                                                      path_1L_chr,
                                                      "?"),
                               consent_1L_chr = consent_1L_chr,
                               consented_args_ls = list(object = output_xx,
                                                        file = paste0(path_1L_chr, "/", fl_nm_1L_chr,".RDS")),
                               consented_msg_1L_chr = paste0("File ",
                                                             paste0(fl_nm_1L_chr, ".RDS"),
                                                             " has been written to ",
                                                             path_1L_chr,
                                                             "."),
                               declined_msg_1L_chr = "Write request cancelled - no new files have been written.")
  }
  return(x)
}
author_SpecificSynopsis <- function(x,
                                    args_ls = NULL,
                                    consent_1L_chr = "",
                                    reference_1L_int = NA_integer_,
                                    type_1L_chr = "Report",
                                    what_1L_chr = "Catalogue",
                                    ...){
  if(what_1L_chr == "Catalogue"){
    outp_smry_ls_ls <- manufacture(x@b_SpecificResults,
                                   what_1L_chr = "indexed_shareable")
    refs_int <- 1:length(outp_smry_ls_ls)
    if(!is.na(reference_1L_int)){
      outp_smry_ls_ls <- outp_smry_ls_ls[reference_1L_int]
      refs_int <- reference_1L_int
    }
    ctlg_nms_chr <- purrr::map2_chr(outp_smry_ls_ls,
                                    refs_int,
                                    ~ {
                                      fl_nm_1L_chr <- paste0("AAA_TTU_MDL_CTG",
                                                             ifelse(.y==1,
                                                                    "",
                                                                    paste0("-",(.y-1))))
                                      authorReport(x %>%
                                                     renewSlot("b_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                                                               .x),
                                                   args_ls = args_ls,
                                                   consent_1L_chr = consent_1L_chr,
                                                   fl_nm_1L_chr = fl_nm_1L_chr,
                                                   what_1L_chr = what_1L_chr)
                                      fl_nm_1L_chr
                                    }
    )
  }
  return(x)
}
