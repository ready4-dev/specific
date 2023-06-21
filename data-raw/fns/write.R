write_analyses <- function(input_params_ls,
                           abstract_args_ls = NULL,
                           combinations_1L_lgl = F,
                           consent_1L_chr = "",
                           consent_indcs_int = 1L,
                           cores_1L_int = 1L,
                           depnt_var_min_val_1L_dbl = numeric(0),
                           existing_predrs_ls = NULL,
                           max_nbr_of_covars_1L_int = integer(0),
                           options_chr = c("Y", "N"),
                           start_at_int = c(2,1)){
  ready4show::write_report(params_ls = input_params_ls$params_ls,
                           paths_ls = input_params_ls$path_params_ls$paths_ls,
                           rprt_nm_1L_chr = "AAA_PMRY_ANLYS_MTH",
                           abstract_args_ls = abstract_args_ls,
                           consent_1L_chr = consent_1L_chr,
                           consent_indcs_int = consent_indcs_int,
                           header_yaml_args_ls = input_params_ls$header_yaml_args_ls,
                           options_chr = options_chr)
  if(!is.null(input_params_ls$scndry_anlys_params_ls)){
    write_secondary_analyses(input_params_ls,
                             combinations_1L_lgl = combinations_1L_lgl,
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             cores_1L_int = cores_1L_int,
                             depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                             existing_predrs_ls = existing_predrs_ls,
                             max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int,
                             options_chr = options_chr)

  }
}
write_box_cox_tfmn <- function (data_tb,
                                predr_var_nm_1L_chr,
                                path_to_write_to_1L_chr,
                                consent_1L_chr = "",
                                consent_indcs_int = 1L,
                                depnt_var_nm_1L_chr = "utl_total_w",# Remove default
                                covar_var_nms_chr = NA_character_,
                                fl_nm_pfx_1L_chr = "A_RT",
                                height_1L_dbl = 6,
                                mdl_types_lup = NULL,
                                options_chr = c("Y", "N"),
                                start_1L_chr = NULL,
                                width_1L_dbl = 6)
{
  if (is.null(mdl_types_lup))
    utils::data("mdl_types_lup", envir = environment())
  mdl <- make_mdl(data_tb, depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                  predr_var_nm_1L_chr = predr_var_nm_1L_chr, covar_var_nms_chr = covar_var_nms_chr,
                  mdl_type_1L_chr = "OLS_NTF", mdl_types_lup = mdl_types_lup,
                  start_1L_chr = start_1L_chr)
  path_to_plot_1L_chr <- ready4show::write_mdl_plt_fl(plt_fn = MASS::boxcox,
                                                      consent_1L_chr = consent_1L_chr,
                                                      consent_indcs_int = consent_indcs_int,
                                                      fn_args_ls = list(mdl, plotit = T),
                                                      height_1L_dbl = height_1L_dbl,
                                                      options_chr = options_chr,
                                                      path_to_write_to_1L_chr = path_to_write_to_1L_chr,
                                                      plt_nm_1L_chr = paste0(fl_nm_pfx_1L_chr, "_", predr_var_nm_1L_chr, "_", "BOXCOX"),
                                                      width_1L_dbl = width_1L_dbl)
  return(path_to_plot_1L_chr)
}
write_mdl_cmprsn <- function(scored_data_tb,
                             ds_smry_ls,
                             mdl_smry_ls,
                             output_data_dir_1L_chr,
                             consent_1L_chr = "",
                             consent_indcs_int = 1L,
                             depnt_var_max_val_1L_dbl = 0.99999,
                             depnt_var_min_val_1L_dbl = 0.00001,
                             options_chr = c("Y", "N"),
                             seed_1L_int = 1234){
  bl_tb <- youthvars::transform_ds_for_tstng(scored_data_tb,
                                             depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                             candidate_predrs_chr = ds_smry_ls$candidate_predrs_chr,
                                             depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl,
                                             round_var_nm_1L_chr = if(identical(ds_smry_ls$round_var_nm_1L_chr,character(0))){NA_character_}else{ds_smry_ls$round_var_nm_1L_chr},
                                             round_val_1L_chr = if(identical(ds_smry_ls$round_var_nm_1L_chr,character(0))){NA_character_}else{ds_smry_ls$round_bl_val_1L_chr})
  ds_smry_ls$candidate_predrs_chr <- reorder_cndt_predrs_chr(ds_smry_ls$candidate_predrs_chr,
                                                             data_tb = bl_tb,
                                                             depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr)
  mdl_smry_ls <- add_prefd_predr_var_to_mdl_smry_ls(mdl_smry_ls,
                                                    ds_smry_ls = ds_smry_ls)
  mdl_smry_ls$smry_of_sngl_predr_mdls_tb <- write_sngl_predr_multi_mdls_outps(data_tb = bl_tb,
                                                                              consent_1L_chr = consent_1L_chr,
                                                                              consent_indcs_int = consent_indcs_int,
                                                                              dictionary_tb = ds_smry_ls$dictionary_tb,
                                                                              folds_1L_int = mdl_smry_ls$folds_1L_int,
                                                                              mdl_types_chr = mdl_smry_ls$mdl_types_chr,
                                                                              depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                                                              depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                              mdl_types_lup = mdl_smry_ls$mdl_types_lup,
                                                                              new_dir_nm_1L_chr =  "A_Candidate_Mdls_Cmprsn",
                                                                              options_chr = options_chr,
                                                                              predr_var_nm_1L_chr = mdl_smry_ls$predr_var_nm_1L_chr,
                                                                              predr_var_desc_1L_chr = mdl_smry_ls$predr_var_desc_1L_chr,
                                                                              predr_vals_dbl = mdl_smry_ls$predr_vals_dbl,
                                                                              path_to_write_to_1L_chr = output_data_dir_1L_chr)
  mdl_smry_ls$prefd_mdl_types_chr <- make_prefd_mdls_vec(mdl_smry_ls$smry_of_sngl_predr_mdls_tb,
                                                         choose_from_pfx_chr = mdl_smry_ls$choose_from_pfx_chr,
                                                         mdl_types_lup = mdl_smry_ls$mdl_types_lup)
  mdl_cmprsn_ls <- list(bl_tb = bl_tb,
                        ds_smry_ls = ds_smry_ls,
                        mdl_smry_ls = mdl_smry_ls)
  return(mdl_cmprsn_ls)
}
write_mdl_plts <- function (data_tb,
                            model_mdl,
                            predr_var_nm_1L_chr,
                            predr_var_desc_1L_chr,
                            predr_vals_dbl,
                            consent_1L_chr = "",
                            consent_indcs_int = 1L,
                            covar_var_nms_chr = NA_character_,
                            depnt_var_min_val_1L_dbl = numeric(0),
                            depnt_var_nm_1L_chr = "utl_total_w",
                            depnt_var_desc_1L_chr = "Utility score", # Remove defaults
                            family_1L_chr = NA_character_,
                            mdl_fl_nm_1L_chr = "OLS_NTF",
                            options_chr = c("Y", "N"),
                            path_to_write_to_1L_chr,
                            plt_indcs_int = 1:5,
                            predn_type_1L_chr = NULL,
                            tfmn_1L_chr = "NTF",
                            tfmn_for_bnml_1L_lgl = F)
{
  data_tb <- transform_ds_for_mdlng(data_tb, depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                    depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                    predr_var_nm_1L_chr = predr_var_nm_1L_chr, covar_var_nms_chr = covar_var_nms_chr)
  tfd_data_tb <- transform_data_tb_for_cmprsn(data_tb, model_mdl = model_mdl,
                                              depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                              predn_type_1L_chr = predn_type_1L_chr, tfmn_for_bnml_1L_lgl = tfmn_for_bnml_1L_lgl,
                                              family_1L_chr = family_1L_chr, tfmn_1L_chr = tfmn_1L_chr)
  if (1 %in% plt_indcs_int) {
    predn_ds_tb <- make_predn_ds_with_one_predr(model_mdl, depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                tfmn_1L_chr = tfmn_1L_chr, predr_var_nm_1L_chr = predr_var_nm_1L_chr,
                                                predr_vals_dbl = predr_vals_dbl, predn_type_1L_chr = predn_type_1L_chr)
  }else{
    predn_ds_tb <- NULL
  }
  purrr::pwalk(list(plt_fn_ls = list(plot_lnr_cmprsn,
                                     plot_auto_lm,
                                     plot_obsd_predd_dnst,
                                     plot_obsd_predd_dnst,
                                     plot_sctr_plt_cmprsn)[plt_indcs_int],
                    fn_args_ls_ls = list(list(data_tb = data_tb,
                                              predn_ds_tb = predn_ds_tb,
                                              predr_var_nm_1L_chr = predr_var_nm_1L_chr,
                                              predr_var_desc_1L_chr = predr_var_desc_1L_chr,
                                              depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                              depnt_var_desc_1L_chr = depnt_var_desc_1L_chr),
                                         list(model_mdl, which_dbl = 1:6, ncol_1L_int = 3L, label_size_1L_int = 3),
                                         list(tfd_data_tb = tfd_data_tb,
                                              depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                              depnt_var_desc_1L_chr = depnt_var_desc_1L_chr),
                                         list(tfd_data_tb = transform_data_tb_for_cmprsn(data_tb,
                                                                                         model_mdl = model_mdl,
                                                                                         depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                                                         new_data_is_1L_chr = ifelse(!4 %in% plt_indcs_int,
                                                                                                                     "Predicted", "Simulated"),
                                                                                         predn_type_1L_chr = NULL,
                                                                                         tfmn_for_bnml_1L_lgl = tfmn_for_bnml_1L_lgl,
                                                                                         family_1L_chr = family_1L_chr,
                                                                                         tfmn_1L_chr = tfmn_1L_chr),
                                              depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                              depnt_var_desc_1L_chr = depnt_var_desc_1L_chr,
                                              predd_val_var_nm_1L_chr = "Simulated"),
                                         list(tfd_data_tb = tfd_data_tb,
                                              depnt_var_nm_1L_chr = depnt_var_nm_1L_chr))[plt_indcs_int],
                    plt_nm_sfx_chr = c("_LNR_CMPRSN",
                                       "_AUTOPLT", "_PRED_DNSTY", "_SIM_DNSTY", "_PRED_SCTR")[plt_indcs_int],
                    size_ls = list(c(6, 6), c(4, 7), c(6, 6), c(6, 6), c(6,
                                                                         6))[plt_indcs_int]), ~ ready4show::write_mdl_plt_fl(plt_fn = ..1,
                                                                                                                             consent_1L_chr = consent_1L_chr,
                                                                                                                             consent_indcs_int = consent_indcs_int,
                                                                                                                             fn_args_ls = ..2,
                                                                                                                             options_chr = options_chr,
                                                                                                                             path_to_write_to_1L_chr = path_to_write_to_1L_chr,
                                                                                                                             plt_nm_1L_chr = paste0(mdl_fl_nm_1L_chr, ifelse(!is.na(covar_var_nms_chr[1]),
                                                                                                                                                                             paste("_", paste0(covar_var_nms_chr[1:min(length(covar_var_nms_chr),
                                                                                                                                                                                                                       3)], collapse = "")), ""), ..3), height_1L_dbl = ..4[1],
                                                                                                                             width_1L_dbl = ..4[2]))
}
write_mdl_smry_rprt <- function(input_params_ls = NULL,
                                abstract_args_ls = NULL,
                                consent_1L_chr = "",
                                consent_indcs_int = 1L,
                                dv_ds_nm_and_url_chr = NULL,
                                options_chr = c("Y", "N"),
                                rprt_lup = NULL,
                                rcrd_nm_1L_chr = "AAA_RPRT_WRTNG_MTH",
                                rprt_nm_1L_chr = "AAA_TTU_MDL_CTG", # Change default
                                start_at_int = c(2,1),
                                use_shareable_mdls_1L_lgl = F){
    header_yaml_args_ls <- input_params_ls$header_yaml_args_ls
    path_params_ls <- input_params_ls$path_params_ls
    output_format_ls <- input_params_ls$output_format_ls
    use_fake_data_1L_lgl <- input_params_ls$params_ls$use_fake_data_1L_lgl
    reference_int <- 0:(ifelse(is.null(input_params_ls$scndry_anlys_params_ls),
                               0,
                               length(input_params_ls$scndry_anlys_params_ls)))
  paths_ls <- path_params_ls$paths_ls
  if(is.null(rprt_lup))
    data("rprt_lup", package = "specific", envir = environment())
  rprt_lups_ls <- purrr::map(reference_int,
              ~{
                if(.x==0){
                  reference_1L_int <- NULL
                }else{
                  reference_1L_int <- .x
                }
                rprt_lup <- rprt_lup %>% transform_rprt_lup(add_suplry_rprt_1L_lgl = !is.null(reference_1L_int),
                                                            add_sharing_rprt_1L_lgl = T,
                                                            start_at_int = start_at_int,
                                                            reference_1L_int = reference_1L_int)
                if(is.null(reference_1L_int)){
                  path_to_outp_fl_1L_chr <- paste0(paths_ls$output_data_dir_1L_chr,"/I_ALL_OUTPUT_.RDS")
                  if(use_shareable_mdls_1L_lgl){
                    main_rprt_append_ls <- list(rltv_path_to_data_dir_1L_chr = "../Output/G_Shareable/Models")
                  }else{
                    main_rprt_append_ls <- NULL
                  }
                  rcrd_rprt_append_ls <- path_params_ls[1:2]
                }else{
                  path_to_outp_fl_1L_chr <- here::here(paths_ls$path_from_top_level_1L_chr,
                                                       paths_ls$write_to_dir_nm_1L_chr,
                                                       paste0("secondary_",reference_1L_int),
                                                       "Output","I_ALL_OUTPUT_.RDS")
                  main_rprt_append_ls <- list(existing_predrs_ls = readRDS(paste0(paths_ls$output_data_dir_1L_chr,
                                                                                  "/I_ALL_OUTPUT_.RDS")) %>%
                                                purrr::pluck("predr_vars_nms_ls"))
                  if(use_shareable_mdls_1L_lgl){
                    main_rprt_append_ls$rltv_path_to_data_dir_1L_chr <- "../Output/G_Shareable/Models"
                  }else{
                    main_rprt_append_ls$rltv_path_to_data_dir_1L_chr <- NULL
                  }
                  paths_ls <- transform_paths_ls_for_scndry(paths_ls,
                                                            reference_1L_int = reference_1L_int,
                                                            remove_prmry_1L_lgl = T)
                  rcrd_rprt_append_ls <- list(transform_paths_ls = list(fn = transform_paths_ls_for_scndry,
                                                                   args_ls = list(reference_1L_int = reference_1L_int,
                                                                                  remove_prmry_1L_lgl = T))) %>%
                    append(path_params_ls[1:2])

                }
                ready4show::write_rprt_with_rcrd(path_to_outp_fl_1L_chr = path_to_outp_fl_1L_chr,
                                                 abstract_args_ls = abstract_args_ls,
                                                 consent_1L_chr = consent_1L_chr,
                                                 consent_indcs_int = consent_indcs_int,
                                                 header_yaml_args_ls = header_yaml_args_ls,
                                                 main_rprt_append_ls = main_rprt_append_ls,
                                                 nbr_of_digits_1L_int = output_format_ls$supplementary_digits_1L_int,
                                                 options_chr = options_chr,
                                                 output_type_1L_chr = output_format_ls$supplementary_outp_1L_chr,
                                                 paths_ls = paths_ls,
                                                 rcrd_nm_1L_chr = rcrd_nm_1L_chr,
                                                 rcrd_rprt_append_ls = rcrd_rprt_append_ls,
                                                 reference_1L_int = reference_1L_int,
                                                 rprt_lup = rprt_lup,
                                                 rprt_nm_1L_chr = rprt_lup$rprt_nms_chr[purrr::map_lgl(rprt_lup$rprt_nms_chr,
                                                                                                       ~startsWith(.x,rprt_nm_1L_chr))],
                                                 rprt_output_type_1L_chr = output_format_ls$supplementary_outp_1L_chr,
                                                 start_at_int = start_at_int,
                                                 use_fake_data_1L_lgl = use_fake_data_1L_lgl)
                if(!is.null(dv_ds_nm_and_url_chr)){
                  ready4::write_to_dv_with_wait(consent_1L_chr = consent_1L_chr,
                                                consent_indcs_int = consent_indcs_int,
                                                dss_tb = tibble::tibble(ds_obj_nm_chr = c(rprt_nm_1L_chr,rcrd_nm_1L_chr),
                                                                        title_chr = rprt_lup %>%
                                                                          dplyr::filter(rprt_nms_chr %in% c(rprt_nm_1L_chr,rcrd_nm_1L_chr)) %>%
                                                                          dplyr::pull(title_chr)),
                                                dv_nm_1L_chr = dv_ds_nm_and_url_chr[1],
                                                ds_url_1L_chr = dv_ds_nm_and_url_chr[2],
                                                options_chr = options_chr,
                                                parent_dv_dir_1L_chr = paths_ls$dv_dir_1L_chr,
                                                paths_to_dirs_chr = paths_ls$reports_dir_1L_chr,
                                                inc_fl_types_chr = ".pdf",
                                                paths_are_rltv_1L_lgl = F)
                }
                rprt_lup
              }) %>% stats::setNames(reference_int %>% purrr::map_chr(~ifelse(.x==0,
                                                                              "Primary",
                                                                              paste0("secondary_",.x))))
  consolidated_mdl_ings_ls <- reference_int %>%
    purrr::reduce(.init = paste0(paths_ls$output_data_dir_1L_chr,"/G_Shareable/Ingredients/mdl_ingredients.RDS") %>%
                    readRDS(),
                  ~
                    if(.y>0){
                    ingredients_ls <- here::here(paths_ls$path_from_top_level_1L_chr,
                                                 paths_ls$write_to_dir_nm_1L_chr,
                                                 paste0("secondary_",.y),
                                                 "Output",
                                                 "G_Shareable",
                                                 "Ingredients",
                                                 "mdl_ingredients.RDS") %>% readRDS()
                    .x <- append(.x,
                                 list(ingredients_ls) %>%
                                   setNames(paste0("secondary_",.y)))
                    .x$dictionary_tb <- dplyr::bind_rows(.x$dictionary_tb,
                                                         ingredients_ls$dictionary_tb) %>%
                      dplyr::distinct()
                    .x$mdls_lup <- dplyr::bind_rows(.x$mdls_lup,
                                                    ingredients_ls$mdls_lup %>%
                                                      dplyr::mutate(source_chr = paste0("Secondary Analysis ",
                                                                                        LETTERS[.y]))) %>%
                      dplyr::distinct()
                    .x$mdls_smry_tb <- dplyr::bind_rows(.x$mdls_smry_tb,
                                                        ingredients_ls$mdls_smry_tb) %>%
                      dplyr::distinct()
                    .x$predictors_lup <- dplyr::bind_rows(.x$predictors_lup,
                                                        ingredients_ls$predictors_lup) %>%
                      dplyr::distinct()
                    .x
                  }else{
                    .x$Primary <- .x
                    .x$mdls_lup <- .x$mdls_lup %>%
                      dplyr::mutate(source_chr = "Primary Analysis")
                    .x
                  })
  ready4::write_with_consent(consented_fn = saveRDS,
                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                    "mdl_ingredients.RDS",
                                                    " to ",
                                                    paste0(paths_ls$output_data_dir_1L_chr,"/G_Shareable/Ingredients"),
                                                    "?"),
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(object = consolidated_mdl_ings_ls,
                                                      file = paste0(paths_ls$output_data_dir_1L_chr,"/G_Shareable/Ingredients/mdl_ingredients.RDS")),
                             consented_msg_1L_chr = paste0("File ",
                                                           "mdl_ingredients.RDS",
                                                           " has been written to ",
                                                           paste0(paths_ls$output_data_dir_1L_chr,"/G_Shareable/Ingredients"),
                                                           "."),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr)

  if(!is.null(input_params_ls)){
    input_params_ls$rprt_lups_ls <- rprt_lups_ls
  }else{
    input_params_ls <- rprt_lups_ls
  }
return(input_params_ls)
}
write_mdl_type_covars_mdls <- function (data_tb,
                                        consent_1L_chr = "",
                                        consent_indcs_int = 1L,
                                        depnt_var_min_val_1L_dbl = numeric(0),
                                        depnt_var_nm_1L_chr = "utl_total_w",# Remove default
                                        options_chr = c("Y", "N"),
                                        predrs_var_nms_chr,
                                        covar_var_nms_chr, mdl_type_1L_chr, path_to_write_to_1L_chr, new_dir_nm_1L_chr = "D_Covars_Selection",
                                        fl_nm_pfx_1L_chr = "D_CT", mdl_types_lup = NULL, start_1L_chr = NA_character_)
{
  if (is.null(mdl_types_lup))
    utils::data("mdl_types_lup", envir = environment())
  arg_vals_chr <- c("control_chr", "predn_type_chr","tfmn_chr") %>%
    purrr::map_chr(~ready4::get_from_lup_obj(mdl_types_lup,
                                             match_var_nm_1L_chr = "short_name_chr", match_value_xx = mdl_type_1L_chr,
                                             target_var_nm_1L_chr = .x, evaluate_1L_lgl = F))
  control_1L_chr <- arg_vals_chr[1]
  predn_type_1L_chr <- arg_vals_chr[2]
  tfmn_1L_chr <- arg_vals_chr[3]
  if (is.na(predn_type_1L_chr))
    predn_type_1L_chr <- NULL
  data_tb <- data_tb %>%
    add_tfd_var_to_ds(depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                      tfmn_1L_chr = tfmn_1L_chr)
  output_dir_1L_chr <- output_dir_1L_chr <- write_new_outp_dir(path_to_write_to_1L_chr,
                                                               consent_1L_chr = consent_1L_chr,
                                                               consent_indcs_int = consent_indcs_int,
                                                               new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                                               options_chr = options_chr)
  smry_of_mdls_with_covars_tb <- purrr::map_dfr(predrs_var_nms_chr,
                                                ~{
                                                  model_mdl <- make_mdl(data_tb,
                                                                        depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                        depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                                        predr_var_nm_1L_chr = .x, covar_var_nms_chr = covar_var_nms_chr,
                                                                        tfmn_1L_chr = tfmn_1L_chr, mdl_type_1L_chr = mdl_type_1L_chr,
                                                                        control_1L_chr = control_1L_chr, mdl_types_lup = mdl_types_lup,
                                                                        start_1L_chr = start_1L_chr)
                                                  mdl_fl_nm_1L_chr <- paste0(fl_nm_pfx_1L_chr, "_",
                                                                             .x, "_", mdl_type_1L_chr)
                                                  ready4::write_with_consent(consented_fn = saveRDS,
                                                                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                                                                    paste0(mdl_fl_nm_1L_chr, ".RDS"),
                                                                                                    " to ",
                                                                                                    output_dir_1L_chr,
                                                                                                    "?"),
                                                                             consent_1L_chr = consent_1L_chr,
                                                                             consent_indcs_int = consent_indcs_int,
                                                                             consented_args_ls = list(object = model_mdl,
                                                                                                      file = paste0(output_dir_1L_chr, "/", mdl_fl_nm_1L_chr, ".RDS")),
                                                                             consented_msg_1L_chr = paste0("File ",
                                                                                                           paste0(mdl_fl_nm_1L_chr, ".RDS"),
                                                                                                           " has been written to ",
                                                                                                           output_dir_1L_chr,
                                                                                                           "."),
                                                                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                                                                             options_chr = options_chr)

                                                  if("summary.betareg" %in% class(summary(model_mdl))){
                                                    coefficients_mat <- summary(model_mdl)$coefficients$mean
                                                  }else{
                                                    coefficients_mat <- summary(model_mdl)$coefficients
                                                  }
                                                  tibble::tibble(variable = .x,
                                                                 Rsquare = caret::R2(stats::predict(model_mdl, type = predn_type_1L_chr) %>% calculate_depnt_var_tfmn(tfmn_1L_chr = tfmn_1L_chr,
                                                                                                                                                                      tfmn_is_outp_1L_lgl = T),
                                                                                     data_tb %>%
                                                                                       dplyr::pull(!!rlang::sym(depnt_var_nm_1L_chr)),
                                                                                     form = "traditional"),
                                                                 AIC = stats::AIC(model_mdl),
                                                                 BIC = stats::BIC(model_mdl),
                                                                 Significant = paste(names(which(coefficients_mat[,4] < 0.01)), collapse = " "))
                                                })
  smry_of_mdls_with_covars_tb <- smry_of_mdls_with_covars_tb %>%
    dplyr::arrange(dplyr::desc(AIC))
  saveRDS(smry_of_mdls_with_covars_tb, paste0(output_dir_1L_chr,
                                              "/", paste0(fl_nm_pfx_1L_chr, "_", "SMRY", "_", mdl_type_1L_chr),
                                              ".RDS"))
  return(smry_of_mdls_with_covars_tb)
}
write_mdl_type_multi_outps <- function (data_tb,
                                        mdl_type_1L_chr,
                                        new_dir_nm_1L_chr,
                                        path_to_write_to_1L_chr,
                                        predrs_var_nms_chr,
                                        consent_1L_chr = "",
                                        consent_indcs_int = 1L,
                                        covar_var_nms_chr = NA_character_,
                                        depnt_var_min_val_1L_dbl = numeric(0),
                                        depnt_var_nm_1L_chr = "utl_total_w",# remove default
                                        fl_nm_pfx_1L_chr = "C_PREDR",
                                        folds_1L_int = 10,
                                        mdl_types_lup = NULL,
                                        options_chr = c("Y", "N"),
                                        plt_indcs_int = c(3, 5),
                                        start_1L_chr = NULL)
{
  if (is.null(mdl_types_lup))
    utils::data("mdl_types_lup", envir = environment())
  output_dir_1L_chr <- write_new_outp_dir(path_to_write_to_1L_chr,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  smry_of_mdl_sngl_predrs_tb <- purrr::map_dfr(predrs_var_nms_chr,
                                               ~{
                                                 tfmn_1L_chr <- ready4::get_from_lup_obj(mdl_types_lup,
                                                                                         match_var_nm_1L_chr = "short_name_chr", match_value_xx = mdl_type_1L_chr,
                                                                                         target_var_nm_1L_chr = "tfmn_chr", evaluate_1L_lgl = F)
                                                 mdl_smry_tb <- write_mdl_type_sngl_outps(data_tb,
                                                                                          consent_1L_chr = consent_1L_chr,
                                                                                          consent_indcs_int = consent_indcs_int,
                                                                                          covar_var_nms_chr = covar_var_nms_chr,
                                                                                          depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                                          depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                                                          folds_1L_int = folds_1L_int,
                                                                                          mdl_fl_nm_1L_chr = paste0(fl_nm_pfx_1L_chr,
                                                                                                                    "_", .x, "_", mdl_type_1L_chr),
                                                                                          mdl_type_1L_chr = mdl_type_1L_chr,
                                                                                          mdl_types_lup = mdl_types_lup,
                                                                                          options_chr = options_chr,
                                                                                          path_to_write_to_1L_chr = output_dir_1L_chr,
                                                                                          plt_indcs_int = plt_indcs_int,
                                                                                          predr_vals_dbl = NA_real_,
                                                                                          predr_var_desc_1L_chr = NA_character_,
                                                                                          predr_var_nm_1L_chr = .x,
                                                                                          start_1L_chr = start_1L_chr,
                                                                                          tfmn_1L_chr = tfmn_1L_chr)
                                                 if (!is.null(folds_1L_int)) {
                                                   mdl_smry_tb <- mdl_smry_tb %>% dplyr::select((-Model)) %>%
                                                     dplyr::mutate(Predictor = .x) %>% dplyr::select(Predictor,
                                                                                                     dplyr::everything())
                                                 }
                                                 mdl_smry_tb
                                               })
  if (!is.null(folds_1L_int)) {
    smry_of_mdl_sngl_predrs_tb <- smry_of_mdl_sngl_predrs_tb %>%
      dplyr::arrange(dplyr::desc(RsquaredP))
  }
  return(smry_of_mdl_sngl_predrs_tb)
}
write_mdl_type_sngl_outps <- function (data_tb,
                                       mdl_fl_nm_1L_chr,
                                       path_to_write_to_1L_chr,
                                       predr_vals_dbl,
                                       predr_var_desc_1L_chr,
                                       predr_var_nm_1L_chr,
                                       consent_1L_chr = "",
                                       consent_indcs_int = 1L,
                                       covar_var_nms_chr = NA_character_,
                                       depnt_var_nm_1L_chr = "utl_total_w",
                                       depnt_var_min_val_1L_dbl = numeric(0),
                                       folds_1L_int = 10,
                                       mdl_type_1L_chr = "OLS_NTF",
                                       mdl_types_lup = NULL,
                                       options_chr = c("Y", "N"),
                                       start_1L_chr = NULL,
                                       plt_indcs_int = NA_integer_,
                                       tfmn_1L_chr = "NTF")
{
  if (is.null(mdl_types_lup))
    utils::data("mdl_types_lup", envir = environment())
  arg_vals_chr <- c("control_chr", "family_chr", "predn_type_chr") %>%
    purrr::map_chr(~ready4::get_from_lup_obj(mdl_types_lup,
                                             match_var_nm_1L_chr = "short_name_chr", match_value_xx = mdl_type_1L_chr,
                                             target_var_nm_1L_chr = .x, evaluate_1L_lgl = F))
  control_1L_chr <- arg_vals_chr[1]
  family_1L_chr <- arg_vals_chr[2]
  predn_type_1L_chr <- arg_vals_chr[3]
  if (is.na(predn_type_1L_chr))
    predn_type_1L_chr <- NULL
  if (is.na(plt_indcs_int[1])) {
    plt_indcs_int <- 1:5
    if (!is.na(control_1L_chr)) {
      if (control_1L_chr %>% startsWith("betareg"))
        plt_indcs_int <- c(1, 3, 4, 5)
    }
  }
  tfmn_for_bnml_1L_lgl <- ready4::get_from_lup_obj(mdl_types_lup,
                                                   match_var_nm_1L_chr = "short_name_chr", match_value_xx = mdl_type_1L_chr,
                                                   target_var_nm_1L_chr = "tfmn_for_bnml_lgl", evaluate_1L_lgl = F)
  data_tb <- data_tb %>%
    add_tfd_var_to_ds(depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                      tfmn_1L_chr = tfmn_1L_chr)
  model_mdl <- make_mdl(data_tb, depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                        depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                        tfmn_1L_chr = tfmn_1L_chr, predr_var_nm_1L_chr = predr_var_nm_1L_chr,
                        covar_var_nms_chr = covar_var_nms_chr, mdl_type_1L_chr = mdl_type_1L_chr,
                        mdl_types_lup = mdl_types_lup, control_1L_chr = control_1L_chr,
                        start_1L_chr = start_1L_chr)
  write_mdl_plts(data_tb,
                 consent_1L_chr = consent_1L_chr,
                 consent_indcs_int = consent_indcs_int,
                 covar_var_nms_chr = covar_var_nms_chr,
                 depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                 family_1L_chr = family_1L_chr,
                 mdl_fl_nm_1L_chr = mdl_fl_nm_1L_chr,
                 model_mdl = model_mdl,
                 options_chr = options_chr,
                 path_to_write_to_1L_chr = path_to_write_to_1L_chr,
                 plt_indcs_int = plt_indcs_int,
                 predn_type_1L_chr = predn_type_1L_chr,
                 predr_vals_dbl = predr_vals_dbl,
                 predr_var_desc_1L_chr = predr_var_desc_1L_chr,
                 predr_var_nm_1L_chr = predr_var_nm_1L_chr,
                 tfmn_1L_chr = tfmn_1L_chr,
                 tfmn_for_bnml_1L_lgl = tfmn_for_bnml_1L_lgl)
  if (!is.null(folds_1L_int)) {
    smry_of_one_predr_mdl_tb <- make_smry_of_mdl_outp(data_tb,
                                                      folds_1L_int = folds_1L_int,
                                                      depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                      depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                      tfmn_1L_chr = tfmn_1L_chr, predr_var_nm_1L_chr = predr_var_nm_1L_chr, covar_var_nms_chr = covar_var_nms_chr,
                                                      mdl_type_1L_chr = mdl_type_1L_chr, mdl_types_lup = mdl_types_lup, start_1L_chr = start_1L_chr,
                                                      predn_type_1L_chr = predn_type_1L_chr)
  }
  else {
    smry_of_one_predr_mdl_tb <- tibble::tibble()
  }
  ready4::write_with_consent(consented_fn = saveRDS,
                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                    paste0(mdl_fl_nm_1L_chr, ".RDS"),
                                                    " to ",
                                                    path_to_write_to_1L_chr,
                                                    "?"),
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(object = model_mdl,
                                                      file = paste0(path_to_write_to_1L_chr, "/", mdl_fl_nm_1L_chr, ".RDS")),
                             consented_msg_1L_chr = paste0("File ",
                                                           paste0(mdl_fl_nm_1L_chr, ".RDS"),
                                                           " has been written to ",
                                                           path_to_write_to_1L_chr,
                                                           "."),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr)

  return(smry_of_one_predr_mdl_tb)
}
write_mdls_to_dv <- function(outp_smry_ls,
                             consent_1L_chr = "",
                             consent_indcs_int = 1L,
                             new_dir_nm_1L_chr = "G_Shareable",
                             options_chr = c("Y", "N"),
                             shareable_title_detail_1L_chr = "",
                             output_dir_chr = NA_character_){
  if(is.na(output_dir_chr[1]))
    output_dir_chr <- write_shareable_dir(outp_smry_ls = outp_smry_ls,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  if (!is.null(outp_smry_ls$dv_ls)) {
    write_shareable_mdls_to_dv(outp_smry_ls,
                               consent_1L_chr = consent_1L_chr,
                               consent_indcs_int = consent_indcs_int,
                               new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                               options_chr = options_chr,
                               output_dir_chr = output_dir_chr,
                               share_ingredients_1L_lgl = T)
    outp_smry_ls$shareable_mdls_tb <- write_shareable_mdls_to_dv(outp_smry_ls,
                                                                 consent_1L_chr = consent_1L_chr,
                                                                 consent_indcs_int = consent_indcs_int,
                                                                 new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                                                 options_chr = options_chr,
                                                                 output_dir_chr = output_dir_chr,
                                                                 share_ingredients_1L_lgl = F,
                                                                 shareable_title_detail_1L_chr = shareable_title_detail_1L_chr)
  }
  return(outp_smry_ls)
}
write_mdls_with_covars_cmprsn <- function(scored_data_tb,
                                          bl_tb,
                                          combinations_1L_lgl = F,
                                          consent_1L_chr = "",
                                          consent_indcs_int = 1L,
                                          depnt_var_min_val_1L_dbl = numeric(0),
                                          ds_smry_ls,
                                          existing_predrs_ls = NULL,
                                          max_nbr_of_covars_1L_int = integer(0),
                                          mdl_smry_ls,
                                          options_chr = c("Y", "N"),
                                          output_data_dir_1L_chr,
                                          seed_1L_int = 1234,
                                          session_data_ls = NULL){
  empty_tb <- write_mdl_type_multi_outps(data_tb = bl_tb,
                                         consent_1L_chr = consent_1L_chr,
                                         consent_indcs_int = consent_indcs_int,
                                         covar_var_nms_chr = mdl_smry_ls$prefd_covars_chr,
                                         depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                         depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                         fl_nm_pfx_1L_chr = "E_CK_CV",
                                         folds_1L_int = NULL,
                                         mdl_type_1L_chr = mdl_smry_ls$prefd_mdl_types_chr[1],
                                         mdl_types_lup = mdl_smry_ls$mdl_types_lup,
                                         new_dir_nm_1L_chr = "E_Predrs_W_Covars_Sngl_Mdl_Cmprsn",
                                         options_chr = options_chr,
                                         path_to_write_to_1L_chr = output_data_dir_1L_chr,
                                         predrs_var_nms_chr = mdl_smry_ls$predr_cmprsn_tb$predr_chr,
                                         start_1L_chr = NA_character_)
  mdl_smry_ls$predr_vars_nms_ls <- make_predr_vars_nms_ls(main_predrs_chr = mdl_smry_ls$predr_cmprsn_tb$predr_chr,
                                                          covars_ls = list(mdl_smry_ls$prefd_covars_chr),
                                                          combinations_1L_lgl = combinations_1L_lgl,
                                                          existing_predrs_ls = existing_predrs_ls,
                                                          max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int)
  mdl_smry_ls$mdl_nms_ls <- make_mdl_nms_ls(mdl_smry_ls$predr_vars_nms_ls,
                                            mdl_types_chr = mdl_smry_ls$prefd_mdl_types_chr)
  outp_smry_ls <- list(scored_data_tb = scored_data_tb,
                       dictionary_tb = ds_smry_ls$dictionary_tb,
                       predictors_lup = ds_smry_ls$predictors_lup,
                       smry_of_sngl_predr_mdls_tb = mdl_smry_ls$smry_of_sngl_predr_mdls_tb,
                       prefd_mdl_types_chr = mdl_smry_ls$prefd_mdl_types_chr,
                       predr_cmprsn_tb = mdl_smry_ls$predr_cmprsn_tb,
                       smry_of_mdl_sngl_predrs_tb = mdl_smry_ls$smry_of_mdl_sngl_predrs_tb,
                       mdls_with_covars_smry_tb = mdl_smry_ls$mdls_with_covars_smry_tb,
                       signt_covars_chr = mdl_smry_ls$signt_covars_chr,
                       prefd_covars_chr = mdl_smry_ls$prefd_covars_chr,
                       depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                       predr_vars_nms_ls = mdl_smry_ls$predr_vars_nms_ls,
                       mdl_nms_ls = mdl_smry_ls$mdl_nms_ls,
                       id_var_nm_1L_chr = ds_smry_ls$id_var_nm_1L_chr,
                       round_var_nm_1L_chr = ds_smry_ls$round_var_nm_1L_chr,
                       round_bl_val_1L_chr = ds_smry_ls$round_bl_val_1L_chr,
                       path_to_write_to_1L_chr = output_data_dir_1L_chr,
                       seed_1L_int = seed_1L_int,
                       folds_1L_int = mdl_smry_ls$folds_1L_int,
                       max_nbr_of_boruta_mdl_runs_int = mdl_smry_ls$max_nbr_of_boruta_mdl_runs_int,
                       mdl_types_lup = mdl_smry_ls$mdl_types_lup,
                       file_paths_chr = list.files(output_data_dir_1L_chr, recursive = T),
                       session_data_ls = session_data_ls)
  ready4::write_with_consent(consented_fn = saveRDS,
                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                    "I_ALL_OUTPUT_.RDS",
                                                    " to ",
                                                    outp_smry_ls$path_to_write_to_1L_chr,
                                                    "?"),
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(object = outp_smry_ls,
                                                      file = paste0(outp_smry_ls$path_to_write_to_1L_chr,"/I_ALL_OUTPUT_.RDS")),
                             consented_msg_1L_chr = paste0("File ",
                                                           "I_ALL_OUTPUT_.RDS",
                                                           " has been written to ",
                                                           outp_smry_ls$path_to_write_to_1L_chr,
                                                           "."),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr)
  return(outp_smry_ls)
}
write_new_outp_dir <- function(path_to_write_to_1L_chr,
                               new_dir_nm_1L_chr,
                               consent_1L_chr = "",
                               consent_indcs_int = 1L,
                               options_chr = c("Y", "N")){
  output_dir_1L_chr <- paste0(path_to_write_to_1L_chr,"/",new_dir_nm_1L_chr)
  ready4::write_new_dirs(output_dir_1L_chr,
                         consent_1L_chr = consent_1L_chr,
                         consent_indcs_int = consent_indcs_int,
                         options_chr = options_chr)
  return(output_dir_1L_chr)
}
write_predr_and_covars_cmprsn <- function(scored_data_tb,
                                          bl_tb,
                                          consent_1L_chr = "",
                                          consent_indcs_int = 1L,
                                          depnt_var_min_val_1L_dbl = numeric(0),
                                          ds_smry_ls,
                                          mdl_smry_ls,
                                          options_chr = c("Y", "N"),
                                          output_data_dir_1L_chr,
                                          seed_1L_int = 1234,
                                          signft_covars_cdn_1L_chr = "any"){
  mdl_smry_ls$predr_cmprsn_tb <- write_predr_cmprsn_outps(data_tb = bl_tb,
                                                          candidate_predrs_chr = ds_smry_ls$candidate_predrs_chr,
                                                          consent_1L_chr = consent_1L_chr,
                                                          consent_indcs_int = consent_indcs_int,
                                                          depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                          depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                                          max_nbr_of_boruta_mdl_runs_int = mdl_smry_ls$max_nbr_of_boruta_mdl_runs_int,
                                                          new_dir_nm_1L_chr = "B_Candidate_Predrs_Cmprsn",
                                                          options_chr = options_chr,
                                                          path_to_write_to_1L_chr = output_data_dir_1L_chr)
  if(identical(mdl_smry_ls$predr_cmprsn_tb$predr_chr, character(0))){
    stop("No important predictors identified - execution aborted. Try specifying other predictors.")
  }
  mdl_smry_ls$smry_of_mdl_sngl_predrs_tb <- write_mdl_type_multi_outps(data_tb = bl_tb,
                                                                       consent_1L_chr = consent_1L_chr,
                                                                       consent_indcs_int = consent_indcs_int,
                                                                       depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                       depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                                                       fl_nm_pfx_1L_chr = "C_PREDR",
                                                                       folds_1L_int = mdl_smry_ls$folds_1L_int,
                                                                       mdl_type_1L_chr = mdl_smry_ls$prefd_mdl_types_chr[1],
                                                                       mdl_types_lup = mdl_smry_ls$mdl_types_lup,
                                                                       new_dir_nm_1L_chr = "C_Predrs_Sngl_Mdl_Cmprsn",
                                                                       options_chr = options_chr,
                                                                       path_to_write_to_1L_chr = output_data_dir_1L_chr,
                                                                       predrs_var_nms_chr = mdl_smry_ls$predr_cmprsn_tb$predr_chr,
                                                                       start_1L_chr = NA_character_)
  bl_tb <- scored_data_tb %>%
    youthvars::transform_ds_for_tstng(depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                      candidate_predrs_chr = ds_smry_ls$candidate_predrs_chr,
                                      covar_var_nms_chr = ds_smry_ls$candidate_covar_nms_chr,
                                      remove_all_msng_1L_lgl = T,
                                      round_var_nm_1L_chr = ds_smry_ls$round_var_nm_1L_chr,
                                      round_val_1L_chr = ds_smry_ls$round_bl_val_1L_chr)
  mdl_smry_ls$mdls_with_covars_smry_tb <- write_mdl_type_covars_mdls(bl_tb,
                                                                     consent_1L_chr = consent_1L_chr,
                                                                     consent_indcs_int = consent_indcs_int,
                                                                     covar_var_nms_chr = ds_smry_ls$candidate_covar_nms_chr,
                                                                     depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                     depnt_var_nm_1L_chr = ds_smry_ls$depnt_var_nm_1L_chr,
                                                                     fl_nm_pfx_1L_chr = "D_CT",
                                                                     mdl_type_1L_chr = mdl_smry_ls$prefd_mdl_types_chr[1],
                                                                     new_dir_nm_1L_chr = "D_Predr_Covars_Cmprsn",
                                                                     mdl_types_lup = mdl_smry_ls$mdl_types_lup,
                                                                     options_chr = options_chr,
                                                                     path_to_write_to_1L_chr = output_data_dir_1L_chr,
                                                                     predrs_var_nms_chr = ds_smry_ls$candidate_predrs_chr)
  mdl_smry_ls$signt_covars_chr <- get_signft_covars(mdls_with_covars_smry_tb = mdl_smry_ls$mdls_with_covars_smry_tb,
                                                    covar_var_nms_chr = ds_smry_ls$candidate_covar_nms_chr,
                                                    X_Ready4useDyad = ready4use::Ready4useDyad(ds_tb = scored_data_tb,
                                                                                               dictionary_r3 = ds_smry_ls$dictionary_tb),
                                                    what_1L_chr = signft_covars_cdn_1L_chr)
  predr_and_covars_cmprsn_ls <- list(bl_tb = bl_tb,
                                     ds_smry_ls = ds_smry_ls,
                                     mdl_smry_ls = mdl_smry_ls)
  return(predr_and_covars_cmprsn_ls)
}
write_predr_and_mdl_tstng_results <- function(scored_data_tb,
                                              combinations_1L_lgl = F,
                                              consent_1L_chr = "",
                                              consent_indcs_int = 1L,
                                              depnt_var_max_val_1L_dbl = 0.99999,
                                              depnt_var_min_val_1L_dbl = 0.00001,
                                              ds_smry_ls,
                                              existing_predrs_ls = NULL,
                                              max_nbr_of_covars_1L_int = integer(0),
                                              mdl_smry_ls,
                                              session_data_ls,
                                              options_chr = c("Y", "N"),
                                              output_data_dir_1L_chr,
                                              seed_1L_int = 1234,
                                              signft_covars_cdn_1L_chr = "any"){
  cmprsn_ls <- write_mdl_cmprsn(scored_data_tb = scored_data_tb,
                                consent_1L_chr = consent_1L_chr,
                                consent_indcs_int = consent_indcs_int,
                                depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl,
                                ds_smry_ls = ds_smry_ls,
                                mdl_smry_ls = mdl_smry_ls,
                                options_chr = options_chr,
                                output_data_dir_1L_chr = output_data_dir_1L_chr,
                                seed_1L_int = seed_1L_int)
  cmprsn_ls <- write_predr_and_covars_cmprsn(scored_data_tb = scored_data_tb,
                                             bl_tb = cmprsn_ls$bl_tb,
                                             consent_1L_chr = consent_1L_chr,
                                             consent_indcs_int = consent_indcs_int,
                                             depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                             ds_smry_ls = cmprsn_ls$ds_smry_ls,
                                             mdl_smry_ls  = cmprsn_ls$mdl_smry_ls,
                                             options_chr = options_chr,
                                             output_data_dir_1L_chr = output_data_dir_1L_chr,
                                             seed_1L_int = seed_1L_int,
                                             signft_covars_cdn_1L_chr  = signft_covars_cdn_1L_chr)
  if(ifelse(is.null(cmprsn_ls$mdl_smry_ls$prefd_covars_chr),
            T,
            is.na(cmprsn_ls$mdl_smry_ls$prefd_covars_chr[1]))){
    cmprsn_ls$mdl_smry_ls$prefd_covars_chr <- cmprsn_ls$mdl_smry_ls$signt_covars_chr
  }
  outp_smry_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = scored_data_tb,
                                                bl_tb = cmprsn_ls$bl_tb,
                                                combinations_1L_lgl = combinations_1L_lgl,
                                                consent_1L_chr = consent_1L_chr,
                                                consent_indcs_int = consent_indcs_int,
                                                depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                ds_smry_ls = cmprsn_ls$ds_smry_ls,
                                                existing_predrs_ls = existing_predrs_ls,
                                                max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int,
                                                mdl_smry_ls = cmprsn_ls$mdl_smry_ls,
                                                options_chr = options_chr,
                                                output_data_dir_1L_chr = output_data_dir_1L_chr,
                                                seed_1L_int = seed_1L_int,
                                                session_data_ls = session_data_ls)
  return(outp_smry_ls)
}
write_predr_cmprsn_outps <- function (data_tb, path_to_write_to_1L_chr, new_dir_nm_1L_chr = "B_Candidate_Predrs_Cmprsn",
                                      consent_1L_chr = "",
                                      consent_indcs_int = 1L,
                                      depnt_var_min_val_1L_dbl = numeric(0),
                                      depnt_var_nm_1L_chr = "utl_total_w",
                                      candidate_predrs_chr, max_nbr_of_boruta_mdl_runs_int = 300L,
                                      options_chr = c("Y", "N"))
{
  if (length(candidate_predrs_chr) > 1) {
    covar_var_nms_chr <- candidate_predrs_chr[2:length(candidate_predrs_chr)]
  } else {
    covar_var_nms_chr <- NA_character_
  }
  data_tb <- transform_ds_for_mdlng(data_tb, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                    depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                    predr_var_nm_1L_chr = candidate_predrs_chr[1], covar_var_nms_chr = covar_var_nms_chr)
  rf_mdl <- randomForest::randomForest(stats::as.formula(paste0(depnt_var_nm_1L_chr,
                                                                " ~ .")), data = data_tb, importance = TRUE)
  boruta_mdl <- Boruta::Boruta(stats::as.formula(paste0(depnt_var_nm_1L_chr,
                                                        " ~ .")), data = data_tb, maxRuns = max_nbr_of_boruta_mdl_runs_int)
  output_dir_1L_chr <- write_new_outp_dir(path_to_write_to_1L_chr,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  purrr::pwalk(list(fn_ls = list(randomForest::varImpPlot,
                                 plot), fn_args_ls_ls = list(list(rf_mdl, main = ""),
                                                             list(boruta_mdl, cex = 1.5, cex.axis = 0.8, las = 2,
                                                                  xlab = "", main = "")), plt_nm_sfx_chr = c("_RF_VAR_IMP",
                                                                                                             "_BORUTA_VAR_IMP"), size_ls = list(c(6, 6), c(4, 6))),
               ~ ready4show::write_mdl_plt_fl(plt_fn = ..1,
                                              consent_1L_chr = consent_1L_chr,
                                              consent_indcs_int = consent_indcs_int,
                                              fn_args_ls = ..2,
                                              options_chr = options_chr,
                                              path_to_write_to_1L_chr = output_dir_1L_chr,
                                              plt_nm_1L_chr = paste0("B_PRED_CMPRSN", ..3), height_1L_dbl = ..4[1],
                                              width_1L_dbl = ..4[2]))
  confirmed_predrs_chr <- names(boruta_mdl$finalDecision)[boruta_mdl$finalDecision ==
                                                            "Confirmed"]
  confirmed_predrs_tb <- rf_mdl$importance %>% tibble::as_tibble(rownames = "predr_chr") %>%
    dplyr::arrange(dplyr::desc(`%IncMSE`)) %>% dplyr::filter(predr_chr %in%
                                                               confirmed_predrs_chr)
  return(confirmed_predrs_tb)
}
write_scndry_analysis <- function(valid_params_ls_ls,
                                  candidate_covar_nms_chr,
                                  path_params_ls,
                                  reference_1L_int,
                                  backend_1L_chr = "cmdstanr",
                                  candidate_predrs_chr = NULL,
                                  combinations_1L_lgl = F,
                                  consent_1L_chr = "",
                                  consent_indcs_int = 1L,
                                  cores_1L_int = 1L,
                                  depnt_var_max_val_1L_dbl = 0.99999,
                                  depnt_var_min_val_1L_dbl = 0.00001,
                                  existing_predrs_ls = NULL,
                                  max_nbr_of_covars_1L_int = integer(0),
                                  new_dir_nm_1L_chr = "F_TS_Mdls",
                                  options_chr = c("Y", "N"),
                                  predictors_lup = NULL,
                                  prefd_covars_chr = NA_character_,
                                  signft_covars_cdn_1L_chr = "any"){
  analysis_params_ls <- valid_params_ls_ls$params_ls %>%
    append(path_params_ls[1:2])
  rename_lup <- valid_params_ls_ls$rename_lup
  if(!is.null(predictors_lup)){
    predictors_lup$short_name_chr <- predictors_lup$short_name_chr %>%
      purrr::map_chr(~ifelse(!.x %in% rename_lup$old_nms_chr,
                             .x,
                             ready4::get_from_lup_obj(rename_lup,
                                                      match_value_xx = .x,
                                                      match_var_nm_1L_chr = "old_nms_chr",
                                                      target_var_nm_1L_chr = "new_nms_chr",
                                                      evaluate_1L_lgl = F)))
    analysis_params_ls$predictors_lup <- predictors_lup
  }
  if(!is.null(candidate_predrs_chr)){
    candidate_predrs_chr <- candidate_predrs_chr %>%
      purrr::map_chr(~ifelse(!.x %in% rename_lup$old_nms_chr,
                             .x,
                             ready4::get_from_lup_obj(rename_lup,
                                                      match_value_xx = .x,
                                                      match_var_nm_1L_chr = "old_nms_chr",
                                                      target_var_nm_1L_chr = "new_nms_chr",
                                                      evaluate_1L_lgl = F)))
    analysis_params_ls$ds_descvs_ls$candidate_predrs_chr <- candidate_predrs_chr
  }
  if(!is.null(candidate_covar_nms_chr)){
    candidate_covar_nms_chr <- candidate_covar_nms_chr %>%
      purrr::map_chr(~ifelse(!.x %in% rename_lup$old_nms_chr,
                             .x,
                             ready4::get_from_lup_obj(rename_lup,
                                                      match_value_xx = .x,
                                                      match_var_nm_1L_chr = "old_nms_chr",
                                                      target_var_nm_1L_chr = "new_nms_chr",
                                                      evaluate_1L_lgl = F)))
  }
  if(ifelse(is.null(prefd_covars_chr),F,!is.na(prefd_covars_chr))){
    prefd_covars_chr <- prefd_covars_chr %>%
      purrr::map_chr(~ifelse(!.x %in% rename_lup$old_nms_chr,
                             .x,
                             ready4::get_from_lup_obj(rename_lup,
                                                      match_value_xx = .x,
                                                      match_var_nm_1L_chr = "old_nms_chr",
                                                      target_var_nm_1L_chr = "new_nms_chr",
                                                      evaluate_1L_lgl = F)))
  }
  analysis_params_ls$prefd_covars_chr <- prefd_covars_chr
  analysis_params_ls$candidate_covar_nms_chr <- candidate_covar_nms_chr
  path_params_ls$paths_ls <- write_scndry_analysis_dir(path_params_ls$paths_ls,
                                                       consent_1L_chr = consent_1L_chr,
                                                       consent_indcs_int = consent_indcs_int,
                                                       options_chr = options_chr,
                                                       reference_1L_int = reference_1L_int)
  params_ls <- list(candidate_predrs_chr = candidate_predrs_chr,
                    transform_paths_ls = list(fn = transform_paths_ls_for_scndry,
                                              args_ls = list(reference_1L_int = reference_1L_int))) %>%
    append(analysis_params_ls)
  params_ls$utl_class_fn_1L_chr <- params_ls$raw_ds_tfmn_fn <- NULL
  params_ls_ls <- transform_params_ls_to_valid(params_ls)
  params_ls <- params_ls_ls %>%
    purrr::pluck("params_ls") %>%
    append(list(rename_lup = params_ls_ls$rename_lup))
  outp_smry_ls <- valid_params_ls_ls$outp_smry_ls
  mdl_smry_ls <- params_ls$mdl_smry_ls
  data_tb <- outp_smry_ls$scored_data_tb
  ds_smry_ls <- params_ls$ds_descvs_ls %>%
    make_analysis_ds_smry_ls(candidate_covar_nms_chr = params_ls$candidate_covar_nms_chr,
                             predictors_lup = params_ls$predictors_lup)
  ds_smry_ls$candidate_predrs_chr <- params_ls$candidate_predrs_chr
  existing_mdls_chr <- outp_smry_ls[["mdl_nms_ls"]] %>% purrr::flatten_chr()
  existing_predrs_ls <- outp_smry_ls$predr_vars_nms_ls
  cmprsns_ls <- write_mdl_cmprsn(scored_data_tb = data_tb, ###
                                 consent_1L_chr = consent_1L_chr,
                                 consent_indcs_int = consent_indcs_int,
                                 depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                 depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl,
                                 ds_smry_ls = ds_smry_ls,
                                 mdl_smry_ls = mdl_smry_ls,
                                 options_chr = options_chr,
                                 output_data_dir_1L_chr = path_params_ls$paths_ls$write_to_dir_nm_1L_chr,
                                 seed_1L_int = params_ls$seed_1L_int)
  if(!is.null(params_ls$prefd_mdl_types_chr)){
    cmprsns_ls$mdl_smry_ls$prefd_mdl_types_chr <- params_ls$prefd_mdl_types_chr
  }
  cmprsns_ls <- write_predr_and_covars_cmprsn(scored_data_tb = data_tb,###
                                              bl_tb = cmprsns_ls$bl_tb,
                                              consent_1L_chr = consent_1L_chr,
                                              consent_indcs_int = consent_indcs_int,
                                              depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                              ds_smry_ls = cmprsns_ls$ds_smry_ls,
                                              mdl_smry_ls  = cmprsns_ls$mdl_smry_ls,
                                              options_chr = options_chr,
                                              output_data_dir_1L_chr = path_params_ls$paths_ls$write_to_dir_nm_1L_chr,
                                              seed_1L_int = params_ls$seed_1L_int,
                                              signft_covars_cdn_1L_chr = signft_covars_cdn_1L_chr)
  if(!is.null(params_ls$prefd_covars_chr)){
    cmprsns_ls$mdl_smry_ls$prefd_covars_chr <- params_ls$prefd_covars_chr
  }
  outp_smry_ls <- write_mdls_with_covars_cmprsn(scored_data_tb = data_tb,###
                                                bl_tb = cmprsns_ls$bl_tb,
                                                combinations_1L_lgl = F, # Correct
                                                consent_1L_chr = consent_1L_chr,
                                                consent_indcs_int = consent_indcs_int,
                                                depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                ds_smry_ls = cmprsns_ls$ds_smry_ls,
                                                existing_predrs_ls = NULL, # Correct
                                                max_nbr_of_covars_1L_int = integer(0), # Correct
                                                mdl_smry_ls = cmprsns_ls$mdl_smry_ls,
                                                options_chr = options_chr,
                                                output_data_dir_1L_chr = path_params_ls$paths_ls$write_to_dir_nm_1L_chr,
                                                seed_1L_int = params_ls$seed_1L_int,
                                                session_data_ls = sessionInfo())
  outp_smry_ls$mdl_nms_ls <- outp_smry_ls$mdl_nms_ls %>%
    purrr::map(~.x[!.x %in% existing_mdls_chr]) %>%
    purrr::compact()
  outp_smry_ls$predr_vars_nms_ls <- outp_smry_ls$predr_vars_nms_ls[outp_smry_ls$predr_vars_nms_ls %>% purrr::map_lgl(~{
    test_chr <- .x
    !any(existing_predrs_ls %>% purrr::map_lgl(~identical(.x,test_chr))
    )})]
  outp_smry_ls <- write_ts_mdls_from_alg_outp(outp_smry_ls = outp_smry_ls,###
                                              backend_1L_chr = backend_1L_chr,
                                              combinations_1L_lgl = combinations_1L_lgl,
                                              consent_1L_chr = consent_1L_chr,
                                              consent_indcs_int = consent_indcs_int,
                                              cores_1L_int = cores_1L_int,
                                              control_ls = params_ls$control_ls,
                                              depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                              existing_predrs_ls = existing_predrs_ls,
                                              iters_1L_int = params_ls$iters_1L_int,
                                              max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int,
                                              new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                              options_chr = options_chr,
                                              path_to_write_to_1L_chr = outp_smry_ls$path_to_write_to_1L_chr,
                                              predictors_lup = params_ls$predictors_lup,
                                              prior_ls = params_ls$prior_ls,
                                              utl_min_val_1L_dbl = params_ls$utl_min_val_1L_dbl)
  return(outp_smry_ls)
}
write_scndry_analysis_dir <- function(paths_ls,
                                      consent_1L_chr = "",
                                      consent_indcs_int = 1L,
                                      options_chr = c("Y", "N"),
                                      reference_1L_int = 1){
  paths_ls <- transform_paths_ls_for_scndry(paths_ls,
                                            reference_1L_int = reference_1L_int)
  ready4::write_new_dirs(paths_ls$write_to_dir_nm_1L_chr,
                         consent_1L_chr = consent_1L_chr,
                         consent_indcs_int = consent_indcs_int,
                         options_chr = options_chr)
  return(paths_ls)
}
write_secondary_analyses <- function(input_params_ls,
                                     backend_1L_chr = "cmdstanr",
                                     combinations_1L_lgl = F,
                                     consent_1L_chr = "",
                                     consent_indcs_int = 1L,
                                     cores_1L_int = 1L,
                                     depnt_var_min_val_1L_dbl = numeric(0),
                                     existing_predrs_ls = NULL,
                                     max_nbr_of_covars_1L_int = integer(0),
                                     new_dir_nm_1L_chr = "F_TS_Mdls",
                                     options_chr = c("Y", "N")){
  references_int <- 1:length(input_params_ls$scndry_anlys_params_ls)
  results_ls <- references_int %>%
    purrr::map(~{
      changes_ls <- input_params_ls$scndry_anlys_params_ls %>%
        purrr::pluck(.x)
      if(is.null(changes_ls$candidate_covar_nms_chr))
        changes_ls$candidate_covar_nms_chr <- input_params_ls$params_ls$candidate_covar_nms_chr %>% transform_names(input_params_ls$rename_lup, invert_1L_lgl = T)
      if(is.null(changes_ls$candidate_predrs_chr)){
        changes_ls$candidate_covar_nms_chr <- changes_ls$candidate_covar_nms_chr[!changes_ls$candidate_covar_nms_chr %in% changes_ls$candidate_predrs_chr]
      }
      write_scndry_analysis(valid_params_ls_ls = input_params_ls,
                            backend_1L_chr = backend_1L_chr,
                            candidate_covar_nms_chr = changes_ls$candidate_covar_nms_chr,
                            candidate_predrs_chr = changes_ls$candidate_predrs_chr,
                            combinations_1L_lgl = combinations_1L_lgl,
                            consent_1L_chr = consent_1L_chr,
                            consent_indcs_int = consent_indcs_int,
                            cores_1L_int = cores_1L_int,
                            depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                            existing_predrs_ls = existing_predrs_ls,
                            max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int,
                            new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                            options_chr = options_chr,
                            path_params_ls = input_params_ls$path_params_ls,
                            predictors_lup = changes_ls$predictors_lup,
                            prefd_covars_chr = changes_ls$prefd_covars_chr,
                            reference_1L_int = .x)})
  return(results_ls)
}
write_shareable_dir <- function(outp_smry_ls,
                                consent_1L_chr = "",
                                consent_indcs_int = 1L,
                                new_dir_nm_1L_chr = "G_Shareable",
                                options_chr = c("Y", "N"),
                                sub_dirs_chr = c("Ingredients","Models","Table_Predn_Tools")){
  output_dir_chr <- write_new_outp_dir(outp_smry_ls$path_to_write_to_1L_chr,
                                       consent_1L_chr = consent_1L_chr,
                                       consent_indcs_int = consent_indcs_int,
                                       new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                       options_chr = options_chr)
  output_dir_chr <- c(output_dir_chr,
                      sub_dirs_chr %>% purrr::map_chr(~write_new_outp_dir(output_dir_chr,
                                                                          consent_1L_chr = consent_1L_chr,
                                                                          consent_indcs_int = consent_indcs_int,
                                                                          new_dir_nm_1L_chr = .x,
                                                                          options_chr = options_chr)))
  return(output_dir_chr)
}
write_shareable_mdls <- function (outp_smry_ls,
                                  consent_1L_chr = "",
                                  consent_indcs_int = 1L,
                                  depnt_var_min_val_1L_dbl = numeric(0),
                                  new_dir_nm_1L_chr = "G_Shareable",
                                  options_chr = c("Y", "N"),
                                  shareable_title_detail_1L_chr = "",
                                  write_mdls_to_dv_1L_lgl = F)
{
  output_dir_chr <- write_shareable_dir(outp_smry_ls = outp_smry_ls,
                                        consent_1L_chr = consent_1L_chr,
                                        consent_indcs_int = consent_indcs_int,
                                        new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                        options_chr = options_chr)
  incld_mdl_paths_chr <- make_incld_mdl_paths(outp_smry_ls)
  fake_ds_tb <- make_fake_ts_data(outp_smry_ls, depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl, depnt_vars_are_NA_1L_lgl = F)
  mdl_types_lup <- outp_smry_ls$mdl_types_lup
  shareable_mdls_ls <- outp_smry_ls$mdl_nms_ls %>% purrr::flatten_chr() %>%
    purrr::map2(incld_mdl_paths_chr, ~{
      model_mdl <- readRDS(paste0(outp_smry_ls$path_to_write_to_1L_chr,"/",.y))

      mdl_smry_tb <- outp_smry_ls$mdls_smry_tb %>% dplyr::filter(Model ==
                                                                   .x)
      mdl_nm_1L_chr <- .x
      mdl_type_1L_chr <- get_mdl_type_from_nm(mdl_nm_1L_chr,
                                              mdl_types_lup = mdl_types_lup)
      tfmn_1L_chr <- ready4::get_from_lup_obj(mdl_types_lup,
                                              match_value_xx = mdl_type_1L_chr,
                                              match_var_nm_1L_chr = "short_name_chr",
                                              target_var_nm_1L_chr = "tfmn_chr",
                                              evaluate_1L_lgl = F)
      predn_type_1L_chr <- ready4::get_from_lup_obj(mdl_types_lup,
                                                    match_value_xx = mdl_type_1L_chr,
                                                    match_var_nm_1L_chr = "short_name_chr",
                                                    target_var_nm_1L_chr = "predn_type_chr",
                                                    evaluate_1L_lgl = F)
      if (is.na(predn_type_1L_chr))
        predn_type_1L_chr <- NULL
      control_1L_chr <- ready4::get_from_lup_obj(mdl_types_lup,
                                                 match_value_xx = mdl_type_1L_chr,
                                                 match_var_nm_1L_chr = "short_name_chr",
                                                 target_var_nm_1L_chr = "control_chr",
                                                 evaluate_1L_lgl = F)
      sd_dbl <- mdl_smry_tb %>%
        dplyr::filter(Parameter == "SD (Intercept)") %>%
        dplyr::select(Estimate, SE) %>%
        t() %>%
        as.vector()
      mdl_fake_ds_tb <- fake_ds_tb %>%
        add_tfd_var_to_ds(depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr,
                          tfmn_1L_chr = tfmn_1L_chr,
                          depnt_var_max_val_1L_dbl = 0.999) %>%
        dplyr::select(names(model_mdl$data))
      model_mdl$data <- mdl_fake_ds_tb
      table_predn_mdl <- make_shareable_mdl(fake_ds_tb = mdl_fake_ds_tb,
                                            mdl_smry_tb = mdl_smry_tb, depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr,
                                            id_var_nm_1L_chr = outp_smry_ls$id_var_nm_1L_chr,
                                            tfmn_1L_chr = tfmn_1L_chr,
                                            mdl_type_1L_chr = mdl_type_1L_chr,
                                            mdl_types_lup = mdl_types_lup,
                                            control_1L_chr = control_1L_chr,
                                            start_1L_chr = NA_character_,
                                            seed_1L_int = outp_smry_ls$seed_1L_int)
      c(4,3) %>%
        purrr::walk2(list(table_predn_mdl, model_mdl),
                     ~{
          ready4::write_with_consent(consented_fn = saveRDS,
                                     prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                            paste0(mdl_nm_1L_chr, ".RDS"),
                                                            " to ",
                                                            output_dir_chr[.x],
                                                            "?"),
                                     consent_1L_chr = consent_1L_chr,
                                     consent_indcs_int = consent_indcs_int,
                                     consented_args_ls = list(object = .y,
                                                              file = paste0(output_dir_chr[.x], "/", mdl_nm_1L_chr,".RDS")),
                                     consented_msg_1L_chr = paste0("File ",
                                                                   paste0(mdl_nm_1L_chr, ".RDS"),
                                                                   " has been written to ",
                                                                   output_dir_chr[.x],
                                                                   "."),
                                     declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                                     options_chr = options_chr)
        })
      scaling_fctr_dbl <- make_scaling_fctr_dbl(outp_smry_ls)
      write_ts_mdl_plts(brms_mdl = model_mdl,
                        consent_1L_chr = consent_1L_chr,
                        consent_indcs_int = consent_indcs_int,
                        depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr,
                        mdl_nm_1L_chr = mdl_nm_1L_chr,
                        options_chr = options_chr,
                        path_to_write_to_1L_chr = output_dir_chr[3],
                        predn_type_1L_chr = predn_type_1L_chr,
                        round_var_nm_1L_chr = outp_smry_ls$round_var_nm_1L_chr,
                        sd_dbl = sd_dbl,
                        sfx_1L_chr = " from table",
                        table_predn_mdl = table_predn_mdl,
                        tfd_data_tb = outp_smry_ls$scored_data_tb %>%
                          transform_tb_to_mdl_inp(depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                  depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr,
                                                  predr_vars_nms_chr = outp_smry_ls$predr_vars_nms_ls %>% purrr::flatten_chr() %>% unique(),
                                                  id_var_nm_1L_chr = outp_smry_ls$id_var_nm_1L_chr,
                                                  round_var_nm_1L_chr = outp_smry_ls$round_var_nm_1L_chr,
                                                  round_bl_val_1L_chr = outp_smry_ls$round_bl_val_1L_chr,
                                                  scaling_fctr_dbl = scaling_fctr_dbl),
                        tfmn_1L_chr = tfmn_1L_chr,
                        utl_min_val_1L_dbl = ifelse(!is.null(outp_smry_ls$utl_min_val_1L_dbl),
                                                    outp_smry_ls$utl_min_val_1L_dbl,
                                                    -1))
      table_predn_mdl
    }) %>% stats::setNames(outp_smry_ls$mdl_nms_ls %>% purrr::flatten_chr())
  outp_smry_ls$shareable_mdls_ls <- shareable_mdls_ls
  outp_smry_ls$shareable_mdls_tb <-  NULL
  ingredients_ls <- list(depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr,
                         dictionary_tb = outp_smry_ls$dictionary_tb %>%
                           dplyr::filter(var_nm_chr %in% names(fake_ds_tb)),
                         id_var_nm_1L_chr = outp_smry_ls$id_var_nm_1L_chr,
                         fake_ds_tb = fake_ds_tb,
                         mdls_lup = outp_smry_ls$shareable_mdls_ls %>%
                           purrr::map2_dfr(names(outp_smry_ls$shareable_mdls_ls),
                                           ~{
                                             if(inherits(.x,"betareg")){
                                               coeffs_dbl <- .x$coefficients$mean
                                             }else{
                                               coeffs_dbl <- .x$coefficients
                                             }
                                             mdl_type_1L_chr = get_mdl_type_from_nm(.y,
                                                                                    mdl_types_lup = outp_smry_ls$mdl_types_lup)
                                             tibble::tibble(mdl_nms_chr = .y) %>%
                                               dplyr::mutate(predrs_ls = list(coeffs_dbl %>%
                                                                                names() %>%
                                                                                stringr::str_remove_all("_change") %>%
                                                                                stringr::str_remove_all("_baseline") %>%
                                                                                stringr::str_remove_all("_scaled") %>%
                                                                                stringr::str_remove_all("_unscaled") %>%
                                                                                unique() %>%
                                                                                purrr::discard(~ .x== "(Intercept)")),
                                                             mdl_type_chr = mdl_type_1L_chr,
                                                             tfmn_chr = ready4::get_from_lup_obj(outp_smry_ls$mdl_types_lup,
                                                                                                 match_value_xx = mdl_type_1L_chr,
                                                                                                 match_var_nm_1L_chr = "short_name_chr",
                                                                                                 target_var_nm_1L_chr = "tfmn_chr",
                                                                                                 evaluate_1L_lgl = F))
                                           }),
                         mdls_smry_tb = outp_smry_ls$mdls_smry_tb,
                         mdl_types_lup = mdl_types_lup,
                         predictors_lup = outp_smry_ls$predictors_lup,
                         round_var_nm_1L_chr = outp_smry_ls$round_var_nm_1L_chr,
                         seed_1L_int = outp_smry_ls$seed_1L_int,
                         utl_min_val_1L_dbl = ifelse(!is.null(outp_smry_ls$utl_min_val_1L_dbl),
                                                     outp_smry_ls$utl_min_val_1L_dbl,
                                                     -1))
  ready4::write_with_consent(consented_fn = saveRDS,
                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                    paste0("mdl_ingredients", ".RDS"),
                                                    " to ",
                                                    output_dir_chr[2],
                                                    "?"),
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(object = ingredients_ls,
                                                      file = paste0(output_dir_chr[2], "/", "mdl_ingredients",".RDS")),
                             consented_msg_1L_chr = paste0("File ",
                                                           paste0("mdl_ingredients", ".RDS"),
                                                           " has been written to ",
                                                           output_dir_chr[2],
                                                           "."),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr)
  outp_smry_ls <- write_mdls_to_dv(outp_smry_ls,
                                   consent_1L_chr = consent_1L_chr,
                                   consent_indcs_int = consent_indcs_int,
                                   new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                   options_chr = options_chr,
                                   output_dir_chr = output_dir_chr,
                                   shareable_title_detail_1L_chr = shareable_title_detail_1L_chr)
  return(outp_smry_ls)
}
write_shareable_mdls_to_dv <- function (outp_smry_ls,
                                        consent_1L_chr = "",
                                        consent_indcs_int = 1L,
                                        new_dir_nm_1L_chr = "G_Shareable",
                                        options_chr = c("Y", "N"),
                                        shareable_title_detail_1L_chr = "",
                                        share_ingredients_1L_lgl = T,
                                        output_dir_chr = NA_character_){
  if(is.na(output_dir_chr[1]))
    output_dir_chr <- write_shareable_dir(outp_smry_ls = outp_smry_ls,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  if(share_ingredients_1L_lgl){
    shareable_mdls_tb <- tibble::tibble(ds_obj_nm_chr = "mdl_ingredients",
                                        title_chr = "An R object that can be used to construct model objects from tables of coefficients. Contains a synthetic dataset.")
  }else{
    shareable_mdls_tb <- tibble::tibble(ds_obj_nm_chr = names(outp_smry_ls$shareable_mdls_ls),
                                        title_chr = paste0("A shareable (contains no confidential data) statistical model, ",
                                                           names(outp_smry_ls$shareable_mdls_ls), ".",
                                                           shareable_title_detail_1L_chr))
  }
  ready4::write_to_dv_with_wait(shareable_mdls_tb,
                                consent_1L_chr = consent_1L_chr,
                                consent_indcs_int = consent_indcs_int,
                                dv_nm_1L_chr = outp_smry_ls$dv_ls$dv_nm_1L_chr,
                                ds_url_1L_chr = outp_smry_ls$dv_ls$ds_url_1L_chr,
                                inc_fl_types_chr = ".RDS",
                                options_chr = options_chr,
                                parent_dv_dir_1L_chr = outp_smry_ls$dv_ls$parent_dv_dir_1L_chr,
                                paths_to_dirs_chr = output_dir_chr[ifelse(share_ingredients_1L_lgl,
                                                                          2,
                                                                          3)],
                                paths_are_rltv_1L_lgl = F)
  if(!share_ingredients_1L_lgl){
    ds_ls <- dataverse::get_dataset(outp_smry_ls$dv_ls$ds_url_1L_chr)
    shareable_mdls_tb <- shareable_mdls_tb %>%
      dplyr::mutate(dv_nm_chr = outp_smry_ls$dv_ls$dv_nm_1L_chr,
                    fl_ids_int = ds_obj_nm_chr %>%
                      purrr::map_int(~ready4::get_fl_id_from_dv_ls(ds_ls,
                                                                   fl_nm_1L_chr = paste0(.x, ".RDS")) %>%
                                       as.integer()))

  }
  return(shareable_mdls_tb)
}
write_sngl_predr_multi_mdls_outps <- function (data_tb,
                                               dictionary_tb,
                                               mdl_types_chr,
                                               path_to_write_to_1L_chr,
                                               predr_vals_dbl,
                                               predr_var_desc_1L_chr,
                                               predr_var_nm_1L_chr,
                                               consent_1L_chr = "",
                                               consent_indcs_int = 1L,
                                               covar_var_nms_chr = NA_character_,
                                               depnt_var_min_val_1L_dbl = numeric(0),
                                               depnt_var_nm_1L_chr = "utl_total_w", # Remove default
                                               fl_nm_pfx_1L_chr = "A_RT_",
                                               folds_1L_int = 10,
                                               mdl_types_lup = NULL,
                                               new_dir_nm_1L_chr =  "A_Candidate_Mdls_Cmprsn",
                                               options_chr = c("Y", "N"),
                                               plt_indcs_int = NA_integer_,
                                               start_1L_chr = NULL)
{
  if (is.null(mdl_types_lup))
    utils::data("mdl_types_lup", envir = environment())
  data_tb <- transform_ds_for_mdlng(data_tb, depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                    depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                    predr_var_nm_1L_chr = predr_var_nm_1L_chr, covar_var_nms_chr = covar_var_nms_chr)
  output_dir_1L_chr <- write_new_outp_dir(path_to_write_to_1L_chr,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  ready4show::write_mdl_plt_fl(plt_fn = make_tfmn_cmprsn_plt,
                               consent_1L_chr = consent_1L_chr,
                               consent_indcs_int = consent_indcs_int,
                               fn_args_ls = list(data_tb = data_tb,
                                                 depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                 dictionary_tb = dictionary_tb),
                               height_1L_dbl = 6,
                               options_chr = options_chr,
                               path_to_write_to_1L_chr = output_dir_1L_chr,
                               plt_nm_1L_chr = "A_TFMN_CMPRSN_DNSTY",
                               width_1L_dbl = 10)
  smry_of_sngl_predr_mdls_tb <- purrr::map_dfr(mdl_types_chr,
                                               ~{
                                                 tfmn_1L_chr <- ready4::get_from_lup_obj(mdl_types_lup,
                                                                                         match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x,
                                                                                         target_var_nm_1L_chr = "tfmn_chr", evaluate_1L_lgl = F)
                                                 write_mdl_type_sngl_outps(data_tb,
                                                                           consent_1L_chr = consent_1L_chr,
                                                                           consent_indcs_int = consent_indcs_int,
                                                                           covar_var_nms_chr = covar_var_nms_chr,
                                                                           depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                                                           depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                                           folds_1L_int = folds_1L_int,
                                                                           mdl_fl_nm_1L_chr = paste0(fl_nm_pfx_1L_chr,
                                                                                                     predr_var_nm_1L_chr, "_", .x),
                                                                           mdl_type_1L_chr = .x,
                                                                           mdl_types_lup = mdl_types_lup,
                                                                           options_chr = options_chr,
                                                                           path_to_write_to_1L_chr = output_dir_1L_chr,
                                                                           plt_indcs_int = plt_indcs_int,
                                                                           predr_vals_dbl = predr_vals_dbl,
                                                                           predr_var_nm_1L_chr = predr_var_nm_1L_chr,
                                                                           predr_var_desc_1L_chr = predr_var_desc_1L_chr,
                                                                           start_1L_chr = start_1L_chr,
                                                                           tfmn_1L_chr = tfmn_1L_chr)
                                               })
  if (!is.null(folds_1L_int))
    smry_of_sngl_predr_mdls_tb <- smry_of_sngl_predr_mdls_tb %>%
    dplyr::arrange(dplyr::desc(RsquaredP))
  return(smry_of_sngl_predr_mdls_tb)
}
write_study_outp_ds <- function(input_params_ls,
                                abstract_args_ls = NULL,
                                consent_1L_chr = "",
                                consent_indcs_int = 1L,
                                dv_mdl_desc_1L_chr = "An R model.",# Generalise / remove default.
                                inc_fl_types_chr = ".pdf",
                                options_chr = c("Y", "N"),
                                purge_data_1L_lgl = FALSE,
                                start_at_int = c(2,1)){
    header_yaml_args_ls <- input_params_ls$header_yaml_args_ls
    path_params_ls <- input_params_ls$path_params_ls
    output_format_ls <- input_params_ls$output_format_ls
    use_fake_data_1L_lgl <- input_params_ls$params_ls$use_fake_data_1L_lgl
    dv_ds_nm_and_url_chr <- input_params_ls$path_params_ls$dv_ds_nm_and_url_chr
    rprt_lups_ls <- input_params_ls$rprt_lups_ls
  paths_ls <- path_params_ls$paths_ls
  rprt_lups_ls %>%
    purrr::walk2(names(rprt_lups_ls),
                 ~ {
                   rprt_lup <- .x
                   reference_1L_int <- ifelse(.y == "Primary",
                                              0,
                                              as.numeric(stringr::str_remove(.y,"secondary_")))
                   if(is.null(rprt_lup)){
                     data("rprt_lup", package = "specific", envir = environment())
                     rprt_lup <- transform_rprt_lup(rprt_lup,
                                                    add_suplry_rprt_1L_lgl = reference_1L_int > 0,
                                                    add_sharing_rprt_1L_lgl = T,
                                                    start_at_int = start_at_int,
                                                    reference_1L_int = reference_1L_int)
                                       }
                    if(reference_1L_int==0){
                     included_rprts_chr <- rprt_lup$rprt_nms_chr[rprt_lup$rprt_nms_chr != "AAA_SHARING_MTH"]
                     transform_paths_ls <- NULL
                   }else{
                     included_rprts_chr <- c("AAA_SUPLRY_ANLYS_MTH",paste0("AAA_TTU_MDL_CTG-",reference_1L_int))[min(2,reference_1L_int):2]
                     transform_paths_ls = list(fn = transform_paths_ls_for_scndry,
                                               args_ls = list(reference_1L_int = reference_1L_int,
                                                              remove_prmry_1L_lgl = T,
                                                              remove_mkdn_1L_lgl = T))
                     paths_ls <- rlang::exec(transform_paths_ls$fn, paths_ls, !!!transform_paths_ls$args_ls)
                   }
                   params_ls <- list(dv_ds_nm_and_url_chr = dv_ds_nm_and_url_chr,
                                     dv_mdl_desc_1L_chr = dv_mdl_desc_1L_chr,
                                     inc_fl_types_chr = inc_fl_types_chr,
                                     nbr_of_digits_1L_int = output_format_ls$supplementary_digits_1L_int,
                                     output_type_1L_chr = output_format_ls$supplementary_outp_1L_chr,
                                     rprt_lup = rprt_lup %>%
                                       dplyr::filter(rprt_nms_chr %in% included_rprts_chr),
                                     share_mdls_1L_lgl = (reference_1L_int==0),
                                     subtitle_1L_chr = ready4::get_from_lup_obj(rprt_lup,
                                                                                   match_value_xx = "AAA_SHARING_MTH",
                                                                                   match_var_nm_1L_chr = "rprt_nms_chr",
                                                                                   target_var_nm_1L_chr = "title_chr",
                                                                                   evaluate_1L_lgl = F),
                                     transform_paths_ls = transform_paths_ls,
                                     use_fake_data_1L_lgl = use_fake_data_1L_lgl) %>%
                     append(path_params_ls[1:2])
                   params_ls %>%
                     ready4show::write_report(abstract_args_ls = abstract_args_ls,
                                              consent_1L_chr = consent_1L_chr,
                                              consent_indcs_int = consent_indcs_int,
                                              header_yaml_args_ls = header_yaml_args_ls,
                                              options_chr = options_chr,
                                              paths_ls = paths_ls,
                                              rprt_lup = rprt_lup,
                                              rprt_nm_1L_chr = "AAA_SHARING_MTH")
                 })
  ready4::write_to_dv_with_wait(consent_1L_chr = consent_1L_chr,
                                consent_indcs_int = consent_indcs_int,
                                dss_tb = tibble::tibble(ds_obj_nm_chr = "AAA_SHARING_MTH",
                                                        title_chr =  rprt_lups_ls[[1]] %>%
                                                          ready4::get_from_lup_obj(match_value_xx = "AAA_SHARING_MTH",
                                                                                      match_var_nm_1L_chr = "rprt_nms_chr",
                                                                                      target_var_nm_1L_chr = "title_chr",
                                                                                      evaluate_1L_lgl = F)),
                                dv_nm_1L_chr = dv_ds_nm_and_url_chr[1],
                                ds_url_1L_chr = dv_ds_nm_and_url_chr[2],
                                inc_fl_types_chr = inc_fl_types_chr,
                                options_chr = options_chr,
                                parent_dv_dir_1L_chr = paths_ls$dv_dir_1L_chr,
                                paths_are_rltv_1L_lgl = F,
                                paths_to_dirs_chr = paths_ls$reports_dir_1L_chr)
  return(dv_ds_nm_and_url_chr)
}
write_to_delete_ds_copies <- function(input_params_ls = NULL,
                                      consent_1L_chr = "",
                                      consent_indcs_int = 1L,
                                      options_chr = c("Y", "N"),
                                      paths_ls = NULL){
  if(is.null(paths_ls))
    paths_ls <- input_params_ls$path_params_ls$paths_ls
  paths_to_outp_chr <- c(paste0(paths_ls$output_data_dir_1L_chr,"/I_ALL_OUTPUT_.RDS"))
  secondary_refs_int <- NULL
  if(!is.null(input_params_ls$scndry_anlys_params_ls)){
    1:length(input_params_ls$scndry_anlys_params_ls)
    paths_to_outp_chr <- c(paths_to_outp_chr,secondary_refs_int %>% purrr::map_chr(~here::here(paths_ls$path_from_top_level_1L_chr,
                                                                                               paths_ls$write_to_dir_nm_1L_chr,
                                                                                               paste0("secondary_",.x),
                                                                                               "Output","I_ALL_OUTPUT_.RDS")))
  }
  paths_to_outp_chr %>%
    purrr::walk(~{
      outp_smry_ls <- readRDS(.x)
      write_to_delete_mdl_fls(outp_smry_ls,
                              consent_1L_chr = consent_1L_chr,
                              consent_indcs_int = consent_indcs_int,
                              options_chr = options_chr)
      outp_smry_ls$scored_data_tb <- NULL
      ready4::write_with_consent(consented_fn = saveRDS,
                                 prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                        .x,
                                                        "?"),
                                 consent_1L_chr = consent_1L_chr,
                                 consent_indcs_int = consent_indcs_int,
                                 consented_args_ls = list(object = outp_smry_ls,
                                                          file = .x),
                                 consented_msg_1L_chr = paste0("File ",
                                                               .x,
                                                               " has been written."),
                                 declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                                 options_chr = options_chr)
    })
}
write_to_delete_mdl_fls <- function (outp_smry_ls,
                                     consent_1L_chr = "",
                                     consent_indcs_int = 1L,
                                     options_chr = c("Y", "N")){
  paths_to_mdls_chr <- outp_smry_ls$file_paths_chr[outp_smry_ls$file_paths_chr %>%
                                                     purrr::map_lgl(~endsWith(.x, ".RDS") & (startsWith(.x,
                                                                                                        "A_Candidate_Mdls_Cmprsn") | startsWith(.x, "C_Predrs_Sngl_Mdl_Cmprsn") |
                                                                                               startsWith(.x, "D_Predr_Covars_Cmprsn") | startsWith(.x,
                                                                                                                                                    "E_Predrs_W_Covars_Sngl_Mdl_Cmprsn") | startsWith(.x,
                                                                                                                                                                                                      "F_TS_Mdls")) & !endsWith(.x, "mdls_smry_tb.RDS"))]
  consented_fn <- function(outp_smry_ls, paths_to_mdls_chr) {
    paths_to_mdls_chr %>% purrr::walk(~unlink(paste0(outp_smry_ls$path_to_write_to_1L_chr,
                                                     "/", .x)))
  }
  ready4::write_with_consent(consented_fn = consented_fn,
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(outp_smry_ls = outp_smry_ls,
                                                      paths_to_mdls_chr = paths_to_mdls_chr),
                             consented_msg_1L_chr = paste0("File",
                                                           ifelse(length(paths_to_mdls_chr) > 1,
                                                                  paste0("s ",
                                                                         ready4::make_list_phrase(paths_to_mdls_chr),
                                                                         " have been deleted."),
                                                                  paste0(" ", paths_to_mdls_chr,
                                                                         " has been deleted."))),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr,
                             prompt_1L_chr = paste0("Do you confirm that you want to delete the file",
                                                    ifelse(length(paths_to_mdls_chr) > 1, paste0("s ",
                                                                                                 ready4::make_list_phrase(paths_to_mdls_chr)),
                                                           paste0(" ", paths_to_mdls_chr)), "?"))
}
write_ts_mdl_plts <- function (brms_mdl, # Rename lngl
                               mdl_nm_1L_chr,
                               path_to_write_to_1L_chr,
                               tfd_data_tb,
                               args_ls = NULL,
                               consent_1L_chr = "",
                               consent_indcs_int = 1L,
                               depnt_var_desc_1L_chr = "Utility score",
                               depnt_var_nm_1L_chr = "utl_total_w",
                               height_dbl = c(rep(6, 2), rep(5,8)),
                               options_chr = c("Y", "N"),
                               predn_type_1L_chr = NULL,
                               round_var_nm_1L_chr = "round",
                               rsl_dbl = rep(300,10),
                               sd_dbl = NA_real_,
                               seed_1L_dbl = 23456,
                               sfx_1L_chr = " from table",
                               table_predn_mdl = NULL,
                               tfmn_1L_chr = "NTF",
                               units_1L_chr = "in",
                               utl_min_val_1L_dbl = -1,
                               width_dbl = c(rep(6, 2), rep(6, 8)))
{
  set.seed(seed_1L_dbl)
  tfd_data_tb <- transform_ds_for_all_cmprsn_plts(depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                  is_brms_mdl_1L_lgl = inherits(brms_mdl,"brmsfit"),
                                                  model_mdl = brms_mdl,
                                                  predn_type_1L_chr = predn_type_1L_chr,
                                                  sd_dbl = NA_real_,
                                                  sfx_1L_chr = ifelse(inherits(brms_mdl,"brmsfit"),
                                                                      " from brmsfit",
                                                                      sfx_1L_chr),
                                                  tfd_data_tb = tfd_data_tb,
                                                  tfmn_1L_chr = tfmn_1L_chr,
                                                  utl_min_val_1L_dbl = utl_min_val_1L_dbl)
  if(!is.null(table_predn_mdl)){
    tfd_data_tb <- transform_ds_for_all_cmprsn_plts(depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                    is_brms_mdl_1L_lgl = F,
                                                    model_mdl = table_predn_mdl,
                                                    predn_type_1L_chr = predn_type_1L_chr,
                                                    sd_dbl = sd_dbl,
                                                    sfx_1L_chr = ifelse(is.null(brms_mdl),
                                                                        " from table",
                                                                        sfx_1L_chr),
                                                    tfd_data_tb = tfd_data_tb,
                                                    tfmn_1L_chr = tfmn_1L_chr,
                                                    utl_min_val_1L_dbl = utl_min_val_1L_dbl)
  }
  plt_nms_chr <- paste0(mdl_nm_1L_chr, "_",
                        c("coefs", "hetg",
                          "dnst", "sctr_plt",
                          "sim_dnst", "sim_sctr",
                          "cnstrd_dnst","cnstrd_sctr_plt",
                          "cnstrd_sim_dnst", "cnstrd_sim_sctr"))
  mdl_plts_paths_ls <- purrr::map(ifelse(inherits(brms_mdl,"brmsfit"),1,3):10, ~{
    plt_fn <- fn_args_ls <- NULL
    if (.x %in% c(1, 2)) {
      plt <- plot(brms_mdl, ask = F, plot = F)
      if (length(plt) >= .x) {
        fn_args_ls <- list(brms_mdl = brms_mdl, idx_1L_int = as.integer(.x))
        plt_fn <- function(brms_mdl, idx_1L_int) {
          plot(brms_mdl, ask = F, plot = F)[idx_1L_int]
        }
      }
    }  else {
      plot_fn_and_args_ls <- make_plot_fn_and_args_ls(args_ls = args_ls,
                                                      brms_mdl = NULL, # This is correct
                                                      depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                                                      depnt_var_desc_1L_chr = depnt_var_desc_1L_chr,
                                                      predn_type_1L_chr =  predn_type_1L_chr,
                                                      round_var_nm_1L_chr = round_var_nm_1L_chr,
                                                      sd_dbl = sd_dbl,
                                                      seed_1L_dbl = seed_1L_dbl,
                                                      sfx_1L_chr = ifelse(is.null(table_predn_mdl),
                                                                          ifelse(inherits(brms_mdl,"brmsfit"),
                                                                                 " from brmsfit",
                                                                                 sfx_1L_chr),
                                                                          ifelse(is.null(brms_mdl),
                                                                                 " from table",
                                                                                 sfx_1L_chr)),
                                                      table_predn_mdl = table_predn_mdl,
                                                      tfd_data_tb = tfd_data_tb,
                                                      tfmn_1L_chr = tfmn_1L_chr,
                                                      type_1L_chr = c("coefs", "hetg",
                                                                      "dnst", "sctr_plt",
                                                                      "sim_dnst", "sim_sctr",
                                                                      "cnstrd_dnst","cnstrd_sctr_plt",
                                                                      "cnstrd_sim_dnst", "cnstrd_sim_sctr")[.x])
      plt_fn <- plot_fn_and_args_ls$plt_fn
      fn_args_ls <- plot_fn_and_args_ls$fn_args_ls
    }
    ready4show::write_mdl_plt_fl(plt_fn,
                                 consent_1L_chr = consent_1L_chr,
                                 consent_indcs_int = consent_indcs_int,
                                 fn_args_ls = fn_args_ls,
                                 height_1L_dbl = height_dbl[.x],
                                 options_chr = options_chr,
                                 path_to_write_to_1L_chr = path_to_write_to_1L_chr,
                                 plt_nm_1L_chr = plt_nms_chr[.x],
                                 rsl_1L_dbl = rsl_dbl[.x],
                                 units_1L_chr = units_1L_chr,
                                 width_1L_dbl = width_dbl[.x])
  }) %>%
    stats::setNames(plt_nms_chr[ifelse(inherits(brms_mdl,"brmsfit"),1,3):10]) %>%
    purrr::discard(is.na)
  return(mdl_plts_paths_ls)
}
write_ts_mdls <- function (data_tb,
                           mdl_types_lup,
                           mdl_nms_ls,
                           mdl_smry_dir_1L_chr,
                           predictors_lup,
                           predr_vars_nms_ls,
                           backend_1L_chr = getOption("brms.backend", "rstan"),
                           consent_1L_chr = "",
                           consent_indcs_int = 1L,
                           control_ls = NULL,
                           cores_1L_int = 1L,
                           depnt_var_min_val_1L_dbl = numeric(0),
                           depnt_var_nm_1L_chr = "utl_total_w",
                           id_var_nm_1L_chr = "fkClientID",
                           iters_1L_int = 4000L,
                           options_chr = c("Y", "N"),
                           prior_ls = NULL,
                           round_var_nm_1L_chr = "round",
                           round_bl_val_1L_chr = "Baseline",
                           seed_1L_int = 1000L,
                           utl_min_val_1L_dbl = -1){
  if (!dir.exists(mdl_smry_dir_1L_chr))
    dir.create(mdl_smry_dir_1L_chr)
  args_ls <- list(backend_1L_chr = backend_1L_chr,
                  consent_1L_chr = consent_1L_chr,
                  consent_indcs_int = consent_indcs_int,
                  control_ls = control_ls,
                  data_tb = data_tb,
                  mdl_nms_ls = mdl_nms_ls,
                  mdl_smry_dir_1L_chr = mdl_smry_dir_1L_chr,
                  mdl_types_lup = mdl_types_lup,
                  predictors_lup = predictors_lup,
                  predr_vars_nms_ls = predr_vars_nms_ls,
                  depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                  depnt_var_nm_1L_chr = depnt_var_nm_1L_chr,
                  id_var_nm_1L_chr = id_var_nm_1L_chr,
                  iters_1L_int = iters_1L_int,
                  options_chr = options_chr,
                  prior_ls = prior_ls,
                  round_var_nm_1L_chr = round_var_nm_1L_chr,
                  round_bl_val_1L_chr = round_bl_val_1L_chr,
                  seed_1L_int = seed_1L_int,
                  utl_min_val_1L_dbl = utl_min_val_1L_dbl)
  if(cores_1L_int>1){
    threaded_ls <- parallel::mclapply(1:length(mdl_nms_ls),
                                      function(idx_1L_int, args_ls){rlang::exec(make_inner_loop_mdl_smry, idx_1L_int , !!!args_ls)},
                                      args_ls,
                                      mc.cores = cores_1L_int)
    mdls_smry_tb <- threaded_ls %>% purrr::map_dfr(~.x)
  }else{
    mdls_smry_tb <- purrr::map_dfr(1:length(mdl_nms_ls), ~{
      rlang::exec(make_inner_loop_mdl_smry, .x, !!!args_ls)
    })
  }
  ready4::write_with_consent(consented_fn = saveRDS,
                             consent_1L_chr = consent_1L_chr,
                             consent_indcs_int = consent_indcs_int,
                             consented_args_ls = list(object = mdls_smry_tb,
                                                      file = paste0(mdl_smry_dir_1L_chr, "/mdls_smry_tb.RDS")),
                             consented_msg_1L_chr = paste0("File ",
                                                           paste0(mdl_smry_dir_1L_chr, "/mdls_smry_tb.RDS"),
                                                           " has been written"),
                             declined_msg_1L_chr = "Write request cancelled - no new files have been written.",
                             options_chr = options_chr,
                             prompt_1L_chr = paste0("Do you confirm that you want to write the file ",
                                                    paste0(mdl_smry_dir_1L_chr, "/mdls_smry_tb.RDS"),
                                                    "?"))
  return(mdls_smry_tb)
}
write_ts_mdls_from_alg_outp <- function (outp_smry_ls, # rename lngl
                                         predictors_lup,
                                         backend_1L_chr = getOption("brms.backend", "rstan"),
                                         combinations_1L_lgl = F,
                                         consent_1L_chr = "",
                                         consent_indcs_int = 1L,
                                         control_ls = NULL,
                                         cores_1L_int = 1L,
                                         depnt_var_min_val_1L_dbl = numeric(0),
                                         existing_predrs_ls = NULL,
                                         iters_1L_int = 4000L,
                                         max_nbr_of_covars_1L_int = integer(0),
                                         new_dir_nm_1L_chr = "F_TS_Mdls",
                                         options_chr = c("Y", "N"),
                                         path_to_write_to_1L_chr = NA_character_,
                                         prior_ls = NULL,
                                         utl_min_val_1L_dbl = -1){
  if(is.na(path_to_write_to_1L_chr))
    path_to_write_to_1L_chr <- outp_smry_ls$path_to_write_to_1L_chr %>%
      stringr::str_sub(end=-8)
  output_dir_1L_chr <- write_new_outp_dir(path_to_write_to_1L_chr,
                                          consent_1L_chr = consent_1L_chr,
                                          consent_indcs_int = consent_indcs_int,
                                          new_dir_nm_1L_chr = new_dir_nm_1L_chr,
                                          options_chr = options_chr)
  outp_smry_ls$predr_vars_nms_ls <- make_predr_vars_nms_ls(main_predrs_chr = outp_smry_ls$predr_cmprsn_tb$predr_chr,
                                                           covars_ls = list(outp_smry_ls$prefd_covars_chr),
                                                           combinations_1L_lgl = combinations_1L_lgl,
                                                           existing_predrs_ls = existing_predrs_ls,
                                                           max_nbr_of_covars_1L_int = max_nbr_of_covars_1L_int)
  outp_smry_ls$mdl_nms_ls <- make_mdl_nms_ls(outp_smry_ls$predr_vars_nms_ls,
                                             mdl_types_chr = outp_smry_ls$prefd_mdl_types_chr)
  mdls_smry_tb <- write_ts_mdls(backend_1L_chr = backend_1L_chr,
                                consent_1L_chr = consent_1L_chr,
                                consent_indcs_int = consent_indcs_int,
                                control_ls = control_ls,
                                cores_1L_int = cores_1L_int,
                                data_tb = outp_smry_ls$scored_data_tb,
                                depnt_var_min_val_1L_dbl = depnt_var_min_val_1L_dbl,
                                depnt_var_nm_1L_chr = outp_smry_ls$depnt_var_nm_1L_chr, predr_vars_nms_ls = outp_smry_ls$predr_vars_nms_ls,
                                id_var_nm_1L_chr = outp_smry_ls$id_var_nm_1L_chr,
                                iters_1L_int = iters_1L_int,
                                round_var_nm_1L_chr = outp_smry_ls$round_var_nm_1L_chr,
                                mdl_nms_ls = outp_smry_ls$mdl_nms_ls,
                                mdl_smry_dir_1L_chr = output_dir_1L_chr,
                                mdl_types_lup = outp_smry_ls$mdl_types_lup,
                                options_chr = options_chr,
                                predictors_lup = predictors_lup,
                                prior_ls = prior_ls,
                                round_bl_val_1L_chr = outp_smry_ls$round_bl_val_1L_chr,
                                seed_1L_int = outp_smry_ls$seed_1L_int,
                                utl_min_val_1L_dbl = utl_min_val_1L_dbl)
  outp_smry_ls$mdls_smry_tb <- mdls_smry_tb
  outp_smry_ls$utl_min_val_1L_dbl <- utl_min_val_1L_dbl
  outp_smry_ls$file_paths_chr <- list.files(outp_smry_ls$path_to_write_to_1L_chr, recursive = T)
  outp_smry_ls$combinations_1L_lgl <- combinations_1L_lgl
  outp_smry_ls$max_nbr_of_covars_1L_int <- max_nbr_of_covars_1L_int
  return(outp_smry_ls)
}
