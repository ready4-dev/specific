get_background_text <- function(results_ls){
  text_1L_chr <- results_ls$study_descs_ls$background_1L_chr
  return(text_1L_chr)
}
get_brms_mdl <- function(outp_smry_ls,
                         mdl_nm_1L_chr){
  ranked_mdl_nms_chr <- outp_smry_ls$mdl_nms_ls %>% purrr::flatten_chr()
  incld_mdl_paths_chr <- make_incld_mdl_paths(outp_smry_ls)
  brms_mdl <- readRDS(paste0(outp_smry_ls$path_to_write_to_1L_chr,"/",
                             incld_mdl_paths_chr[incld_mdl_paths_chr %>% endsWith(paste0(mdl_nm_1L_chr,".RDS"))]))
  return(brms_mdl)
}
get_conclusion_text <- function(results_ls){
  text_1L_chr <- results_ls$study_descs_ls$conclusion_1L_chr
  return(text_1L_chr)
}
get_cndt_mdls <- function(filter_1L_lgl = T,
                          mdl_short_nms_chr = NA_character_,
                          mdl_types_lup = NULL){
  cndt_mdls_lup <- get_cndts_for_mxd_mdls(mdl_types_lup = mdl_types_lup,
                                          filter_1L_lgl = filter_1L_lgl)
  if(!is.na(mdl_short_nms_chr[1])){
    cndt_mdls_lup <- cndt_mdls_lup %>%
      dplyr::filter(short_name_chr %in% mdl_short_nms_chr)
  }
  cndt_mdls_lup <- cndt_mdls_lup %>%
    specific_models()
  return(cndt_mdls_lup)
}
get_cndts_for_mxd_mdls <- function(mdl_types_lup = NULL,
                                   filter_1L_lgl = T){
  if(is.null(mdl_types_lup))
    utils::data("mdl_types_lup", package = "specific", envir = environment())
  cndts_for_mxd_mdls_lup <- mdl_types_lup
  if(filter_1L_lgl)
  cndts_for_mxd_mdls_lup <- cndts_for_mxd_mdls_lup  %>%
    dplyr::filter(!tfmn_for_bnml_lgl,
                  short_name_chr != "BET_LOG" )
  return(cndts_for_mxd_mdls_lup)
}
get_covars_by_ctg <-  function(results_ls,
                               collapse_1L_lgl = F){
  covars_by_ctg_ls <- results_ls$candidate_covars_ls %>%
    purrr::map( ~ .x) %>%
    stats::setNames(get_covar_ctgs(results_ls,
                                   collapse_1L_lgl = F))
  if(collapse_1L_lgl){
    covars_by_ctg_ls <- covars_by_ctg_ls %>%
      purrr::map2(names(covars_by_ctg_ls),
                  ~{
                    covars_1L_chr <- .x  %>%
                      sort() %>%
                      ready4::make_list_phrase()
                    paste0(ifelse(length(.x)>1, .y %>% Hmisc::capitalize(), paste0("The ",.y)),
                           " covariate",
                           ifelse(length(.x)>1,"s were "," was "),
                           covars_1L_chr,".")
                  })
  }
  return(covars_by_ctg_ls)
}
get_covar_ctgs <- function(results_ls,
                           collapse_1L_lgl = T){
  covar_ctgs_chr <- names(results_ls$candidate_covars_ls) %>% tolower()
  if(collapse_1L_lgl){
    covar_ctgs_chr  <- covar_ctgs_chr %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed(","," and")
  }
  return(covar_ctgs_chr )
}
get_hlth_utl_nm <- function(results_ls, # Generalise to dep var
                            short_nm_1L_lgl = T){
  health_utl_nm_1L_chr <- ifelse(short_nm_1L_lgl,
                                 results_ls$study_descs_ls$health_utl_nm_1L_chr,
                                 results_ls$study_descs_ls$health_utl_long_nm_1L_chr)
  return(health_utl_nm_1L_chr)
}
get_hlth_utl_stat <- function(results_ls, # Generalise to dep var
                              stat_1L_chr = "bl_mean"){
  hlth_utl_stat_1L_chr <- switch(stat_1L_chr,
                                 "bl_mean" = results_ls$hlth_utl_and_predrs_ls$bl_hu_mean_1L_dbl,
                                 "bl_sd" = results_ls$hlth_utl_and_predrs_ls$bl_hu_sd_1L_dbl,
                                 "fup_mean" = results_ls$hlth_utl_and_predrs_ls$fup_hu_mean_1L_dbl,
                                 "fup_sd" = results_ls$hlth_utl_and_predrs_ls$fup_hu_sd_1L_dbl)
  return(hlth_utl_stat_1L_chr)
}
get_lngl_ttu_types <- function(results_ls,
                               collapse_1L_lgl = T){
  mdl_types_chr <- results_ls$ttu_lngl_ls$best_mdls_tb$model_type
  if(collapse_1L_lgl){
    mdl_types_chr <- mdl_types_chr %>%
      paste0(collapse = ", ") %>%
      stringi::stri_replace_last(fixed = ",", " and")
  }
  return(mdl_types_chr)
}
get_link_from_tfmn <- function(tfmn_1L_chr,
                               is_OLS_1L_lgl = F){
  link_1L_chr <- ifelse(is_OLS_1L_lgl,
                        "identity",
                        ifelse(tfmn_1L_chr == "LOG",
                               "log",
                               ifelse(tfmn_1L_chr == "LGT",
                                      "logit",
                                      ifelse(tfmn_1L_chr == "CLL",
                                             "cloglog",
                                             ifelse(tfmn_1L_chr == "LOGLOG",
                                                    "loglog",
                                                    ifelse(tfmn_1L_chr == "NTF",
                                                           "identity",
                                                           "ERROR"))))))
  if(link_1L_chr=="ERROR")
    stop("Link cannot be identified - incorrect transformation argument tfmn_1L_chr")
  return(link_1L_chr)
}
get_mdl_cmprsns <- function(results_ls,
                            describe_1L_lgl = T,
                            mixed_1L_lgl = F,
                            as_list_1L_lgl = F){
  if(as_list_1L_lgl){
    mdl_types_chr <- c("OLS", "GLM")[c("OLS", "GLM") %in%
      results_ls$tables_ls$tenf_sngl_predr_tb$Model]
    mdl_cmprsns_ls <- mdl_types_chr %>%
      purrr::map(~{
        if(.x == "OLS"){
          mdls_chr <- results_ls$tables_ls$tenf_sngl_predr_tb$Model[(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="OLS")+1):(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="GLM")-1)] %>% unique()
        }
        if(.x == "GLM"){
          mdls_chr <-results_ls$tables_ls$tenf_sngl_predr_tb$Model[(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="GLM")+1):length(results_ls$tables_ls$tenf_sngl_predr_tb$Model)] %>%
            unique()
        }
        mdls_chr
        }) %>%
      stats::setNames(mdl_types_chr)
    mdl_cmprsns_xx <- mdl_cmprsns_ls
  }else{
    mdl_cmprsns_1L_chr <- paste0(
      ifelse(!"OLS" %in%
               results_ls$tables_ls$tenf_sngl_predr_tb$Model,
             "",
             ifelse(describe_1L_lgl,
                    paste0("OLS regression models used ", results_ls$tables_ls$tenf_sngl_predr_tb$Model[(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="OLS")+1):(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="GLM")-1)] %>% unique() %>% purrr::map_chr(~ .x %>% stringi::stri_replace_last_fixed("(","(measured on a scale of ")) %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed(","," and") %>% tolower(),"."),
                    ifelse(mixed_1L_lgl,"linear mixed effect models (LMMs)","ordinary least squares (OLS) regression models"))

      ),
      ifelse(!describe_1L_lgl & length(intersect(c("OLS","GLM"),
                                                 results_ls$tables_ls$tenf_sngl_predr_tb$Model))==2," and ",ifelse(describe_1L_lgl," ","")),
      ifelse(!"GLM" %in%
               results_ls$tables_ls$tenf_sngl_predr_tb$Model,
             "",
             ifelse(describe_1L_lgl,
                    paste0("GLMs used ",
                           results_ls$tables_ls$tenf_sngl_predr_tb$Model[(which(results_ls$tables_ls$tenf_sngl_predr_tb$Model=="GLM")+1):length(results_ls$tables_ls$tenf_sngl_predr_tb$Model)] %>%
                             unique() %>%
                             purrr::map_chr(~ .x %>%
                                              stringi::stri_replace_last_fixed("(","(measured on a scale of ")) %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed(","," and") %>% tolower()),
                    ifelse(mixed_1L_lgl,"generalised linear mixed effect models (GLMMs)","generalised linear models (GLMs)"))
      ))
    mdl_cmprsns_xx <- mdl_cmprsns_1L_chr
  }
  return(mdl_cmprsns_xx)
}
get_mdls_with_signft_covars <- function(outp_smry_ls,
                                        params_ls_ls){
  signft_covars_chr <- outp_smry_ls$mdls_with_covars_smry_tb %>%
    get_signft_covars(covar_var_nms_chr = params_ls_ls$params_ls$candidate_covar_nms_chr) # (Maybe) Needs editing to account for dummy variables - Need to check.  Would then need to update make_results_ls_spine
  signft_vars_ls <- outp_smry_ls[["mdls_with_covars_smry_tb"]]$Significant %>%
    purrr::map(~strsplit(.x, " ")) %>% purrr::flatten()
  mdls_with_signft_covars_ls <- signft_covars_chr %>%
    purrr::map(~{
      covar_nm_1L_chr <- .x
      mdls_chr <- outp_smry_ls$mdls_with_covars_smry_tb %>%
        dplyr::filter(purrr::map_lgl(signft_vars_ls,
                                     ~ any(.x == covar_nm_1L_chr))) %>%
        dplyr::pull(variable)
      mdls_chr

    }) %>%
    stats::setNames(signft_covars_chr)
  return(mdls_with_signft_covars_ls)
}
get_mdl_type_from_nm <- function(mdl_nm_1L_chr,
                                 mdl_types_lup = NULL){
  if(is.null(mdl_types_lup))
    utils::data("mdl_types_lup", package = "specific", envir = environment())
  mdl_type_1L_chr <- (mdl_types_lup %>%
                        dplyr::pull(short_name_chr))[mdl_types_lup %>%
                                                       dplyr::pull(short_name_chr) %>%
                                                       purrr::map_lgl(~endsWith(mdl_nm_1L_chr,.x))]
  return(mdl_type_1L_chr)
}
get_nbr_of_predrs <- function(results_ls,
                              as_words_1L_lgl = T){
  nbr_of_predrs_xx <- results_ls$study_descs_ls$predr_ctgs_ls %>% purrr::map_int(~length(.x[.x %in% results_ls$candidate_predrs_chr])) %>% sum()
  if(as_words_1L_lgl)
    nbr_of_predrs_xx <- nbr_of_predrs_xx %>% xfun::numbers_to_words()
  return(nbr_of_predrs_xx)
}
get_nbr_of_predrs_by_ctg <- function(results_ls){
  multiple_1L_lgl <- length(get_predr_ctgs(results_ls,
                                           collapse_1L_lgl = F)>1)
  predrs_by_ctg_1L_chr <- results_ls$study_descs_ls$predr_ctgs_ls[names(results_ls$study_descs_ls$predr_ctgs_ls) %>%
                                                                    tolower() %>%
                                                                    purrr::map_lgl(~.x %in% get_predr_ctgs(results_ls,
                                                                                                           collapse_1L_lgl = F))] %>%
    purrr::map2_chr(get_predr_ctgs(results_ls,
                                   collapse_1L_lgl = F),
                    ~ paste0(.y,
                             ifelse(multiple_1L_lgl,
                                    paste0(" (",
                                           length(.x[.x %in% results_ls$candidate_predrs_chr]) %>%
                                             xfun::numbers_to_words(),
                                           " measure",
                                           ifelse(length(.x[.x %in%
                                                              results_ls$candidate_predrs_chr])>1,
                                                  "s",
                                                  ""),
                                           ")"),
                                    ""))) %>%
    paste0(collapse = ", ") %>%
    stringi::stri_replace_last_fixed(","," and") %>%
    tolower()
  return(predrs_by_ctg_1L_chr )
}
get_nbr_of_scndry_analyses <- function(results_ls,
                                       as_words_1L_lgl = T,
                                       capitalise_1L_lgl = T){
  nbr_of_scndry_analyses_1L_xx <- names(results_ls$mdl_ingredients_ls) %>%
    startsWith("secondary") %>% sum()
  if(as_words_1L_lgl){
    nbr_of_scndry_analyses_1L_xx <- nbr_of_scndry_analyses_1L_xx %>% xfun::numbers_to_words()
    if(capitalise_1L_lgl){
      nbr_of_scndry_analyses_1L_xx <- nbr_of_scndry_analyses_1L_xx %>% Hmisc::capitalize()
    }
  }
  return(nbr_of_scndry_analyses_1L_xx)
}
get_ordered_sngl_csnl_mdls <- function(results_ls,
                                       select_int = NULL,
                                       collapse_1L_lgl = F){
  ordered_sngl_csnl_mdls_chr <- results_ls$ttu_cs_ls$cs_mdls_predrs_seq_dscdng_chr
  if(!is.null(select_int)){
    ordered_sngl_csnl_mdls_chr <- ordered_sngl_csnl_mdls_chr[select_int]
  }
  if(collapse_1L_lgl){
    ordered_sngl_csnl_mdls_chr <- ordered_sngl_csnl_mdls_chr %>%
      paste0(collapse = ", ") %>%
      stringi::stri_replace_last(fixed = ",", " and")
  }
  return(ordered_sngl_csnl_mdls_chr)
}
get_popl_descvs <- function(results_ls){
  popl_descvs_1L_chr <- results_ls$tables_ls$participant_descs$variable %>%
    unique() %>%
    # tolower()  %>%
    paste0(collapse = ", ") %>%
    stringi::stri_replace_last_fixed(","," and")
  return(popl_descvs_1L_chr)
}
get_predrs_by_ctg <-  function(results_ls,
                               long_desc_1L_lgl = F,
                               transform_1L_lgl = F,
                               collapse_1L_lgl = F){
  predrs_by_ctg_ls <- results_ls$study_descs_ls$predr_ctgs_ls[names(results_ls$study_descs_ls$predr_ctgs_ls) %>%
                                                                tolower() %>%
                                                                purrr::map_lgl(~.x %in% get_predr_ctgs(results_ls,
                                                                                                       collapse_1L_lgl = F))] %>%
    purrr::map(  ~ .x[.x %in% results_ls$candidate_predrs_chr]) %>%
    stats::setNames(get_predr_ctgs(results_ls,
                                   collapse_1L_lgl = F))
  if(long_desc_1L_lgl){
    predrs_by_ctg_ls <- predrs_by_ctg_ls %>%
      purrr::map2(names(predrs_by_ctg_ls) %>% Hmisc::capitalize(),
                  ~{
                    predr_descs_1L_chr <- .x %>% purrr::map_chr(~         paste0(ready4::get_from_lup_obj(results_ls$mdl_ingredients_ls$dictionary_tb,
                                                                                                             match_value_xx = .x,
                                                                                                             match_var_nm_1L_chr = "var_nm_chr",
                                                                                                             target_var_nm_1L_chr = "var_desc_chr",
                                                                                                             evaluate_1L_lgl = F),
                                                                                 " (",
                                                                                 .x %>% transform_names(rename_lup = results_ls$var_nm_change_lup),
                                                                                 " - measured on a scale of ",
                                                                                 ready4::get_from_lup_obj(results_ls$mdl_ingredients_ls$predictors_lup,
                                                                                                             match_value_xx = .x,
                                                                                                             match_var_nm_1L_chr = "short_name_chr",
                                                                                                             target_var_nm_1L_chr = "min_val_dbl",
                                                                                                             evaluate_1L_lgl = F),
                                                                                 "-",
                                                                                 ready4::get_from_lup_obj(results_ls$mdl_ingredients_ls$predictors_lup,
                                                                                                             match_value_xx = .x,
                                                                                                             match_var_nm_1L_chr = "short_name_chr",
                                                                                                             target_var_nm_1L_chr = "max_val_dbl",
                                                                                                             evaluate_1L_lgl = F),
                                                                                 ")")
                    ) %>% paste0(collapse = ", ") %>%
                      stringi::stri_replace_last_fixed(","," and")
                    paste0(.y," was measured by ", predr_descs_1L_chr,".")

                  })
  }else{
    if(transform_1L_lgl){
      predrs_by_ctg_ls <- predrs_by_ctg_ls %>%
        purrr::map(~{
          purrr::map_chr(.x,
                         ~ transform_names(.x,
                                           rename_lup = results_ls$var_nm_change_lup))
        }) %>% purrr::flatten_chr()
      if(collapse_1L_lgl){
        predrs_by_ctg_ls <- predrs_by_ctg_ls %>%
          paste0(collapse = ", ") %>%
          stringi::stri_replace_last(fixed = ",", " and")
      }
    }
  }
  return(predrs_by_ctg_ls)
}
get_predr_ctgs <- function(results_ls,
                           collapse_1L_lgl = T){
  predr_ctgs_chr <- (results_ls$study_descs_ls$predr_ctgs_ls %>% names())[(results_ls$study_descs_ls$predr_ctgs_ls %>% purrr::map_int(~length(.x[.x %in% results_ls$candidate_predrs_chr]))) > 0] %>% tolower()
  if(collapse_1L_lgl){
    predr_ctgs_chr <- predr_ctgs_chr %>% paste0(collapse = ", ") %>% stringi::stri_replace_last_fixed(","," and")
  }

  return(predr_ctgs_chr)
}
get_prefd_mdl_predrs <- function(results_ls){
  predrs_1L_chr <- results_ls$predr_var_nms_chr %>%
    paste0(collapse = ", ") %>%
    stringi::stri_replace_last(fixed = ",", " and")
  return(predrs_1L_chr)
}
get_random_intercept <- function(mdls_smry_tb,
                                 mdl_nm_1L_chr,
                                 deterministic_1L_lgl = T){
  mdl_smry_tb <- mdls_smry_tb %>%
    dplyr::filter(Model == mdl_nm_1L_chr)
  sd_dbl <- c(mdl_smry_tb %>%
                ready4::get_from_lup_obj(match_value_xx = "SD (Intercept)",
                                            match_var_nm_1L_chr = "Parameter",
                                            target_var_nm_1L_chr = "Estimate",
                                            evaluate_1L_lgl = F),
              ifelse(deterministic_1L_lgl,
                     0,
                     mdl_smry_tb %>%
                       ready4::get_from_lup_obj(match_value_xx = "SD (Intercept)",
                                                   match_var_nm_1L_chr = "Parameter",
                                                   target_var_nm_1L_chr = "SE",
                                                   evaluate_1L_lgl = F)))
  return(sd_dbl)
}
get_scndry_anlys_descs <- function(results_ls){
  nbr_of_scndry_analyses_1L_int <- get_nbr_of_scndry_analyses(results_ls, as_words_1L_lgl = F)
  if(nbr_of_scndry_analyses_1L_int > 0){
    scndry_anlys_descs_chr <- 1:nbr_of_scndry_analyses_1L_int %>%
      purrr::map_chr(~{
        secondary_ls <- results_ls$mdl_ingredients_ls %>% purrr::pluck(paste0("secondary_",.x))
        mdls_lup <- secondary_ls$mdls_lup
        predictors_chr <- mdls_lup$predrs_ls %>%
          unique() %>%
          purrr::map_chr(~{
            .x %>%
              purrr::map_chr(~ready4::get_from_lup_obj(secondary_ls$dictionary_tb %>% ready4use::remove_labels_from_ds(),
                                                          match_value_xx =.x,
                                                          match_var_nm_1L_chr = "var_nm_chr",
                                                          target_var_nm_1L_chr = "var_desc_chr",
                                                          evaluate_1L_lgl = F)) %>%
              paste0(collapse = ", ") %>%
              stringi::stri_replace_last_fixed(","," and")

          })
        paste0(ifelse(nbr_of_scndry_analyses_1L_int ==1,
                      "The secondary analysis used ",
                      paste0("Secondary Analysis ",LETTERS[.x], " used ")),
               ifelse(length(predictors_chr)==1,
                      paste0(predictors_chr, " as a predictor."),
                      paste0(predictors_chr, " as predictors.")))
      })
  }
  return(scndry_anlys_descs_chr)
}
get_selected_mixed_mdls <- function(results_ls,
                                    collapse_1L_lgl = T){
  mixed_mdls_xx <- results_ls$ttu_lngl_ls$best_mdls_tb %>% purrr::pmap_chr(~paste0(..1," (",..2,")"))
  if(collapse_1L_lgl){
    mixed_mdls_xx <- mixed_mdls_xx  %>%
      paste0(collapse = ", ") %>%
      stringi::stri_replace_last(fixed = ",", " and")
  }
  return(mixed_mdls_xx)
}
get_signft_covars <- function (mdls_with_covars_smry_tb, covar_var_nms_chr, what_1L_chr = "any", X_Ready4useDyad = NULL)
{

  signif_vars_chr <- mdls_with_covars_smry_tb$Significant %>%
    purrr::map(~strsplit(.x, " ")) %>% purrr::flatten() %>%
    purrr::flatten_chr() %>% unique()
  signt_covars_chr <- covar_var_nms_chr[covar_var_nms_chr %in%
                                          signif_vars_chr]
  if(what_1L_chr == "all"){
    signt_covars_chr <- signt_covars_chr[signt_covars_chr %>% purrr::map_lgl(~sum((mdls_with_covars_smry_tb$Significant %>%
                                                                                     purrr::map(~strsplit(.x, " ")) %>% purrr::flatten() %>%
                                                                                     purrr::flatten_chr()) ==.x)==length(signt_covars_chr))]
  }
  if(!is.null(X_Ready4useDyad)){
    dummys_chr <- manufacture(X_Ready4useDyad, flatten_1L_lgl = T, type_1L_chr = "dummys", what_1L_chr = "factors")
    signt_dumys_ls <- mdls_with_covars_smry_tb$Significant %>%
      purrr::map(~{
        terms_1L_chr <- .x
        dummys_chr[dummys_chr %>%
                     purrr::map_lgl(~stringr::str_detect(terms_1L_chr,.x))]
      })
    signt_dumys_chr <- signt_dumys_ls %>% purrr::flatten_chr() %>% unique()
    if(what_1L_chr == "all" && !identical(signt_dumys_chr, character(0))){
      signt_dumys_chr  <- signt_dumys_chr[signt_dumys_chr %>% purrr::map_lgl(~sum((signt_dumys_ls %>% purrr::flatten_chr())==.x)==length(signt_dumys_chr))]
    }
    signt_fctrs_chr <- signt_dumys_chr  %>%
      purrr::map_chr(~manufacture(X_Ready4useDyad, flatten_1L_lgl = T, type_1L_chr = "dummys", what_1L_chr = "factors-d", match_1L_chr = .x)) %>%
      unique()
    signt_covars_chr <- c(signt_covars_chr, signt_fctrs_chr) %>% sort()
  }
  if(identical(signt_covars_chr, character(0))){
    signt_covars_chr <- NA_character_
  }
  return(signt_covars_chr)
}
get_table_predn_mdl <- function(mdl_nm_1L_chr,
                                ingredients_ls,
                                analysis_1L_chr = NULL){
  mdl_type_1L_chr <- get_mdl_type_from_nm(mdl_nm_1L_chr,
                                          mdl_types_lup = ingredients_ls$mdl_types_lup)
  tfmn_1L_chr <- ready4::get_from_lup_obj(ingredients_ls$mdl_types_lup,
                                          match_value_xx = mdl_type_1L_chr,
                                          match_var_nm_1L_chr = "short_name_chr",
                                          target_var_nm_1L_chr = "tfmn_chr",
                                          evaluate_1L_lgl = F)
  if(is.null(analysis_1L_chr)){
    fake_ds_tb <- ingredients_ls$fake_ds_tb
  }else{
    reference_1L_chr <- ifelse(analysis_1L_chr == "Primary Analysis",
                               "Primary",
                               paste0("secondary_",which(LETTERS == stringr::str_sub(analysis_1L_chr,start=-1))))
    fake_ds_tb <- ingredients_ls %>% purrr::pluck(reference_1L_chr) %>% purrr::pluck("fake_ds_tb")
  }
  fake_ds_tb <- fake_ds_tb %>%
    add_tfd_var_to_ds(depnt_var_nm_1L_chr = ingredients_ls$depnt_var_nm_1L_chr,
                      tfmn_1L_chr = tfmn_1L_chr)
  table_predn_mdl <- make_shareable_mdl(fake_ds_tb = fake_ds_tb,
                                        mdl_smry_tb = ingredients_ls$mdls_smry_tb %>% dplyr::filter(Model == mdl_nm_1L_chr),
                                        x_ready4use_dictionary = ingredients_ls$dictionary_tb,
                                        depnt_var_nm_1L_chr = ingredients_ls$depnt_var_nm_1L_chr,
                                        id_var_nm_1L_chr = ingredients_ls$id_var_nm_1L_chr,
                                        tfmn_1L_chr = tfmn_1L_chr,
                                        mdl_type_1L_chr = mdl_type_1L_chr,
                                        mdl_types_lup = ingredients_ls$mdl_types_lup,
                                        control_1L_chr = ready4::get_from_lup_obj(ingredients_ls$mdl_types_lup,
                                                                                  match_value_xx = mdl_type_1L_chr,
                                                                                  match_var_nm_1L_chr = "short_name_chr",
                                                                                  target_var_nm_1L_chr = "control_chr",
                                                                                  evaluate_1L_lgl = F),
                                        start_1L_chr = NA_character_,
                                        seed_1L_int = ingredients_ls$seed_1L_int)
  return(table_predn_mdl)
}
