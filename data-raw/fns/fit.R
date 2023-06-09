fit_ts_model_with_brm <- function (data_tb,
                                   depnt_var_nm_1L_chr,
                                   family_fn_1L_chr,
                                   id_var_nm_1L_chr,
                                   predr_vars_nms_chr,
                                   backend_1L_chr = getOption("brms.backend", "rstan"),
                                   control_ls = NULL,
                                   is_csnl_1L_lgl = F,
                                   iters_1L_int = 4000L, seed_1L_int = 1000L, prior_ls = NULL)
{
  mdl_ls <- brms::brm(formula = stats::as.formula(paste0(depnt_var_nm_1L_chr,
                                                         " ~ ", purrr::map_chr(predr_vars_nms_chr, ~paste0(.x,
                                                                                                           ifelse(is_csnl_1L_lgl,
                                                                                                                  ifelse(paste0(.x,"_scaled") %in% names(data_tb),
                                                                                                                         "_scaled + ",
                                                                                                                         " + "),
                                                                                                                  "_baseline + "),
                                                                                                           ifelse(is_csnl_1L_lgl,"",paste0(.x, "_change + "))
                                                         )) %>% paste0(collapse = ""),
                                                         "(1|", id_var_nm_1L_chr, ")")), backend = backend_1L_chr,
                      data = data_tb, family = eval(parse(text = family_fn_1L_chr)),
                      iter = iters_1L_int, seed = seed_1L_int, prior = prior_ls, control = control_ls)
  return(mdl_ls)
}
