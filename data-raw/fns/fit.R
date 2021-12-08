fit_ts_model_with_brm <- function (data_tb,# rename lngl ?
                                   depnt_var_nm_1L_chr, predr_vars_nms_chr, id_var_nm_1L_chr,
                                   backend_1L_chr = getOption("brms.backend", "rstan"), family_fn_1L_chr,
                                   #link_1L_chr = "identity",
                                   iters_1L_int = 4000L, seed_1L_int = 1000L, prior_ls = NULL, control_ls = NULL)
{
    mdl_ls <- brms::brm(formula = stats::as.formula(paste0(depnt_var_nm_1L_chr,
                                                           " ~ ", purrr::map_chr(predr_vars_nms_chr, ~paste0(.x,
                                                                                                             "_baseline + ", .x, "_change + ")) %>% paste0(collapse = ""),
                                                           "(1|", id_var_nm_1L_chr, ")")), backend = backend_1L_chr,
                        data = data_tb, family = eval(parse(text = family_fn_1L_chr)),
                        iter = iters_1L_int, seed = seed_1L_int, prior = prior_ls, control = control_ls)
    return(mdl_ls)
}
