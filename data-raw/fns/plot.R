plot_auto_lm <- function (mdl, which_dbl = 1:6, ncol_1L_int = 3L, label_size_1L_int = 3)
{
  #pacman::p_load(char = "ggfortify")
    plt <- ggplot2::autoplot(mdl, which = which_dbl, ncol = ncol_1L_int,
        label.size = label_size_1L_int)
    if (6 %in% which_dbl)
        plt[which(which_dbl == 6)] <- plt[which(which_dbl ==
            6)] + ggtitle("Cook's vs Leverage")
    plt
}
plot_lnr_cmprsn <- function (data_tb,
                             predn_ds_tb,
                             predr_var_nm_1L_chr,
                             predr_var_desc_1L_chr,
                             depnt_var_nm_1L_chr = "utl_total_w",
                             depnt_var_desc_1L_chr = "Total weighted utility score" # Remove default
                             )
{
    data_tb <- data_tb %>% dplyr::filter(!is.na(!!rlang::sym(predr_var_nm_1L_chr)))
    ggplot2::ggplot(data_tb, ggplot2::aes(x = !!rlang::sym(predr_var_nm_1L_chr),
        y = !!rlang::sym(depnt_var_nm_1L_chr))) + ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "loess", size = 1.5) +
        ggplot2::geom_line(data = predn_ds_tb, ggplot2::aes(x = !!rlang::sym(predr_var_nm_1L_chr),
            y = !!rlang::sym(depnt_var_nm_1L_chr)), col = "red") +
        ggplot2::theme_bw() + ggplot2::labs(x = predr_var_desc_1L_chr,
        y = depnt_var_desc_1L_chr)
}
plot_obsd_predd_dnst <- function (tfd_data_tb,
                                  depnt_var_nm_1L_chr = "utl_total_w", # Remove default
                                  depnt_var_desc_1L_chr = "Total weighted utility score",# Remove default
    predd_val_var_nm_1L_chr = "Predicted", base_size_1L_dbl = 11, cmprsn_predd_var_nm_1L_chr = NA_character_, new_var_nm_1L_chr = NA_character_,x_lbl_1L_chr = NA_character_)
{
    if(is.na(cmprsn_predd_var_nm_1L_chr))
      cmprsn_predd_var_nm_1L_chr <- NULL
    if(is.na(x_lbl_1L_chr))
      x_lbl_1L_chr <- depnt_var_desc_1L_chr
    if(!is.na(new_var_nm_1L_chr)){
      tfd_data_tb <- tfd_data_tb %>%
        dplyr::rename(!!rlang::sym(new_var_nm_1L_chr) := !!rlang::sym(predd_val_var_nm_1L_chr))
      predd_val_var_nm_1L_chr <- new_var_nm_1L_chr
    }
  args_ls <- list(predd_val_var_nm_1L_chr,
                    cmprsn_predd_var_nm_1L_chr)
    tfd_data_tb %>%
      dplyr::mutate(Observed = !!rlang::sym(depnt_var_nm_1L_chr)) %>%
        tidyr::gather(variable, value,
                      !!!args_ls,
                      #!!rlang::sym(predd_val_var_nm_1L_chr),!!rlang::sym(cmprsn_predd_var_nm_1L_chr),
            Observed) %>% ggplot2::ggplot(ggplot2::aes(x = value,
        fill = variable)) + ggalt::geom_bkde(alpha = 0.5) + ggplot2::geom_rug() +
        viridis::scale_fill_viridis(discrete = TRUE) + ggplot2::theme_bw(base_size = base_size_1L_dbl) +
        ggplot2::theme(legend.position = "bottom") + ggplot2::labs(x = x_lbl_1L_chr,
        y = "Density", fill = "")
}
plot_obsd_predd_sctr_cmprsn <- function (tfd_data_tb, depnt_var_nm_1L_chr = "utl_total_w",# Remove defaults
                                         depnt_var_desc_1L_chr = "Total weighted utility score", # Remove defaults
                                         round_var_nm_1L_chr = "round",
                                         args_ls = NULL, base_size_1L_dbl = 11, correspondences_lup = NULL, predd_val_var_nm_1L_chr = "Predicted", x_lbl_1L_chr = NA_character_, y_lbl_1L_chr = NA_character_)
{
  if(is.na(x_lbl_1L_chr))
    x_lbl_1L_chr <- paste0("Observed ", depnt_var_desc_1L_chr)

  if(is.na(y_lbl_1L_chr))
    y_lbl_1L_chr <- paste0(predd_val_var_nm_1L_chr, " ", depnt_var_desc_1L_chr)
  if(!is.null(correspondences_lup) && !identical(round_var_nm_1L_chr, character(0)) && ifelse(identical(round_var_nm_1L_chr, character(0)),T,!is.na(round_var_nm_1L_chr)))
    tfd_data_tb <- tfd_data_tb %>%
      dplyr::mutate(!!rlang::sym(round_var_nm_1L_chr) := !!rlang::sym(round_var_nm_1L_chr) %>%
                      purrr::map_chr(~ready4::get_from_lup_obj(correspondences_lup,
                                                               match_value_xx = .x,
                                                               match_var_nm_1L_chr = "old_nms_chr",
                                                               target_var_nm_1L_chr = "new_nms_chr")))
  if(!identical(round_var_nm_1L_chr, character(0)) && ifelse(identical(round_var_nm_1L_chr, character(0)),T,!is.na(round_var_nm_1L_chr))){
    mapping_aes <- ggplot2::aes(x = !!rlang::sym(depnt_var_nm_1L_chr), y = !!rlang::sym(predd_val_var_nm_1L_chr),
                                col = !!rlang::sym(round_var_nm_1L_chr))
  }else{
    mapping_aes <- ggplot2::aes(x = !!rlang::sym(depnt_var_nm_1L_chr), y = !!rlang::sym(predd_val_var_nm_1L_chr))
  }
  ggplot2::ggplot(tfd_data_tb) +
    rlang::exec(ggplot2::geom_point,
                mapping_aes,
                size = 1,
                !!!args_ls) +
    ggplot2::theme_bw(base_size = base_size_1L_dbl) +
    ggplot2::xlim(0,1) +
    ggplot2::ylim(0, 1) +
    ggplot2::scale_color_manual(values = c("#D55E00", "#56B4E9")) +
    ggplot2::labs(x = x_lbl_1L_chr,
                  y = y_lbl_1L_chr,
                  col = "") +
    ggplot2::theme(legend.position = "bottom")
}
plot_sctr_plt_cmprsn <- function (tfd_data_tb,
                                  depnt_var_nm_1L_chr = "utl_total_w", # Remove defaults
                                  predd_val_var_nm_1L_chr = "Predicted")
{
  tfd_data_tb %>% ggplot2::ggplot(ggplot2::aes(x = !!rlang::sym(depnt_var_nm_1L_chr),
                                               y = !!rlang::sym(predd_val_var_nm_1L_chr))) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", size = 1.5) +
    ggplot2::theme_bw() + ggplot2::geom_abline(intercept = 0,
                                               slope = 1, color = "red", linetype = "dashed", size = 1.5) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
}
