#' 
#' Depict (plot) features of a ready4 framework module (or sub-module)
#' @name depict-SpecificSynopsis
#' @description depict method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param axis_text_sclg_1L_dbl Axis text scaling (a double vector of length one), Default: 1.5
#' @param axis_title_sclg_1L_dbl Axis title scaling (a double vector of length one), Default: 2
#' @param base_height_1L_dbl Base height (a double vector of length one), Default: 13
#' @param base_size_1L_dbl Base size (a double vector of length one), Default: 30
#' @param depnt_var_desc_1L_chr Dependent variable description (a character vector of length one), Default: 'NA'
#' @param dim_plot_heights_int Dimension plot heights (an integer vector), Default: c(10L, 1L)
#' @param dim_plot_log_log_tfmn_1L_lgl Dimension plot log log transformation (a logical vector of length one), Default: F
#' @param dim_plot_rows_cols_pair_int Dimension plot rows columns pair (an integer vector), Default: c(3L, 2L)
#' @param labels_chr Labels (a character vector), Default: c("A", "B", "C", "D")
#' @param label_x_1L_dbl Label x (a double vector of length one), Default: 0.2
#' @param label_y_1L_dbl Label y (a double vector of length one), Default: 0.9
#' @param label_size_1L_dbl Label size (a double vector of length one), Default: 30
#' @param legend_sclg_1L_dbl Legend scaling (a double vector of length one), Default: 2
#' @param mdl_idxs_int Model indices (an integer vector), Default: 1:2
#' @param rel_heights_dbl Rel heights (a double vector), Default: c(4, 10, 1)
#' @param scale_dbl Scale (a double vector), Default: c(0.9, 0.9, 0.9)
#' @param timepoint_old_nms_chr Timepoint old names (a character vector), Default: 'NA'
#' @param timepoint_new_nms_chr Timepoint new names (a character vector), Default: 'NA'
#' @param use_png_fls_1L_lgl Use png files (a logical vector of length one), Default: F
#' @param utl_plot_label_1L_chr Utility plot label (a character vector of length one), Default: ' '
#' @param utl_by_rnd_plots_params_ls Utility by round plots parameters (a list), Default: list(width_1L_dbl = 6, height_1L_dbl = 4)
#' @param what_1L_chr What (a character vector of length one), Default: 'composite_mdl'
#' @param write_1L_lgl Write (a logical vector of length one), Default: F
#' @param y_label_1L_chr Y label (a character vector of length one), Default: ' '
#' @return Plot (a plot)
#' @rdname depict-methods
#' @aliases depict,SpecificSynopsis-method
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom rlang exec
#' @importFrom youthvars make_var_by_round_plt make_sub_tot_plts
#' @importFrom ready4 get_from_lup_obj depict
#' @importFrom cowplot get_legend plot_grid save_plot
#' @importFrom ggplot2 theme
methods::setMethod("depict", "SpecificSynopsis", function (x, axis_text_sclg_1L_dbl = 1.5, axis_title_sclg_1L_dbl = 2, 
    base_height_1L_dbl = 13, base_size_1L_dbl = 30, depnt_var_desc_1L_chr = NA_character_, 
    dim_plot_heights_int = c(10L, 1L), dim_plot_log_log_tfmn_1L_lgl = F, 
    dim_plot_rows_cols_pair_int = c(3L, 2L), labels_chr = c("A", 
        "B", "C", "D"), label_x_1L_dbl = 0.2, label_y_1L_dbl = 0.9, 
    label_size_1L_dbl = 30, legend_sclg_1L_dbl = 2, mdl_idxs_int = 1:2, 
    rel_heights_dbl = c(4, 10, 1), scale_dbl = c(0.9, 0.9, 0.9), 
    timepoint_old_nms_chr = NA_character_, timepoint_new_nms_chr = NA_character_, 
    use_png_fls_1L_lgl = F, utl_plot_label_1L_chr = " ", utl_by_rnd_plots_params_ls = list(width_1L_dbl = 6, 
        height_1L_dbl = 4), what_1L_chr = "composite_mdl", write_1L_lgl = F, 
    y_label_1L_chr = " ") 
{
    plt <- NULL
    outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls, 
        x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
    if (!is.na(timepoint_new_nms_chr[1])) {
        correspondences_lup <- ready4show::ready4show_correspondences() %>% 
            renew(old_nms_chr = timepoint_old_nms_chr, new_nms_chr = timepoint_new_nms_chr)
    }
    else {
        correspondences_lup <- NULL
    }
    if (what_1L_chr == "composite_mdl") {
        plt <- make_cmpst_sctr_and_dnst_plt(outp_smry_ls, base_size_1L_dbl = base_size_1L_dbl, 
            correspondences_lup = correspondences_lup, depnt_var_desc_1L_chr = depnt_var_desc_1L_chr, 
            labels_chr = labels_chr, label_x_1L_dbl = label_x_1L_dbl, 
            label_y_1L_dbl = label_y_1L_dbl, label_size_1L_dbl = label_size_1L_dbl, 
            mdl_idxs_int = mdl_idxs_int, use_png_fls_1L_lgl = use_png_fls_1L_lgl)
        write_path_1L_chr <- paste0(outp_smry_ls$path_to_write_to_1L_chr, 
            "/dens_and_sctr.png")
    }
    if (what_1L_chr == "composite_utl") {
        ds_descvs_ls <- manufacture(x, what_1L_chr = "ds_descvs_ls")
        outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls, 
            x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
        maui_domains_col_nms_chr <- x@c_SpecificParameters@domain_labels_chr
        first_plt <- rlang::exec(youthvars::make_var_by_round_plt, 
            !!!list(data_tb = outp_smry_ls$scored_data_tb, legend_sclg_1L_dbl = legend_sclg_1L_dbl, 
                var_nm_1L_chr = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
                x_label_1L_chr = ds_descvs_ls$dictionary_tb %>% 
                  ready4::get_from_lup_obj(match_value_xx = ds_descvs_ls$utl_wtd_var_nm_1L_chr, 
                    match_var_nm_1L_chr = "var_nm_chr", target_var_nm_1L_chr = "var_desc_chr", 
                    evaluate_1L_lgl = F) %>% as.vector(), label_fill_1L_chr = utl_plot_label_1L_chr, 
                axis_text_sclg_1L_dbl = axis_text_sclg_1L_dbl, 
                axis_title_sclg_1L_dbl = axis_title_sclg_1L_dbl, 
                y_label_1L_chr = y_label_1L_chr))
        second_plt <- rlang::exec(youthvars::make_sub_tot_plts, 
            !!!list(data_tb = outp_smry_ls$scored_data_tb, add_legend_1L_lgl = F, 
                axis_text_sclg_1L_dbl = axis_text_sclg_1L_dbl, 
                axis_title_sclg_1L_dbl = axis_title_sclg_1L_dbl, 
                col_nms_chr = maui_domains_col_nms_chr, legend_sclg_1L_dbl = legend_sclg_1L_dbl, 
                plot_rows_cols_pair_int = dim_plot_rows_cols_pair_int, 
                round_var_nm_1L_chr = ds_descvs_ls$round_var_nm_1L_chr, 
                heights_int = dim_plot_heights_int, make_log_log_tfmn_1L_lgl = dim_plot_log_log_tfmn_1L_lgl, 
                y_label_1L_chr = y_label_1L_chr))
        legend_ls <- cowplot::get_legend(first_plt)
        plt <- cowplot::plot_grid(first_plt + ggplot2::theme(legend.position = "none"), 
            second_plt, legend_ls, nrow = 3L, rel_heights = rel_heights_dbl, 
            scale = scale_dbl)
        write_path_1L_chr <- paste0(x@a_Ready4showPaths@outp_data_dir_1L_chr, 
            "/Output/_Descriptives/combined_utl.png")
    }
    if (write_1L_lgl) {
        cowplot::save_plot(write_path_1L_chr, plt, base_height = base_height_1L_dbl)
    }
    return(plt)
})
#' 
#' Depict (plot) features of a ready4 framework module (or sub-module)
#' @name depict-SpecificProject
#' @description depict method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param mdl_idxs_int Model indices (an integer vector), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param plt_indcs_int Plot indcs (an integer vector), Default: NULL
#' @return NULL
#' @rdname depict-methods
#' @aliases depict,SpecificProject-method
#' @export 
#' @importFrom purrr map_lgl map map_int discard
#' @importFrom knitr include_graphics
#' @importFrom ready4 depict
methods::setMethod("depict", "SpecificProject", function (x, mdl_idxs_int = NULL, output_type_1L_chr = "HTML", 
    plt_indcs_int = NULL) 
{
    if (is.null(mdl_idxs_int)) {
        mdl_idxs_int <- 1
    }
    if (is.null(plt_indcs_int)) 
        plt_indcs_int <- 1:5
    predr_var_nm_1L_chr <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$predr_var_nm_1L_chr
    plt_paths_chr <- list.files(x@paths_chr, recursive = T)[list.files(x@paths_chr, 
        recursive = T) %>% purrr::map_lgl(~endsWith(.x, ".png"))]
    plt_paths_ls <- x@b_SpecificParameters@candidate_mdls_lup$short_name_chr[mdl_idxs_int] %>% 
        purrr::map(~{
            pfx_1L_chr <- paste0("A_Candidate_Mdls_Cmprsn/A_RT_", 
                predr_var_nm_1L_chr, "_", .x)
            paths_chr <- plt_paths_chr[plt_paths_chr %>% purrr::map_lgl(~startsWith(.x, 
                pfx_1L_chr))]
            paths_chr <- paste0(x@paths_chr, "/", paths_chr)
            paths_chr[purrr::map_int(c("_LNR_CMPRSN", "_AUTOPLT", 
                "_PRED_DNSTY", "_SIM_DNSTY", "_PRED_SCTR")[plt_indcs_int], 
                ~{
                  idx_1L_int <- which(paths_chr %>% endsWith(paste0(.x, 
                    ".png")))
                  idx_1L_int <- ifelse(identical(idx_1L_int, 
                    integer(0)), NA_integer_, idx_1L_int)
                  idx_1L_int
                }) %>% purrr::discard(is.na)]
        })
    knitr::include_graphics({
        {
            plt_paths_ls[[1]]
        }
    })
})
