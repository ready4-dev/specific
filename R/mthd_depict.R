#' 
#' Depict (plot) features of a ready4 framework module (or sub-module)
#' @name depict-SpecificSynopsis
#' @description depict method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param base_height PARAM_DESCRIPTION, Default: 13
#' @param base_size_1L_dbl Base size (a double vector of length one), Default: 30
#' @param depnt_var_desc_1L_chr Dependent variable description (a character vector of length one), Default: 'NA'
#' @param labels_chr Labels (a character vector), Default: c("A", "B", "C", "D")
#' @param label_x_1L_dbl Label x (a double vector of length one), Default: 0.2
#' @param label_y_1L_dbl Label y (a double vector of length one), Default: 0.9
#' @param label_size_1L_dbl Label size (a double vector of length one), Default: 30
#' @param mdl_idxs_int Model indices (an integer vector), Default: 1:2
#' @param timepoint_old_nms_chr Timepoint old names (a character vector), Default: 'NA'
#' @param timepoint_new_nms_chr Timepoint new names (a character vector), Default: 'NA'
#' @param use_png_fls_1L_lgl Use png files (a logical vector of length one), Default: F
#' @param what_1L_chr What (a character vector of length one), Default: 'composite_mdl'
#' @param write_1L_lgl Write (a logical vector of length one), Default: F
#' @return Plot (a plot)
#' @rdname depict-methods
#' @aliases depict,SpecificSynopsis-method
#' @export 
#' @importFrom ready4show ready4show_correspondences
#' @importFrom cowplot save_plot
#' @importFrom ready4 depict
methods::setMethod("depict", "SpecificSynopsis", function (x, base_height = 13, base_size_1L_dbl = 30, depnt_var_desc_1L_chr = NA_character_, 
    labels_chr = c("A", "B", "C", "D"), label_x_1L_dbl = 0.2, 
    label_y_1L_dbl = 0.9, label_size_1L_dbl = 30, mdl_idxs_int = 1:2, 
    timepoint_old_nms_chr = NA_character_, timepoint_new_nms_chr = NA_character_, 
    use_png_fls_1L_lgl = F, what_1L_chr = "composite_mdl", write_1L_lgl = F) 
{
    plt <- NULL
    outp_smry_ls <- append(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls, 
        x@b_SpecificResults@b_SpecificPrivate@private_outp_ls)
    if (!is.na(timepoint_new_nms_chr[1])) {
        correspondences_lup <- ready4show::ready4show_correspondences() %>% 
            renew(old_nms_chr = timepoint_old_nms_chr, new_nms_chr = timepoint_new_nms_chr)
    }
    else {
        correspondences_lup
    }
    if (what_1L_chr == "composite_mdl") {
        plt <- make_cmpst_sctr_and_dnst_plt(outp_smry_ls, base_size_1L_dbl = base_size_1L_dbl, 
            correspondences_lup = correspondences_lup, depnt_var_desc_1L_chr = depnt_var_desc_1L_chr, 
            labels_chr = labels_chr, label_x_1L_dbl = label_x_1L_dbl, 
            label_y_1L_dbl = label_y_1L_dbl, label_size_1L_dbl = label_size_1L_dbl, 
            mdl_idxs_int = mdl_idxs_int, use_png_fls_1L_lgl = use_png_fls_1L_lgl)
        if (write_1L_lgl) {
            cowplot::save_plot(paste0(outp_smry_ls$path_to_write_to_1L_chr, 
                "/dens_and_sctr.png"), plt, base_height = base_height)
        }
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
