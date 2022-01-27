#' 
#' Author and save files
#' @name author-SpecificModels
#' @description author method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param what_1L_chr What (a character vector of length one), Default: 'workspace'
#' @param digits_1L_int Digits (an integer vector of length one), Default: 3
#' @param reference_1L_int Reference (an integer vector of length one), Default: NULL
#' @return x (An object of class SpecificModels)
#' @rdname author-methods
#' @aliases author,SpecificModels-method
#' @export 
#' @importFrom ready4show make_paths_ls write_all_outp_dirs
#' @importFrom rlang exec
#' @importFrom youthvars write_descv_tbls write_descv_plots
#' @importFrom ready4 author
methods::setMethod("author", "SpecificModels", function (x, what_1L_chr = "workspace", digits_1L_int = 3L, reference_1L_int = NULL) 
{
    if (what_1L_chr == "workspace") {
        if (!is.null(reference_1L_int)) {
            transform_paths_ls <- list(fn = transform_paths_ls_for_scndry, 
                args_ls = list(reference_1L_int = reference_1L_int))
        }
        else {
            transform_paths_ls <- NULL
        }
        path_params_ls <- make_path_params_ls()
        path_params_ls$use_fake_data_1L_lgl <- x@b_SpecificParameters@fake_1L_lgl
        paths_ls <- path_params_ls %>% ready4show::make_paths_ls(depth_1L_int = ifelse(is.null(transform_paths_ls), 
            1, 2))
        if (!is.null(transform_paths_ls)) {
            paths_ls <- rlang::exec(transform_paths_ls$fn, paths_ls, 
                !!!transform_paths_ls$args_ls)
        }
        paths_ls <- ready4show::write_all_outp_dirs(paths_ls = paths_ls)
        x@b_SpecificParameters@paths_ls <- paths_ls
    }
    if (what_1L_chr == "descriptives") {
        ds_descvs_ls <- manufacture(x, what_1L_chr = "ds_descvs_ls")
        descv_tbl_ls <- youthvars::write_descv_tbls(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
            ds_descvs_ls = ds_descvs_ls, predictors_lup = x@b_SpecificParameters@predictors_lup, 
            descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr, 
            nbr_of_digits_1L_int = digits_1L_int)
        descv_plts_paths_ls <- youthvars::write_descv_plots(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb, 
            ds_descvs_ls = ds_descvs_ls, descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr, 
            lbl_nms_chr = x@b_SpecificParameters@itm_labels_chr, 
            maui_domains_pfxs_1L_chr = x@b_SpecificParameters@itm_prefix_1L_chr)
    }
    return(x)
})
#' 
#' Author and save files
#' @name author-SpecificProject
#' @description author method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'I_ALL_OUTPUT_'
#' @param path_1L_chr Path (a character vector of length one), Default: 'NA'
#' @param type_1L_chr Type (a character vector of length one), Default: 'results'
#' @param what_1L_chr What (a character vector of length one), Default: 'public'
#' @return NULL
#' @rdname author-methods
#' @aliases author,SpecificProject-method
#' @export 
#' @importFrom ready4 author
methods::setMethod("author", "SpecificProject", function (x, fl_nm_1L_chr = "I_ALL_OUTPUT_", path_1L_chr = NA_character_, 
    type_1L_chr = "results", what_1L_chr = "public") 
{
    if (type_1L_chr %in% c("purge_all", "purge_write")) {
        write_to_delete_mdl_fls(x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls)
    }
    if (type_1L_chr != "purge_all") {
        path_1L_chr <- ifelse(is.na(path_1L_chr), x@paths_chr[1], 
            path_1L_chr)
        if (type_1L_chr == "results") {
            if (what_1L_chr == "public") 
                output_xx <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls
            if (what_1L_chr == "private") 
                output_xx <- x@c_SpecificResults@b_SpecificPrivate@private_outp_ls
            if (what_1L_chr == "all") 
                output_xx <- x@c_SpecificResults
        }
        if (type_1L_chr == "parameters") 
            output_xx <- x@b_SpecificParameters
        if (type_1L_chr %in% c("project", "purge_write")) {
            if (what_1L_chr == "public") {
                output_xx <- x
                output_xx@a_YouthvarsProfile@a_Ready4useDyad@ds_tb <- output_xx@a_YouthvarsProfile@a_Ready4useDyad@ds_tb[0, 
                  ]
                output_xx@c_SpecificResults@b_SpecificPrivate <- SpecificPrivate()
                output_xx@paths_chr <- NA_character_
            }
        }
        saveRDS(output_xx, paste0(path_1L_chr, "/", fl_nm_1L_chr, 
            ".RDS"))
    }
})
