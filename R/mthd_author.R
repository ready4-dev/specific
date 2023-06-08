#'
#' Author and save files
#' @name author-SpecificModels
#' @description author method applied to SpecificModels
#' @param x An object of class SpecificModels
#' @param prefd_mdl_types_chr Preferred model types (a character vector), Default: NULL
#' @param what_1L_chr What (a character vector of length one), Default: 'all'
#' @param digits_1L_int Digits (an integer vector of length one), Default: 3
#' @param reference_1L_int Reference (an integer vector of length one), Default: NULL
#' @return x (An object of class SpecificModels)
#' @rdname author-methods
#' @aliases author,SpecificModels-method
#' @export
#' @importFrom ready4show make_paths_ls write_all_outp_dirs
#' @importFrom rlang exec
#' @importFrom youthvars write_descv_tbls write_descv_plots
#' @importFrom hutils longest_prefix
#' @importFrom methods callNextMethod
#' @importFrom ready4 author
methods::setMethod("author", "SpecificModels", function (x, prefd_mdl_types_chr = NULL, what_1L_chr = "all",
    digits_1L_int = 3L, reference_1L_int = NULL)
{
    if (what_1L_chr %in% c("all", "descriptives", "models", "workspace")) {
        session_data_ls <- sessionInfo()
        if (what_1L_chr %in% c("workspace", "all")) {
            if (!is.null(reference_1L_int)) {
                transform_paths_ls <- list(fn = transform_paths_ls_for_scndry,
                  args_ls = list(reference_1L_int = reference_1L_int))
            }
            else {
                transform_paths_ls <- NULL
            }
            path_params_ls <- make_path_params_ls()
            path_params_ls$path_from_top_level_1L_chr <- x@paths_chr
            path_params_ls$use_fake_data_1L_lgl <- x@b_SpecificParameters@fake_1L_lgl
            paths_ls <- path_params_ls %>% ready4show::make_paths_ls(depth_1L_int = ifelse(is.null(transform_paths_ls),
                1, 2))
            if (!is.null(transform_paths_ls)) {
                paths_ls <- rlang::exec(transform_paths_ls$fn,
                  paths_ls, !!!transform_paths_ls$args_ls)
            }
            paths_ls <- ready4show::write_all_outp_dirs(paths_ls = paths_ls)
            x@b_SpecificParameters@paths_ls <- paths_ls
        }
        if (what_1L_chr %in% c("descriptives", "all")) {
            ds_descvs_ls <- manufacture(x, what_1L_chr = "ds_descvs_ls")
            descv_tbl_ls <- youthvars::write_descv_tbls(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                ds_descvs_ls = ds_descvs_ls, predictors_lup = x@b_SpecificParameters@predictors_lup,
                descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr,
                nbr_of_digits_1L_int = digits_1L_int, participation_var_1L_chr = x@a_YouthvarsProfile@participation_var_1L_chr)
            descv_plts_paths_ls <- youthvars::write_descv_plots(x@a_YouthvarsProfile@a_Ready4useDyad@ds_tb,
                ds_descvs_ls = ds_descvs_ls, descv_outp_dir_1L_chr = x@b_SpecificParameters@paths_ls$descv_outp_dir_1L_chr,
                lbl_nms_chr = x@b_SpecificParameters@itm_labels_chr,
                maui_domains_pfxs_1L_chr = hutils::longest_prefix(x@b_SpecificParameters@domain_labels_chr))
        }
        if (what_1L_chr %in% c("models", "all")) {
            x <- investigate(x)
            if (!is.null(prefd_mdl_types_chr))
                x <- renew(x, new_val_xx = prefd_mdl_types_chr,
                  type_1L_chr = "results", what_1L_chr = "prefd_mdls")
            x <- investigate(x)
            if (!is.null(prefd_covars_chr))
                x <- renew(x, new_val_xx = prefd_covars_chr,
                  type_1L_chr = "results", what_1L_chr = "prefd_covars")
            x <- investigate(x)
            x <- investigate(x)
            x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$session_data_ls <- session_data_ls
        }
    }
    else {
        methods::callNextMethod()
    }
    return(x)
})
#'
#' Author and save files
#' @name author-SpecificSynopsis
#' @description author method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param reference_1L_int Reference (an integer vector of length one), Default: NA
#' @param type_1L_chr Type (a character vector of length one), Default: 'Report'
#' @param what_1L_chr What (a character vector of length one), Default: 'Catalogue'
#' @return NULL
#' @rdname author-methods
#' @aliases author,SpecificSynopsis-method
#' @export
#' @importFrom purrr map2_chr
#' @importFrom ready4 author
methods::setMethod("author", "SpecificSynopsis", function (x, reference_1L_int = NA_integer_, type_1L_chr = "Report",
    what_1L_chr = "Catalogue")
{
    if (what_1L_chr == "Catalogue") {
        outp_smry_ls_ls <- manufacture(x@b_SpecificResults, what_1L_chr = "indexed_shareable")
        refs_int <- 1:length(outp_smry_ls_ls)
        if (!is.na(reference_1L_int)) {
            outp_smry_ls_ls <- outp_smry_ls_ls[reference_1L_int]
            refs_int <- reference_1L_int
        }
        ctlg_nms_chr <- purrr::map2_chr(outp_smry_ls_ls, refs_int,
            ~{
                fl_nm_1L_chr <- paste0("AAA_TTU_MDL_CTG", ifelse(.y ==
                  1, "", paste0("-", (.y - 1))))
                authorReport(x %>% renewSlot("b_SpecificResults@a_SpecificShareable@shareable_outp_ls",
                  .x), fl_nm_1L_chr = fl_nm_1L_chr, what_1L_chr = "Catalogue")
                fl_nm_1L_chr
            })
    }
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
#' @importFrom purrr walk
#' @importFrom ready4 author
methods::setMethod("author", "SpecificProject", function (x, fl_nm_1L_chr = "I_ALL_OUTPUT_", path_1L_chr = NA_character_,
    type_1L_chr = "results", what_1L_chr = "public")
{
    if (type_1L_chr %in% c("purge_all", "purge_write")) {
        outp_smry_ls_ls <- manufactureSlot(x, "c_SpecificResults",
            what_1L_chr = "indexed_shareable")
        purrr::walk(outp_smry_ls_ls, ~write_to_delete_mdl_fls(.x))
    }
    if (type_1L_chr != "purge_all") {
        path_1L_chr <- ifelse(is.na(path_1L_chr), x@paths_chr[1],
            path_1L_chr)
        if (type_1L_chr == "results") {
            if (what_1L_chr %in% c("public", "purge_write"))
                output_xx <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls
            if (what_1L_chr == "private")
                output_xx <- x@c_SpecificResults@b_SpecificPrivate@private_outp_ls
            if (what_1L_chr == "all")
                output_xx <- append(x@c_SpecificResults@b_SpecificPrivate@private_outp_ls,
                  x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls)
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
