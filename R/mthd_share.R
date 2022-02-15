#' 
#' Share (publish) open data
#' @name share-SpecificProject
#' @description share method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'mdl_ingredients'
#' @param repos_Ready4useRepos PARAM_DESCRIPTION
#' @return NULL
#' @rdname share-methods
#' @aliases share,SpecificProject-method
#' @export 
#' @importFrom ready4 share
methods::setMethod("share", "SpecificProject", function (x, fl_nm_1L_chr = "mdl_ingredients", repos_Ready4useRepos) 
{
    x@c_SpecificResults@b_SpecificPrivate <- SpecificPrivate()
    x@paths_chr <- NA_character_
    y <- share(repos_Ready4useRepos, obj_to_share_xx = x, fl_nm_1L_chr = fl_nm_1L_chr)
})
#' 
#' Share (publish) open data
#' @name share-SpecificSynopsis
#' @description share method applied to SpecificSynopsis
#' @param x An object of class SpecificSynopsis
#' @param consolidate_1L_lgl Consolidate (a logical vector of length one), Default: T
#' @param fl_nm_1L_chr File name (a character vector of length one), Default: 'mdl_ingredients'
#' @param type_1L_chr Type (a character vector of length one), Default: 'Models'
#' @param what_1L_chr What (a character vector of length one), Default: 'ingredients'
#' @return NULL
#' @rdname share-methods
#' @aliases share,SpecificSynopsis-method
#' @export 
#' @importFrom purrr reduce map_chr
#' @importFrom dplyr bind_rows distinct mutate
#' @importFrom ready4 write_to_dv_with_wait share
#' @importFrom tibble tibble
methods::setMethod("share", "SpecificSynopsis", function (x, consolidate_1L_lgl = T, fl_nm_1L_chr = "mdl_ingredients", 
    type_1L_chr = "Models", what_1L_chr = "ingredients") 
{
    path_to_outp_dir_1L_chr <- x@a_Ready4showPaths@outp_data_dir_1L_chr
    if (type_1L_chr == "Models" & what_1L_chr %in% c("ingredients")) {
        secondary_1L_int <- x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls %>% 
            names() %>% startsWith("secondary_") %>% sum()
        object_xx <- paste0(path_to_outp_dir_1L_chr, "/Output/G_Shareable/Ingredients/mdl_ingredients.RDS") %>% 
            readRDS()
        if (secondary_1L_int > 0 & consolidate_1L_lgl) {
            object_xx <- 1:secondary_1L_int %>% purrr::reduce(.init = object_xx, 
                ~if (.y > 0) {
                  ingredients_ls <- paste0(path_to_outp_dir_1L_chr, 
                    "/secondary_", .y, "/G_Shareable/Ingredients/mdl_ingredients.RDS") %>% 
                    readRDS()
                  .x <- append(.x, list(ingredients_ls) %>% setNames(paste0("secondary_", 
                    .y)))
                  .x$dictionary_tb <- dplyr::bind_rows(.x$dictionary_tb, 
                    ingredients_ls$dictionary_tb) %>% dplyr::distinct()
                  .x$mdls_lup <- dplyr::bind_rows(.x$mdls_lup, 
                    ingredients_ls$mdls_lup %>% dplyr::mutate(source_chr = paste0("Secondary Analysis ", 
                      LETTERS[.y]))) %>% dplyr::distinct()
                  .x$mdls_smry_tb <- dplyr::bind_rows(.x$mdls_smry_tb, 
                    ingredients_ls$mdls_smry_tb) %>% dplyr::distinct()
                  .x$predictors_lup <- dplyr::bind_rows(.x$predictors_lup, 
                    ingredients_ls$predictors_lup) %>% dplyr::distinct()
                  .x
                }
                else {
                  .x$Primary <- .x
                  .x$mdls_lup <- .x$mdls_lup %>% dplyr::mutate(source_chr = "Primary Analysis")
                  .x
                })
            saveRDS(object_xx, paste0(path_to_outp_dir_1L_chr, 
                "/Output/G_Shareable/Ingredients/mdl_ingredients.RDS"))
        }
        Y <- share(x@e_Ready4useRepos, obj_to_share_xx = object_xx, 
            fl_nm_1L_chr = fl_nm_1L_chr)
    }
    if (type_1L_chr == "Report") {
        if (what_1L_chr == "Catalogue") {
            ctlg_path_1L_chr <- paste0(path_to_outp_dir_1L_chr, 
                "/", x@a_Ready4showPaths@reports_dir_1L_chr, 
                "/", what_1L_chr)
            outp_smry_ls_ls <- manufacture(x@b_SpecificResults, 
                what_1L_chr = "indexed_shareable")
            refs_int <- 1:length(outp_smry_ls_ls)
            ctlg_nms_chr <- purrr::map_chr(refs_int, ~paste0("AAA_TTU_MDL_CTG", 
                ifelse(.x == 1, "", paste0("-", (.x - 1)))))
            ready4::write_to_dv_with_wait(dss_tb = tibble::tibble(ds_obj_nm_chr = ctlg_nms_chr, 
                title_chr = purrr::map_chr(1:length(ctlg_nms_chr), 
                  ~paste0("Catalogue of utility mapping models", 
                    ifelse(.x == 1, " (Primary Analysis)", paste0(" (Supplementary Analysis ", 
                      (.x - 1), ")"))))), dv_nm_1L_chr = x@e_Ready4useRepos@dv_nm_1L_chr, 
                ds_url_1L_chr = x@e_Ready4useRepos@dv_ds_nm_1L_chr, 
                parent_dv_dir_1L_chr = paste0(x@b_SpecificResults@a_SpecificShareable@shareable_outp_ls$path_to_write_to_1L_chr, 
                  "/H_Dataverse"), paths_to_dirs_chr = paste0(x@a_Ready4showPaths@outp_data_dir_1L_chr, 
                  "/", x@a_Ready4showPaths@reports_dir_1L_chr, 
                  "/", what_1L_chr), inc_fl_types_chr = ".pdf", 
                paths_are_rltv_1L_lgl = F)
        }
    }
})
