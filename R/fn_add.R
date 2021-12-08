#' Add preferred predictor variable to model summary list
#' @description add_prefd_predr_var_to_mdl_smry_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add preferred predictor variable to model summary list. Function argument mdl_smry_ls specifies the object to be updated. The function returns Model summary (a list).
#' @param mdl_smry_ls Model summary (a list)
#' @param ds_smry_ls Dataset summary (a list)
#' @return Model summary (a list)
#' @rdname add_prefd_predr_var_to_mdl_smry_ls
#' @export 
#' @importFrom ready4 get_from_lup_obj
#' @keywords internal
add_prefd_predr_var_to_mdl_smry_ls <- function (mdl_smry_ls, ds_smry_ls) 
{
    mdl_smry_ls$predr_var_nm_1L_chr <- ds_smry_ls$candidate_predrs_chr[1]
    mdl_smry_ls$predr_var_desc_1L_chr <- ds_smry_ls$predictors_lup %>% 
        ready4::get_from_lup_obj(match_value_xx = mdl_smry_ls$predr_var_nm_1L_chr, 
            match_var_nm_1L_chr = "short_name_chr", target_var_nm_1L_chr = "long_name_chr", 
            evaluate_1L_lgl = F)
    mdl_smry_ls$predr_vals_dbl <- make_predr_vals(mdl_smry_ls$predr_var_nm_1L_chr, 
        candidate_predrs_lup = ds_smry_ls$predictors_lup)
    return(mdl_smry_ls)
}
#' Add transformed variable to dataset
#' @description add_tfd_var_to_ds() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add transformed variable to dataset. Function argument data_tb specifies the object to be updated. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param depnt_var_nm_1L_chr Dependent variable name (a character vector of length one)
#' @param tfmn_1L_chr Transformation (a character vector of length one)
#' @param depnt_var_max_val_1L_dbl Dependent variable maximum value (a double vector of length one), Default: NULL
#' @return Data (a tibble)
#' @rdname add_tfd_var_to_ds
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_tfd_var_to_ds <- function (data_tb, depnt_var_nm_1L_chr, tfmn_1L_chr, depnt_var_max_val_1L_dbl = NULL) 
{
    data_tb <- data_tb %>% dplyr::mutate(`:=`(!!rlang::sym(transform_depnt_var_nm(depnt_var_nm_1L_chr, 
        tfmn_1L_chr = tfmn_1L_chr)), !!rlang::sym(depnt_var_nm_1L_chr) %>% 
        calculate_depnt_var_tfmn(tfmn_1L_chr = tfmn_1L_chr, tfmn_is_outp_1L_lgl = F, 
            depnt_var_max_val_1L_dbl = depnt_var_max_val_1L_dbl)))
    return(data_tb)
}
#' Add unique identifiers to tibbles list
#' @description add_uids_to_tbs_ls() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add unique identifiers to tibbles list. Function argument tbs_ls specifies the object to be updated. The function returns Tibbles (a list).
#' @param tbs_ls Tibbles (a list)
#' @param prefix_1L_chr Prefix (a character vector of length one)
#' @param id_var_nm_1L_chr Identity variable name (a character vector of length one), Default: 'fkClientID'
#' @return Tibbles (a list)
#' @rdname add_uids_to_tbs_ls
#' @export 
#' @importFrom purrr map map_chr
#' @importFrom dplyr mutate arrange
#' @importFrom rlang sym
#' @importFrom tidyselect all_of
#' @importFrom stringr str_replace
#' @importFrom stats setNames
#' @keywords internal
add_uids_to_tbs_ls <- function (tbs_ls, prefix_1L_chr, id_var_nm_1L_chr = "fkClientID") 
{
    participant_ids <- paste0(prefix_1L_chr, 1:nrow(tbs_ls[[1]])) %>% 
        sample(nrow(tbs_ls[[1]]))
    tbs_ls <- purrr::map(tbs_ls, ~{
        .x %>% dplyr::mutate(`:=`(!!rlang::sym(id_var_nm_1L_chr), 
            tidyselect::all_of(participant_ids[1:nrow(.x)]))) %>% 
            dplyr::arrange(!!rlang::sym(id_var_nm_1L_chr) %>% 
                purrr::map_chr(~stringr::str_replace(.x, prefix_1L_chr, 
                  "")) %>% as.numeric())
    }) %>% stats::setNames(names(tbs_ls))
    return(tbs_ls)
}
