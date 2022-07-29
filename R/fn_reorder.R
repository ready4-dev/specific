#' Reorder candidate predictors character vector
#' @description reorder_cndt_predrs_chr() is a Reorder function that reorders an object to conform to a pre-specified schema. Specifically, this function implements an algorithm to reorder candidate predictors character vector. The function is called for its side effects and does not return a value.
#' @param candidate_predrs_chr Candidate predictors (a character vector)
#' @param data_tb Data (a tibble)
#' @param depnt_var_nm_1L_chr Dependent variable name (a character vector of length one), Default: 'utl_total_w'
#' @param method_1L_chr Method (a character vector of length one), Default: 'pearson'
#' @return Reordered candidate (predictors)
#' @rdname reorder_cndt_predrs_chr
#' @export 
#' @importFrom dplyr select all_of mutate across arrange desc filter pull
#' @importFrom Hmisc rcorr
#' @importFrom tibble as_tibble
#' @importFrom rlang sym
reorder_cndt_predrs_chr <- function (candidate_predrs_chr, data_tb, depnt_var_nm_1L_chr = "utl_total_w", 
    method_1L_chr = "pearson") 
{
    data_mat <- as.matrix(data_tb %>% dplyr::select(c(dplyr::all_of(depnt_var_nm_1L_chr), 
        dplyr::all_of(candidate_predrs_chr))))
    corr_ls <- Hmisc::rcorr(data_mat, type = method_1L_chr)
    reordered_cndt_predrs <- corr_ls$r %>% tibble::as_tibble(rownames = "var_nms_chr") %>% 
        dplyr::mutate(dplyr::across(where(is.numeric), abs)) %>% 
        dplyr::arrange(dplyr::desc(!!rlang::sym(depnt_var_nm_1L_chr))) %>% 
        dplyr::filter(var_nms_chr != depnt_var_nm_1L_chr) %>% 
        dplyr::pull(var_nms_chr) %>% as.vector()
    return(reordered_cndt_predrs)
}
