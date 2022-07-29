reorder_cndt_predrs_chr <- function (candidate_predrs_chr, data_tb, depnt_var_nm_1L_chr = "utl_total_w",
    method_1L_chr = "pearson")
{
    data_mat <- as.matrix(data_tb %>% dplyr::select(c(dplyr::all_of(depnt_var_nm_1L_chr),
        dplyr::all_of(candidate_predrs_chr))))
    corr_ls <- Hmisc::rcorr(data_mat, type = method_1L_chr)
    reordered_cndt_predrs <- corr_ls$r %>% tibble::as_tibble(rownames = "var_nms_chr") %>%
        dplyr::mutate(dplyr::across(where(is.numeric), abs)) %>%
        dplyr::arrange(dplyr::desc(!!rlang::sym(depnt_var_nm_1L_chr))) %>%
        dplyr::filter(var_nms_chr != depnt_var_nm_1L_chr) %>% dplyr::pull(var_nms_chr) %>%
        as.vector()
    return(reordered_cndt_predrs)
}

