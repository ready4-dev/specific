#' 
#' Depict features of an instance of a class by generating a plot
#' @name depict-SpecificProject
#' @description depict method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param mdl_idcs_int Model idcs (an integer vector), Default: NULL
#' @param output_type_1L_chr Output type (a character vector of length one), Default: 'HTML'
#' @param plt_indcs_int Plot indcs (an integer vector), Default: NULL
#' @return NULL
#' @rdname depict-methods
#' @aliases depict,SpecificProject-method
#' @export 
#' @importFrom purrr map_lgl map map_int discard
#' @importFrom knitr include_graphics
#' @importFrom ready4 depict
methods::setMethod("depict", "SpecificProject", function (x, mdl_idcs_int = NULL, output_type_1L_chr = "HTML", 
    plt_indcs_int = NULL) 
{
    if (is.null(mdl_idcs_int)) {
        mdl_idcs_int <- 1
    }
    if (is.null(plt_indcs_int)) 
        plt_indcs_int <- 1:5
    predr_var_nm_1L_chr <- x@c_SpecificResults@a_SpecificShareable@shareable_outp_ls$mdl_smry_ls$predr_var_nm_1L_chr
    plt_paths_chr <- list.files(x@paths_chr, recursive = T)[list.files(x@paths_chr, 
        recursive = T) %>% purrr::map_lgl(~endsWith(.x, ".png"))]
    plt_paths_ls <- x@b_SpecificParameters@candidate_mdls_lup$short_name_chr[mdl_idcs_int] %>% 
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
