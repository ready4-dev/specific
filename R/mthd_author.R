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
