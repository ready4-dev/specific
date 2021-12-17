#' 
#' Share data contained in an instance of a class via an online repository
#' @name share-SpecificProject
#' @description share method applied to SpecificProject
#' @param x An object of class SpecificProject
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param repos_Ready4useRepos PARAM_DESCRIPTION
#' @return NULL
#' @rdname share-methods
#' @aliases share,SpecificProject-method
#' @export 
#' @importFrom ready4 share
methods::setMethod("share", "SpecificProject", function (x, fl_nm_1L_chr, repos_Ready4useRepos) 
{
    x@c_SpecificResults@b_SpecificPrivate <- SpecificPrivate()
    x@paths_chr <- NA_character_
    y <- share(repos_Ready4useRepos, obj_to_share_xx = x, fl_nm_1L_chr = fl_nm_1L_chr)
})
