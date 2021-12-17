share_SpecificProject <- function(x,
                                  fl_nm_1L_chr,
                                  repos_Ready4useRepos){
  x@c_SpecificResults@b_SpecificPrivate <- SpecificPrivate()
  x@paths_chr <- NA_character_
  y <- share(repos_Ready4useRepos,
             obj_to_share_xx = x,
             fl_nm_1L_chr = fl_nm_1L_chr)
}
