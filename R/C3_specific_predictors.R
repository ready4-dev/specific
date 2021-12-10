
setOldClass(c("specific_predictors","tbl_df", "tbl", "data.frame"))
#' Candidate predictors lookup table
#' @description Create a new valid instance of the Candidate predictors lookup table
#' @param x A prototype for the Candidate predictors lookup table, Default: make_pt_specific_predictors()
#' @return A validated instance of the Candidate predictors lookup table
#' @details Candidate predictors lookup table
#' @rdname specific_predictors
#' @export 
specific_predictors <- function(x = make_pt_specific_predictors()){ 
validate_specific_predictors(make_new_specific_predictors(x))
}
#' make new specific predictors Candidate predictors lookup table
#' @description Create a new unvalidated instance of the Candidate predictors lookup table
#' @param x A prototype for the Candidate predictors lookup table
#' @return An unvalidated instance of the Candidate predictors lookup table
#' @details Candidate predictors lookup table
#' @rdname make_new_specific_predictors
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_specific_predictors <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("specific_predictors",setdiff(make_pt_specific_predictors() %>% class(),class(x))),
class(x))
x
}
#' make prototype specific predictors Candidate predictors lookup table
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param min_val_dbl Minimum value (a double vector), Default: numeric(0)
#' @param max_val_dbl Maximum value (a double vector), Default: numeric(0)
#' @param class_chr Class (a character vector), Default: character(0)
#' @param increment_dbl Increment (a double vector), Default: numeric(0)
#' @param class_fn_chr Class function (a character vector), Default: character(0)
#' @param mdl_scaling_dbl Model scaling (a double vector), Default: numeric(0)
#' @param covariate_lgl Covariate (a logical vector), Default: logical(0)
#' @return A prototype for Candidate predictors lookup table
#' 
#' @rdname specific_predictors
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_specific_predictors <- function(short_name_chr = character(0),
long_name_chr = character(0),
min_val_dbl = numeric(0),
max_val_dbl = numeric(0),
class_chr = character(0),
increment_dbl = numeric(0),
class_fn_chr = character(0),
mdl_scaling_dbl = numeric(0),
covariate_lgl = logical(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr,
min_val_dbl = min_val_dbl,
max_val_dbl = max_val_dbl,
class_chr = class_chr,
increment_dbl = increment_dbl,
class_fn_chr = class_fn_chr,
mdl_scaling_dbl = mdl_scaling_dbl,
covariate_lgl = covariate_lgl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate specific predictors Candidate predictors lookup table
#' @description Validate an instance of the Candidate predictors lookup table
#' @param x An unvalidated instance of the Candidate predictors lookup table
#' @return A prototpe for Candidate predictors lookup table
#' @details Candidate predictors lookup table
#' @rdname validate_specific_predictors
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_specific_predictors <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_specific_predictors())],
names(make_pt_specific_predictors())))!=length(names(make_pt_specific_predictors()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_specific_predictors()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_specific_predictors() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_specific_predictors())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_specific_predictors() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' is specific predictors Candidate predictors lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Candidate predictors lookup table
#' 
#' @rdname specific_predictors
#' @export 
is_specific_predictors <- function(x) inherits(validate_specific_predictors(x), "specific_predictors")
