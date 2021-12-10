
setOldClass(c("specific_models","tbl_df", "tbl", "data.frame"))
#' Candidate models lookup table
#' @description Create a new valid instance of the Candidate models lookup table
#' @param x A prototype for the Candidate models lookup table, Default: make_pt_specific_models()
#' @return A validated instance of the Candidate models lookup table
#' @details Candidate models lookup table
#' @rdname specific_models
#' @export 
specific_models <- function(x = make_pt_specific_models()){ 
validate_specific_models(make_new_specific_models(x))
}
#' make new specific models Candidate models lookup table
#' @description Create a new unvalidated instance of the Candidate models lookup table
#' @param x A prototype for the Candidate models lookup table
#' @return An unvalidated instance of the Candidate models lookup table
#' @details Candidate models lookup table
#' @rdname make_new_specific_models
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_specific_models <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("specific_models",setdiff(make_pt_specific_models() %>% class(),class(x))),
class(x))
x
}
#' make prototype specific models Candidate models lookup table
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param control_chr Control (a character vector), Default: character(0)
#' @param family_chr Family (a character vector), Default: character(0)
#' @param fn_chr Function (a character vector), Default: character(0)
#' @param start_chr Start (a character vector), Default: character(0)
#' @param predn_type_chr Prediction type (a character vector), Default: character(0)
#' @param tfmn_chr Transformation (a character vector), Default: character(0)
#' @param tfmn_for_bnml_lgl Transformation for binomial (a logical vector), Default: logical(0)
#' @param fixed_acronym_chr Fixed acronym (a character vector), Default: character(0)
#' @param mixed_acronym_chr Mixed acronym (a character vector), Default: character(0)
#' @param mixed_type_chr Mixed type (a character vector), Default: character(0)
#' @param with_chr With (a character vector), Default: character(0)
#' @return A prototype for Candidate models lookup table
#' 
#' @rdname specific_models
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_specific_models <- function(short_name_chr = character(0),
long_name_chr = character(0),
control_chr = character(0),
family_chr = character(0),
fn_chr = character(0),
start_chr = character(0),
predn_type_chr = character(0),
tfmn_chr = character(0),
tfmn_for_bnml_lgl = logical(0),
fixed_acronym_chr = character(0),
mixed_acronym_chr = character(0),
mixed_type_chr = character(0),
with_chr = character(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr,
control_chr = control_chr,
family_chr = family_chr,
fn_chr = fn_chr,
start_chr = start_chr,
predn_type_chr = predn_type_chr,
tfmn_chr = tfmn_chr,
tfmn_for_bnml_lgl = tfmn_for_bnml_lgl,
fixed_acronym_chr = fixed_acronym_chr,
mixed_acronym_chr = mixed_acronym_chr,
mixed_type_chr = mixed_type_chr,
with_chr = with_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate specific models Candidate models lookup table
#' @description Validate an instance of the Candidate models lookup table
#' @param x An unvalidated instance of the Candidate models lookup table
#' @return A prototpe for Candidate models lookup table
#' @details Candidate models lookup table
#' @rdname validate_specific_models
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_specific_models <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_specific_models())],
names(make_pt_specific_models())))!=length(names(make_pt_specific_models()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_specific_models()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_specific_models() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_specific_models())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_specific_models() %>% 
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
#' is specific models Candidate models lookup table
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Candidate models lookup table
#' 
#' @rdname specific_models
#' @export 
is_specific_models <- function(x) inherits(validate_specific_models(x), "specific_models")
