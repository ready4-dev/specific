## ----message=FALSE, warning=FALSE---------------------------------------------
library(ready4)
library(ready4show)
library(ready4use)
library(scorz)
library(betareg)
library(specific)

## ----message=FALSE------------------------------------------------------------
X <- SpecificConverter(a_ScorzProfile = ready4use::Ready4useRepos(gh_repo_1L_chr = "ready4-dev/scorz", 
                                                                  gh_tag_1L_chr = "Documentation_0.0") %>%
                         ingest(fls_to_ingest_chr = "ymh_ScorzEuroQol5",
                                metadata_1L_lgl = F)) %>%
  metamorphose() 

## -----------------------------------------------------------------------------
class(X)

## ----echo = F, message=FALSE--------------------------------------------------
ds_tb <- X %>%
  procureSlot("a_YouthvarsProfile@a_Ready4useDyad@ds_tb") 

## ----inputds, echo=F, eval = knitr::is_html_output(), results='asis'----------
X %>%
  procureSlot("a_YouthvarsProfile@a_Ready4useDyad") %>%
  exhibit(display_1L_chr = "head")

## -----------------------------------------------------------------------------
procureSlot(X,
            "b_SpecificParameters@depnt_var_nm_1L_chr")

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@depnt_var_min_max_dbl",
               c(-1,1))

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@candidate_predrs_chr",
               new_val_xx = c("K10_int","Psych_well_int")) 

## -----------------------------------------------------------------------------
X <- renewSlot(X, 
               "b_SpecificParameters@predictors_lup", 
               short_name_chr = c("K10_int","Psych_well_int"),
               long_name_chr = c("Kessler Psychological Distress - 10 Item Total Score",
                                 "Overall Wellbeing Measure (Winefield et al. 2012)"),
               min_val_dbl = c(10,18),
               max_val_dbl = c(50,90),
               class_chr = "integer",
               increment_dbl = 1,
               class_fn_chr = "as.integer",
               mdl_scaling_dbl = 0.01,
               covariate_lgl = F)

## -----------------------------------------------------------------------------
exhibitSlot(X,
            "b_SpecificParameters@predictors_lup")

## -----------------------------------------------------------------------------
X <- renewSlot(X, 
               "b_SpecificParameters@candidate_covars_chr",
               new_val_xx = c("d_sex_birth_s", "d_age",  "d_sexual_ori_s", "d_studying_working"))

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@descv_var_nms_chr",
               c("d_age","Gender","d_relation_s",
                 "d_sexual_ori_s", "Region", "d_studying_working")) 

## -----------------------------------------------------------------------------
procureSlot(X,"a_YouthvarsProfile@timepoint_var_nm_1L_chr")

## -----------------------------------------------------------------------------
procureSlot(X,"a_YouthvarsProfile@timepoint_vals_chr")

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@msrmnt_date_var_nm_1L_chr",
               "data_collection_dtm")

## ----mdlslup, tab.cap='Model types lookup table', tab.id = 'mdlslup', results = "asis"----
exhibitSlot(X,
            "b_SpecificParameters@candidate_mdls_lup")

## ----warning=F----------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@candidate_mdls_lup",
               slice_idxs_int = c(1L,5L,7L,8L))

## -----------------------------------------------------------------------------
procureSlot(X,
            "b_SpecificParameters@folds_1L_int")

## -----------------------------------------------------------------------------
procureSlot(X,
            "b_SpecificParameters@max_mdl_runs_1L_int")

## -----------------------------------------------------------------------------
procureSlot(X,
            "b_SpecificParameters@seed_1L_int")

## -----------------------------------------------------------------------------
X <- ratify(X)

## ----warning = F--------------------------------------------------------------
X <- renewSlot(X,
               "paths_chr",
               tempdir())

## -----------------------------------------------------------------------------
X <- renewSlot(X,
               "b_SpecificParameters@fake_1L_lgl",
               T)

## -----------------------------------------------------------------------------
X <- author(X,
            what_1L_chr = "workspace")

## ---- results='hide', message=FALSE, warning=FALSE, fig.show='hide'-----------
X <- author(X,
            what_1L_chr = "descriptives",
            digits_1L_int = 3L)


