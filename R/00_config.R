######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 00_config.R  -  SINGLE SOURCE OF TRUTH for paths, seed and conventions.
## Every analysis script sources this first:  source("R/00_config.R")
## Run everything from the REPOSITORY ROOT.
######################################################################

## ---- Reproducibility --------------------------------------------------
SEED <- 2024
set.seed(SEED)

## ---- Paths (all relative to the repo root) ----------------------------
DATA_RAW_DIR   <- file.path("data", "Clean_data_csv")   # cleaned data (INPUT, approved)
DATA_RDATA_DIR <- file.path("data", "Clean_data_RData")
DERIVED_DIR    <- file.path("data", "derived")          # analysis-ready + imputed (OUTPUT of 10_impute)
OUT_DIR        <- "outputs"                             # one subfolder per script
FIG_DIR        <- "figures"
TAB_DIR        <- "tables"

## Main analysis dataset (cleaned, pre-imputation) produced by the approved cleaning script.
ANALYSIS_CSV <- file.path(DATA_RAW_DIR, "final_df_cleaned.csv")

## Helper: get (and create) a script's output folder, e.g. out_dir("30_lasso")
out_dir <- function(step) {
  d <- file.path(OUT_DIR, step)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

## ---- Outcome ----------------------------------------------------------
## estado_final: "Definitive cure" (169) vs "Therapeutic failure" (31, 15.5%).
## POSITIVE / event class = FAILURE (the clinically important, minority class).
OUTCOME_VAR   <- "estado_final"
OUTCOME_EVENT <- "Therapeutic failure"
OUTCOME_REF   <- "Definitive cure"
# Convenience: 1 = failure, 0 = cure
make_outcome <- function(x) factor(ifelse(x == OUTCOME_EVENT, "failure", "cure"),
                                   levels = c("cure", "failure"))

## ---- Validation strategy (shared across model scripts) ----------------
## Few events (31) -> primary internal validation = repeated stratified CV
## + bootstrap optimism correction; plus a stratified 70/30 hold-out as a check.
## Class imbalance is handled INSIDE training folds only (never on held-out data):
##   split first  ->  balance TRAIN only  ->  evaluate on untouched real-distribution set.
CV_FOLDS    <- 10
CV_REPEATS  <- 20
BOOT_REPS   <- 500
SPLIT_PROP  <- 0.70   # train fraction for the hold-out check

## ---- Failure-focused metrics (cures must NOT dominate) ----------------
## Report: PR-AUC (average precision), recall/sensitivity for FAILURE, specificity,
## PPV/NPV, F2 (weights recall), ROC-AUC, calibration (slope/intercept), Brier.
PRIMARY_METRIC <- "PR_AUC"

message("[config] loaded | seed=", SEED,
        " | outcome=", OUTCOME_VAR, " (event=", OUTCOME_EVENT, ")")
