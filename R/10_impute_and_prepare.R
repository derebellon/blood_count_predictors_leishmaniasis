######################################################################
## 10_impute_and_prepare.R
## Builds the analysis-ready modeling datasets AFTER the (approved) cleaning,
## applying LOGICAL constraints so imputed data make clinical sense, and runs
## multiple imputation (mice) separately for the PRE and POST scenarios.
##
## INPUT : data/Clean_data_csv/final_df_cleaned.csv  (approved cleaned data, n=200)
## OUTPUT: data/derived/analysis_base.rds      (modeling frame, pre-imputation)
##         data/derived/mids_pre.rds           (mice object, PRE, full cohort)
##         data/derived/mids_post.rds          (mice object, POST, post-hemogram subset)
##         data/derived/imputed_pre_stacked.rds / imputed_post_stacked.rds
##         outputs/10_imputation/*             (missingness + sanity reports)
##
## Design decisions (confirmed with D. Rebellón 2026-10-06):
##  - Dose: use treatment-agnostic `rango_dosis_medicamento` (complete, n=200);
##    drop dosis_glu/dosis_mil (structurally not-applicable by treatment).
##  - Species: NA -> "Not isolated/Unknown" (parasite not isolated), never imputed.
##  - Comorbidity: use clean binary `comorbilidades` (Yes/No); drop the
##    inconsistent `numero_comorbilidades`.
##  - POST model: PRIMARY = restrict to patients WITH a post hemogram (option b);
##    a full-cohort imputed POST set is also produced for SENSITIVITY (option c),
##    because the ~15% without a post hemogram is likely informatively missing.
##  - Derived features (percentages, ratios i_*, variation post-pre) are NOT
##    imputed directly; they are RE-COMPUTED from imputed base counts so they stay
##    internally consistent.
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({library(mice); library(dplyr)})

dir.create(out_dir("10_imputation"), showWarnings = FALSE, recursive = TRUE)
log_lines <- c()
logm <- function(...) { s <- paste0(...); message(s); log_lines <<- c(log_lines, s) }

## ---- 1. Load cleaned data --------------------------------------------
raw <- read.csv(ANALYSIS_CSV, stringsAsFactors = FALSE, check.names = TRUE)
logm("[10] loaded ", nrow(raw), " rows x ", ncol(raw), " cols")

## Outcome (1=failure event, 0=cure)
raw$outcome <- make_outcome(raw[[OUTCOME_VAR]])
logm("[10] outcome: ", paste(names(table(raw$outcome)), table(raw$outcome), sep="=", collapse=" | "))

## ---- 2. Structural recodes (make data make sense) --------------------
# Species: not isolated -> explicit category
raw$especie_corta[is.na(raw$especie_corta) | raw$especie_corta %in% c("", "NA")] <- "Not isolated/Unknown"
# Comorbidity: clean binary
raw$comorbilidades <- factor(raw$comorbilidades, levels = c("No", "Yes"))

## ---- 3. Feature sets --------------------------------------------------
# Baseline clinical (available PRE treatment). Near-constant vars dropped.
clinical_pre <- c("edad", "sexo", "etnia", "tiempo_evolucion_dicotomica",
                  "imc", "lesiones_categorica", "tipo_lesion_eval_base_corta",
                  "adenopatia_evalbas", "infeccion_concom_evalbas",
                  "comorbilidades", "especie_corta", "tratamiento",
                  "rango_dosis_medicamento")
# Base hemogram counts PRE (derived %/ratios recomputed later)
hemo_pre_base <- c("Leucocitos_pre","Neutrofilos_pre","Linfocitos_pre","Monocitos_pre",
                   "Eosinofilos_pre","Basofilos_pre","Granulocitos_pre",
                   "Globulos_rojos_pre","Hemoglobina_pre","Hematocrito_pre","Plaquetas_pre")
# End-of-treatment clinical (POST). Low-variance ones kept but flagged.
clinical_post <- c("variacion_lesion_post", "infeccion_concom_ftto")
# Base hemogram counts POST
hemo_post_base <- c("Leucocitos_post","Neutrofilos_post","Linfocitos_post","Monocitos_post",
                    "Eosinofilos_post","Basofilos_post","Granulocitos_post",
                    "Globulos_rojos_post","Hemoglobina_post","Hematocrito_post")

# Leakage / not-predictors to always exclude from models
leakage <- c("estado_final","estado_fin_tto","estado_sem8","estado_sem13","estado_sem26",
             "estado_va","dias_sem8","dias_sem13","dias_sem26","dias_va",
             "acude_sem8","acude_sem13","acude_sem26","acude_va","codigo_paciente",
             "dosis_glu_pres","dosis_mil_pres","numero_comorbilidades")

keep_cols <- unique(c("outcome", clinical_pre, hemo_pre_base, clinical_post, hemo_post_base,
                      "hemo_pos_realizado"))
keep_cols <- keep_cols[keep_cols %in% names(raw)]
base <- raw[, keep_cols]

## Coerce types: characters -> factors; keep numerics numeric
to_factor <- c("sexo","etnia","tiempo_evolucion_dicotomica","lesiones_categorica",
               "tipo_lesion_eval_base_corta","adenopatia_evalbas","infeccion_concom_evalbas",
               "especie_corta","tratamiento","rango_dosis_medicamento",
               "variacion_lesion_post","infeccion_concom_ftto")
for (v in intersect(to_factor, names(base))) base[[v]] <- factor(base[[v]])
num_vars <- intersect(c("edad","imc", hemo_pre_base, hemo_post_base), names(base))
for (v in num_vars) base[[v]] <- suppressWarnings(as.numeric(base[[v]]))

## Drop zero/near-zero variance predictors (cannot predict); report them
nzv <- sapply(setdiff(names(base), "outcome"), function(v){
  x <- base[[v]]; x <- x[!is.na(x)]
  if (length(unique(x)) <= 1) return(TRUE)
  tb <- sort(table(x), decreasing = TRUE)
  (tb[1] / sum(tb)) > 0.98          # >98% one value
})
nzv_vars <- names(nzv)[nzv]
if (length(nzv_vars)) logm("[10] dropped near-constant predictors (>98% one value): ",
                           paste(nzv_vars, collapse=", "))
base <- base[, setdiff(names(base), nzv_vars)]

saveRDS(base, file.path(DERIVED_DIR, "analysis_base.rds"))

## ---- 4. Missingness report (modeling features only) ------------------
miss <- sapply(base, function(x) sum(is.na(x)))
miss <- sort(miss[miss > 0], decreasing = TRUE)
logm("[10] features with missing values (of n=", nrow(base), "):")
for (v in names(miss)) logm("      ", v, ": ", miss[v], " (", round(100*miss[v]/nrow(base)), "%)")

has_post <- base$hemo_pos_realizado %in% c("1", 1)
logm("[10] patients WITH post hemogram: ", sum(has_post), " / ", nrow(base),
     " (POST model primary subset)")

## ---- 5. Multiple imputation --------------------------------------------
# PRE scenario: clinical_pre + hemo_pre_base (full cohort). Derived recomputed after.
pre_vars <- intersect(c("outcome", clinical_pre, hemo_pre_base), names(base))
dat_pre  <- base[, pre_vars]
logm("[10] PRE imputation on ", ncol(dat_pre), " vars, n=", nrow(dat_pre))
mids_pre <- mice(dat_pre, m = 20, maxit = 20, seed = SEED, printFlag = FALSE)
saveRDS(mids_pre, file.path(DERIVED_DIR, "mids_pre.rds"))

# POST scenario (primary): restrict to patients with a post hemogram.
post_vars <- intersect(c("outcome", clinical_pre, hemo_pre_base,
                         clinical_post, hemo_post_base), names(base))
dat_post <- base[has_post, post_vars]
logm("[10] POST imputation (primary subset) on ", ncol(dat_post), " vars, n=", nrow(dat_post))
mids_post <- mice(dat_post, m = 20, maxit = 20, seed = SEED, printFlag = FALSE)
saveRDS(mids_post, file.path(DERIVED_DIR, "mids_post.rds"))

## ---- 6. Recompute derived features from imputed base + sanity checks --
add_derived <- function(df) {
  g <- function(n) if (n %in% names(df)) df[[n]] else NA
  # neutrophil-lymphocyte & other key ratios (pre); extend as needed downstream
  df$ratio_neu_linfo_pre <- g("Neutrofilos_pre") / g("Linfocitos_pre")
  df$ratio_eos_linfo_pre <- g("Eosinofilos_pre") / g("Linfocitos_pre")
  df$ratio_mono_linfo_pre<- g("Monocitos_pre")   / g("Linfocitos_pre")
  if ("Neutrofilos_post" %in% names(df)) {
    df$ratio_neu_linfo_post <- g("Neutrofilos_post") / g("Linfocitos_post")
    df$var_leucocitos  <- g("Leucocitos_post")  - g("Leucocitos_pre")
    df$var_neutrofilos <- g("Neutrofilos_post") - g("Neutrofilos_pre")
    df$var_linfocitos  <- g("Linfocitos_post")  - g("Linfocitos_pre")
  }
  df
}

sanity <- function(df, label) {
  issues <- c()
  cnt <- intersect(c("Leucocitos_pre","Neutrofilos_pre","Plaquetas_pre","Hemoglobina_pre",
                     "Leucocitos_post"), names(df))
  for (v in cnt) if (any(df[[v]] < 0, na.rm=TRUE)) issues <- c(issues, paste0(v," has negative values"))
  if (any(is.na(df$outcome))) issues <- c(issues, "outcome has NA")
  logm("[10][sanity ", label, "] ", if(length(issues)) paste(issues, collapse="; ") else "OK (no negatives, outcome complete)")
}

comp_pre  <- complete(mids_pre,  "long", include = FALSE) |> add_derived()
comp_post <- complete(mids_post, "long", include = FALSE) |> add_derived()
sanity(comp_pre,  "PRE")
sanity(comp_post, "POST")
saveRDS(comp_pre,  file.path(DERIVED_DIR, "imputed_pre_stacked.rds"))
saveRDS(comp_post, file.path(DERIVED_DIR, "imputed_post_stacked.rds"))

writeLines(log_lines, file.path(out_dir("10_imputation"), "imputation_report.txt"))
logm("[10] DONE. Derived datasets + mice objects written to data/derived/.")
