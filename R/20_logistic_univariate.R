######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 20_logistic_univariate.R
## Regresiones logisticas UNIVARIADAS (Odds Ratios) para el tamizaje de
## variables clinicas y del hemograma, como en la tesis (do-files 4, 5, 100),
## portado a R. Se corre sobre la imputacion multiple (mids) y se AGRUPA
## (pooling de Rubin). Tablas con gtsummary.
## INFERENCIA: aqui NO se usan las ratios floored X/IG (solo van en ML).
##
## INPUT : data/derived/mids_pre_full.rds , data/derived/mids_post_full.rds
## OUTPUT: outputs/20_logistic_univariate/*.html  (tablas OR)
##         outputs/20_logistic_univariate/univariate_OR.rds
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice)       ## imputacion multiple + pooling
  library(gtsummary)  ## tablas de resumen y regresion (favorito de David)
  library(dplyr)      ## manejo de datos
  library(gt)         ## exportar a HTML
})

paso <- "20_logistic_univariate"

## ---- Normality pre-check (Shapiro-Wilk) ------------------------------
## Rationale: the blood-count distributions are skewed, so group comparisons
## use non-parametric tests (Wilcoxon-Mann-Whitney). We document that choice
## by formally testing normality of the continuous variables (one completed
## imputation is adequate for this descriptive check).
dest_sw <- out_dir(paso)                                         # ensure this step's output folder exists
cont_vars <- c("edad", "Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre",   # continuous variables to test
               "Eosinofilos_pre", "Basofilos_pre", "Granulocitos_pre",
               "Globulos_rojos_pre", "Hemoglobina_pre", "Hematocrito_pre", "Plaquetas_pre")
d_sw <- mice::complete(readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds")), 1) # one completed dataset
cont_vars <- cont_vars[cont_vars %in% names(d_sw)]               # keep variables actually present
shapiro_tab <- do.call(rbind, lapply(cont_vars, function(v) {    # run Shapiro-Wilk per variable
  x <- d_sw[[v]]; x <- x[is.finite(x)]                           # drop non-finite values
  tt <- tryCatch(shapiro.test(x), error = function(e) NULL)      # guard degenerate columns
  if (is.null(tt)) return(NULL)                                  # skip if it cannot run
  data.frame(variable = v, W = round(unname(tt$statistic), 3),   # W statistic
             p_value = signif(tt$p.value, 3),                    # p-value
             normal = ifelse(tt$p.value > 0.05, "yes", "no"))    # p>0.05 => compatible with normality
}))
write.csv(shapiro_tab, file.path(dest_sw, "shapiro_continuous.csv"), row.names = FALSE)  # save normality table
message("[20] Shapiro-Wilk: ", sum(shapiro_tab$normal == "no"), "/", nrow(shapiro_tab),
        " continuous variables non-normal -> non-parametric (Wilcoxon) tests used")     # verdict to the log

## ---- Listas de predictores (INFERENCIA, sin ratios floored X/IG) -----
clinicas <- c("edad", "sexo", "etnia", "tiempo_evolucion_dicotomica", "imc",
              "lesiones_categorica", "tipo_lesion_eval_base_corta",
              "adenopatia_evalbas", "infeccion_concom_evalbas", "comorbilidades",
              "especie_corta", "tratamiento", "rango_dosis_medicamento")

hemo_pre <- c("Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre", "Eosinofilos_pre",
              "Basofilos_pre", "Granulocitos_pre", "Globulos_rojos_pre",
              "Hemoglobina_pre", "Hematocrito_pre", "Plaquetas_pre",
              "pct_neutro_pre", "pct_linfo_pre", "pct_mono_pre", "pct_eosino_pre",
              "pct_baso_pre",
              "i_neu_linfo_pre", "i_eos_linfo_pre", "i_mono_linfo_pre",
              "i_baso_linfo_pre", "i_granu_linfo_pre", "i_eos_mono_pre",
              "i_eos_neu_pre", "i_eos_baso_pre", "i_neu_mono_pre",
              "i_neu_baso_pre", "i_baso_mono_pre", "i_monoeosneu_lin_pre")

## ---- Estandarizacion de predictores CONTINUOS (OR por 1 DE) ----------
## Mejora sobre la tesis: los OR por unidad cruda son ininterpretables cuando la
## escala es muy grande/pequena (plaquetas OR=0.99, ratios OR=0.00). Estandarizo
## las continuas a z-score (media 0, DE 1) -> OR por cada aumento de 1 DE,
## comparable entre parametros. Las categoricas se dejan igual.
estandarizar_continuas <- function(mids_obj, predictores) {
  larga <- complete(mids_obj, "long", include = TRUE)
  continuas <- predictores[sapply(predictores, function(v)
    is.numeric(larga[[v]]) && length(unique(stats::na.omit(larga[[v]]))) > 5)]
  for (v in continuas) larga[[v]] <- as.numeric(scale(larga[[v]]))
  list(mids = as.mids(larga), continuas = continuas)
}

## ---- Funcion: tabla univariada OR (MI-pooled) para un set de predictores
## Ajusta un modelo logistico por predictor con with(mids, glm()), gtsummary
## agrupa (pool) y exponentia -> OR (IC95%) y valor p. Se apilan con tbl_stack.
## Las continuas entran estandarizadas -> OR por 1 DE.
tabla_univariada <- function(mids_obj, predictores, titulo) {
  est <- estandarizar_continuas(mids_obj, predictores)
  mids_obj <- est$mids
  tablas <- list()
  for (v in predictores) {
    fit <- with(mids_obj, glm(reformulate(v, response = "outcome"),
                              family = binomial(link = "logit")))
    tablas[[v]] <- tbl_regression(
      fit,
      exponentiate = TRUE,
      estimate_fun = function(x) style_ratio(x, digits = 2),
      pvalue_fun   = function(x) style_pvalue(x, digits = 3)
    )
  }
  tbl_stack(tablas) %>%
    modify_header(label = paste0("**", titulo, "**")) %>%
    modify_caption("Univariate logistic regression (OR, 95% CI) for therapeutic failure. Multiply-imputed, pooled.")
}

## Predictores POST (hemograma fin de tratamiento + variaciones post-pre).
## La tesis encontro relevantes eosinofilos% POST y el cambio de monocitos.
hemo_post <- c("Neutrofilos_post", "Linfocitos_post", "Monocitos_post", "Eosinofilos_post",
               "Basofilos_post", "Granulocitos_post", "Globulos_rojos_post",
               "Hemoglobina_post", "Hematocrito_post",
               "pct_neutro_post", "pct_linfo_post", "pct_mono_post", "pct_eosino_post",
               "pct_baso_post",
               "i_neu_linfo_post", "i_eos_linfo_post", "i_mono_linfo_post",
               "i_baso_linfo_post", "i_granu_linfo_post", "i_eos_mono_post",
               "i_eos_neu_post", "i_eos_baso_post", "i_neu_mono_post",
               "i_neu_baso_post", "i_baso_mono_post", "i_monoeosneu_lin_post")
variaciones <- c("var_Neutrofilos", "var_Linfocitos", "var_Monocitos", "var_Eosinofilos",
                 "var_Basofilos", "var_Granulocitos", "var_Globulos_rojos",
                 "var_Hemoglobina", "var_Hematocrito",
                 "var_i_neu_linfo", "var_i_mono_linfo", "var_i_granu_linfo")

## ---- Univariadas PRE -------------------------------------------------
mids_pre <- readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds"))
message("[20] univariadas clinicas (PRE)...")
uni_clin <- tabla_univariada(mids_pre, clinicas, "Clinical and sociodemographic variables")
message("[20] univariadas hemograma (PRE)...")
uni_hemo <- tabla_univariada(mids_pre, hemo_pre, "Blood count parameters (pre-treatment)")

## ---- Univariadas POST (fin de tratamiento + variacion) ---------------
mids_post <- readRDS(file.path(DERIVED_DIR, "mids_post_full.rds"))
message("[20] univariadas hemograma (POST)...")
uni_hemo_post <- tabla_univariada(mids_post, hemo_post, "Blood count parameters (end of treatment)")
message("[20] univariadas variacion (POST-PRE)...")
uni_var <- tabla_univariada(mids_post, variaciones, "Blood count variation (end of treatment - pre)")

## ---- Exportar --------------------------------------------------------
dest <- out_dir(paso)
gt::gtsave(as_gt(uni_clin),      file.path(dest, "univariate_OR_clinical.html"))
gt::gtsave(as_gt(uni_hemo),      file.path(dest, "univariate_OR_hemogram_pre.html"))
gt::gtsave(as_gt(uni_hemo_post), file.path(dest, "univariate_OR_hemogram_post.html"))
gt::gtsave(as_gt(uni_var),       file.path(dest, "univariate_OR_variation.html"))
saveRDS(list(clinical = uni_clin, hemogram_pre = uni_hemo,
             hemogram_post = uni_hemo_post, variation = uni_var),
        file.path(dest, "univariate_OR.rds"))

## ====================================================================
## Univariate RELATIVE RISKS (robust Poisson, MI-pooled) to accompany the
## ORs in Table 2. The thesis reported associations as RR (log-binomial);
## here we obtain the univariate RR with a robust (HC0) Poisson, which
## approximates log-binomial without convergence problems, pooled across
## imputations by Rubin's rules -- exactly the estimator used for the
## MULTIVARIABLE models (script 21). Continuous predictors are standardised
## to 1 SD (RR per 1-SD), matching the OR column. Logistic regression is
## thus used only for the OR screening and for prediction; every reported
## ASSOCIATION (uni- and multivariable) is a robust-Poisson RR.
## OUTPUT: outputs/20_logistic_univariate/univariate_RR_clinical.csv (+ hemogram CSVs)
## ====================================================================
suppressPackageStartupMessages(library(sandwich))               # robust (Huber-White) covariance for Poisson RR
rr_univ_mi <- function(mids_obj, predictores) {                 # per-predictor univariate RR, MI-pooled
  est <- estandarizar_continuas(mids_obj, predictores); mids_obj <- est$mids  # standardise continuous -> RR per 1-SD
  m <- mids_obj$m                                               # number of imputations
  out <- list()
  for (v in predictores) {                                      # one univariate model per predictor
    ests <- lapply(seq_len(m), function(i) {                    # fit on each imputed dataset
      d <- complete(mids_obj, i); d$fail01 <- as.integer(d$outcome == "failure")  # 0/1 failure outcome
      f <- suppressWarnings(glm(reformulate(v, response = "fail01"), data = d, family = poisson(link = "log")))  # Poisson-log
      list(b = coef(f), V = sandwich::vcovHC(f, type = "HC0"))  # robust coefficients + covariance
    })
    bmat <- sapply(ests, `[[`, "b")                             # k terms x m imputations
    if (is.null(dim(bmat))) bmat <- matrix(bmat, nrow = 1, dimnames = list(names(ests[[1]]$b), NULL))
    Qbar <- rowMeans(bmat)                                      # pooled point estimate (Rubin)
    Ubar <- Reduce(`+`, lapply(ests, `[[`, "V")) / m            # within-imputation variance
    B    <- if (m > 1) stats::cov(t(bmat)) else diag(0, length(Qbar))  # between-imputation variance
    Tvar <- diag(Ubar) + (1 + 1/m) * diag(B)                    # total variance
    SE   <- sqrt(Tvar); z <- Qbar / SE                          # pooled SE and Wald z
    df <- data.frame(variable = v, term = names(Qbar), RR = exp(Qbar),
                     lo = exp(Qbar - 1.96*SE), hi = exp(Qbar + 1.96*SE),
                     p = 2*pnorm(-abs(z)), row.names = NULL)
    out[[v]] <- df[df$term != "(Intercept)", ]                  # drop the intercept
  }
  res <- do.call(rbind, out)
  res$`RR (95% CI)` <- sprintf("%.2f (%.2f-%.2f)", res$RR, res$lo, res$hi)  # formatted RR
  res$`p-value`     <- ifelse(res$p < 0.001, "<0.001", sprintf("%.3f", res$p))
  res
}
rr_clin      <- rr_univ_mi(mids_pre,  clinicas)                 # Table 2 clinical RR
rr_hemo_pre  <- rr_univ_mi(mids_pre,  hemo_pre)                 # pre-Tx blood-count RR (for coherence / S-tables)
rr_hemo_post <- rr_univ_mi(mids_post, hemo_post)               # EoTx blood-count RR
write.csv(rr_clin,      file.path(dest, "univariate_RR_clinical.csv"),     row.names = FALSE)
write.csv(rr_hemo_pre,  file.path(dest, "univariate_RR_hemogram_pre.csv"), row.names = FALSE)
write.csv(rr_hemo_post, file.path(dest, "univariate_RR_hemogram_post.csv"),row.names = FALSE)
message("[20] Univariate robust-Poisson RR saved (clinical + blood count, MI-pooled).")

## ---- Resumen en consola: cuales cruzan p<0.05 ------------------------
resumen <- function(tbl, etiqueta) {
  d <- tbl$table_body
  sig <- d[!is.na(d$p.value) & d$p.value < 0.05 & !is.na(d$estimate), c("label", "estimate", "p.value")]
  message("[20] ", etiqueta, " con p<0.05 (univariado): ",
          if (nrow(sig)) paste(sprintf("%s (OR=%.2f, p=%.3f)", sig$label, sig$estimate, sig$p.value), collapse = "; ")
          else "ninguna")
}
resumen(uni_clin, "Clinicas")
resumen(uni_hemo, "Hemograma pre")
resumen(uni_hemo_post, "Hemograma post")
resumen(uni_var, "Variacion (post-pre)")
message("[20] DONE. Tablas en ", dest)
