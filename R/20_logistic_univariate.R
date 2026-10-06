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
