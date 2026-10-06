######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 22_model_comparison.R
## Comparacion de MODELOS MULTIVARIADOS anidados (PRE y EoTx), replicando la
## tabla suplementaria de la tesis (do-file 8: modelos A-E + lrtest). Para cada
## modelo candidato se reporta logLik, AIC, BIC, desviance y la prueba de razon
## de verosimilitud (LR) entre modelos anidados -> justifica la seleccion.
## La comparacion de ESTRUCTURA se hace sobre un dataset imputado representativo
## (como la tesis, que corrio sobre datos completos); los RR finales MI-pooled
## estan en 21_logistic_multivariate_RR.R.
##
## INPUT : data/derived/mids_pre_full.rds , mids_post_full.rds
##         outputs/21_logistic_multivariate/cutpoints.csv
## OUTPUT: outputs/90_model_comparison/*.html , model_comparison.rds
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice); library(dplyr); library(gt)
})
paso <- "90_model_comparison"
dest <- out_dir(paso)

## Puntos de corte definidos en el script 21
cps <- read.csv(file.path("outputs", "21_logistic_multivariate", "cutpoints.csv"))
cut_ig   <- cps$punto_corte[cps$variable == "Granulocitos_pre"]
cut_plaq <- cps$punto_corte[cps$variable == "Plaquetas_pre"]
cut_eos  <- cps$punto_corte[cps$variable == "pct_eosino_post"]
cut_mono <- cps$punto_corte[cps$variable == "monocyte_ratio"]

preparar <- function(d) {
  d$fail01 <- as.integer(d$outcome == "failure")
  d$edad_c <- as.numeric(scale(d$edad, scale = FALSE))
  d$ig_bajo   <- factor(ifelse(d$Granulocitos_pre <= cut_ig,   "Low", "High"), levels = c("High","Low"))
  d$plaq_bajo <- factor(ifelse(d$Plaquetas_pre    <= cut_plaq, "Low", "High"), levels = c("High","Low"))
  if ("pct_eosino_post" %in% names(d)) {
    d$eos_alto  <- factor(ifelse(d$pct_eosino_post >= cut_eos, "High", "Low"), levels = c("Low","High"))
    d$mono_sube <- factor(ifelse((d$Monocitos_post/d$Monocitos_pre) >= cut_mono, "Yes", "No"), levels = c("No","Yes"))
  }
  d
}

d_pre  <- preparar(complete(readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds")),  1))
d_post <- preparar(complete(readRDS(file.path(DERIVED_DIR, "mids_post_full.rds")), 1))

ajustar <- function(formula_str, datos)
  glm(as.formula(formula_str), data = datos, family = poisson(link = "log"))

## Tabla resumen de ajuste de una lista de modelos
tabla_ajuste <- function(modelos, descripciones) {
  do.call(rbind, lapply(seq_along(modelos), function(i) {
    m <- modelos[[i]]
    data.frame(Model = names(modelos)[i], Description = descripciones[i],
               df = length(coef(m)), logLik = round(as.numeric(logLik(m)), 2),
               AIC = round(AIC(m), 1), BIC = round(BIC(m), 1),
               Deviance = round(deviance(m), 1))
  }))
}

## ====================================================================
## MODELOS PRE-TRATAMIENTO (candidatos, como do-file 8)
## ====================================================================
pre <- list(
  M1 = ajustar("fail01 ~ sexo + edad_c + tipo_lesion_eval_base_corta + adenopatia_evalbas + infeccion_concom_evalbas + tratamiento + ig_bajo + plaq_bajo", d_pre),
  M2 = ajustar("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo", d_pre),
  M3 = ajustar("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo + tipo_lesion_eval_base_corta", d_pre),
  M4 = ajustar("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo + tratamiento", d_pre),
  M5 = ajustar("fail01 ~ edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo", d_pre),
  M6 = ajustar("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo + tiempo_evolucion_dicotomica", d_pre),
  M7 = ajustar("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo + especie_corta", d_pre)
)
desc_pre <- c("Full (lesion type + treatment)", "Reduced (base, selected)",
              "Base + lesion type", "Base + treatment", "Base - sex",
              "Base + evolution time", "Base + species")
fit_pre <- tabla_ajuste(pre, desc_pre)

## Pruebas LR entre modelos anidados (como lrtest de la tesis)
lr <- function(a, b) {
  an <- anova(a, b, test = "LRT")
  round(an$`Pr(>Chi)`[2], 4)
}
lr_pre <- data.frame(
  comparison = c("M1 vs M2 (drop lesion+tx)", "M2 vs M3 (add lesion)",
                 "M2 vs M4 (add treatment)", "M2 vs M5 (drop sex)",
                 "M2 vs M6 (add evolution)", "M2 vs M7 (add species)"),
  LR_p = c(lr(pre$M2, pre$M1), lr(pre$M2, pre$M3), lr(pre$M2, pre$M4),
           lr(pre$M5, pre$M2), lr(pre$M2, pre$M6), lr(pre$M2, pre$M7))
)

## ====================================================================
## MODELOS FIN DE TRATAMIENTO / EoTx
## ====================================================================
post <- list(
  M1 = ajustar("fail01 ~ sexo + edad_c + tratamiento + infeccion_concom_ftto + eos_alto + mono_sube + tipo_lesion_eval_base_corta + adenopatia_evalbas", d_post),
  M2 = ajustar("fail01 ~ sexo + edad_c + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas", d_post),
  M3 = ajustar("fail01 ~ sexo + edad_c + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas + tipo_lesion_eval_base_corta", d_post),
  M4 = ajustar("fail01 ~ sexo + edad_c + tratamiento + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas", d_post),
  M5 = ajustar("fail01 ~ edad_c + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas", d_post)
)
desc_post <- c("Full (lesion type + treatment)", "Reduced (base, selected)",
               "Base + lesion type", "Base + treatment", "Base - sex")
fit_post <- tabla_ajuste(post, desc_post)
lr_post <- data.frame(
  comparison = c("M1 vs M2 (drop lesion+tx)", "M2 vs M3 (add lesion)",
                 "M2 vs M4 (add treatment)", "M2 vs M5 (drop sex)"),
  LR_p = c(lr(post$M2, post$M1), lr(post$M2, post$M3),
           lr(post$M2, post$M4), lr(post$M5, post$M2))
)

## ---- Exportar --------------------------------------------------------
gtsave(gt(fit_pre)  %>% tab_header("Pre-treatment candidate models (fit)"),  file.path(dest, "fit_pretreatment.html"))
gtsave(gt(lr_pre)   %>% tab_header("Pre-treatment nested LR tests"),         file.path(dest, "lrtests_pretreatment.html"))
gtsave(gt(fit_post) %>% tab_header("End-of-treatment candidate models (fit)"), file.path(dest, "fit_eotx.html"))
gtsave(gt(lr_post)  %>% tab_header("End-of-treatment nested LR tests"),      file.path(dest, "lrtests_eotx.html"))
saveRDS(list(fit_pre = fit_pre, lr_pre = lr_pre, fit_post = fit_post, lr_post = lr_post),
        file.path(dest, "model_comparison.rds"))

message("[22] PRE candidate models (AIC):")
for (i in seq_len(nrow(fit_pre))) message(sprintf("   %-4s %-32s AIC=%.1f BIC=%.1f", fit_pre$Model[i], fit_pre$Description[i], fit_pre$AIC[i], fit_pre$BIC[i]))
message("[22] PRE nested LR tests: ", paste(sprintf("%s p=%.3f", lr_pre$comparison, lr_pre$LR_p), collapse = " | "))
message("[22] POST candidate models (AIC):")
for (i in seq_len(nrow(fit_post))) message(sprintf("   %-4s %-32s AIC=%.1f BIC=%.1f", fit_post$Model[i], fit_post$Description[i], fit_post$AIC[i], fit_post$BIC[i]))
message("[22] POST nested LR tests: ", paste(sprintf("%s p=%.3f", lr_post$comparison, lr_post$LR_p), collapse = " | "))
message("[22] DONE. Tablas en ", dest)
