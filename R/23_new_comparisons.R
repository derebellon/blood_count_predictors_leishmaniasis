######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 23_new_comparisons.R
## Comparaciones ADICIONALES que no estaban en la tesis, para responder mejor
## la pregunta de investigacion:
##  (A) HIPOTESIS CENTRAL: clinico solo vs clinico + hemograma vs hemograma solo
##      -> prueba LR de si el hemograma AGREGA sobre lo clinico (PRE y EoTx).
##  (B) Continuo vs dicotomizado: la dicotomizacion pierde informacion predictiva?
##  (C) Modelo COMBINADO pre + EoTx (en quienes tienen ambos): el EoTx agrega
##      sobre lo PRE?
## Discriminacion in-sample (AUC) + AIC/BIC + prueba LR. La validacion fuera de
## muestra (CV/bootstrap) va en el bloque de ML.
##
## INPUT : data/derived/mids_pre_full.rds , mids_post_full.rds ,
##         outputs/21_logistic_multivariate/cutpoints.csv
## OUTPUT: outputs/90_model_comparison/new_*.html , new_comparisons.rds
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice); library(dplyr); library(gt); library(pROC)
})
paso <- "90_model_comparison"
dest <- out_dir(paso)

cps <- read.csv(file.path("outputs", "21_logistic_multivariate", "cutpoints.csv"))
cut_ig   <- cps$punto_corte[cps$variable == "Granulocitos_pre"]
cut_plaq <- cps$punto_corte[cps$variable == "Plaquetas_pre"]
cut_eos  <- cps$punto_corte[cps$variable == "pct_eosino_post"]
cut_mono <- cps$punto_corte[cps$variable == "monocyte_ratio"]

preparar <- function(d) {
  d$fail01 <- as.integer(d$outcome == "failure")
  d$edad_c <- as.numeric(scale(d$edad, scale = FALSE))
  # dicotomizadas (como la tesis)
  d$ig_bajo   <- factor(ifelse(d$Granulocitos_pre <= cut_ig,   "Low", "High"), levels = c("High","Low"))
  d$plaq_bajo <- factor(ifelse(d$Plaquetas_pre    <= cut_plaq, "Low", "High"), levels = c("High","Low"))
  # continuas estandarizadas (para comparar con la dicotomizacion)
  d$ig_z   <- as.numeric(scale(d$Granulocitos_pre))
  d$plaq_z <- as.numeric(scale(d$Plaquetas_pre))
  if ("pct_eosino_post" %in% names(d)) {
    d$eos_alto  <- factor(ifelse(d$pct_eosino_post >= cut_eos, "High", "Low"), levels = c("Low","High"))
    d$mono_sube <- factor(ifelse((d$Monocitos_post/d$Monocitos_pre) >= cut_mono, "Yes", "No"), levels = c("No","Yes"))
    d$eos_z     <- as.numeric(scale(d$pct_eosino_post))
    d$mono_z    <- as.numeric(scale(d$Monocitos_post/d$Monocitos_pre))
  }
  d
}
d_pre  <- preparar(complete(readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds")),  1))
d_post <- preparar(complete(readRDS(file.path(DERIVED_DIR, "mids_post_full.rds")), 1))

aj  <- function(f, d) glm(as.formula(f), data = d, family = poisson(link = "log"))
auc <- function(m, d) as.numeric(pROC::auc(pROC::roc(d$fail01, predict(m, type = "response"), quiet = TRUE)))
lrp <- function(small, big) round(anova(small, big, test = "LRT")$`Pr(>Chi)`[2], 4)
fila <- function(nombre, m, d) data.frame(Model = nombre, df = length(coef(m)),
             AIC = round(AIC(m),1), BIC = round(BIC(m),1), AUC = round(auc(m,d),3))

## ====================================================================
## (A) HIPOTESIS: clinico solo vs clinico+hemograma vs hemograma solo
## ====================================================================
# PRE
clin_pre <- "fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas"
pre_clin  <- aj(clin_pre, d_pre)
pre_both  <- aj(paste(clin_pre, "+ ig_bajo + plaq_bajo"), d_pre)
pre_hemo  <- aj("fail01 ~ ig_bajo + plaq_bajo", d_pre)
A_pre <- rbind(fila("Clinical only", pre_clin, d_pre),
               fila("Clinical + blood count", pre_both, d_pre),
               fila("Blood count only", pre_hemo, d_pre))
A_pre_lr <- lrp(pre_clin, pre_both)   # el hemograma AGREGA sobre lo clinico?

# EoTx
clin_post <- "fail01 ~ sexo + edad_c + infeccion_concom_ftto + adenopatia_evalbas"
post_clin <- aj(clin_post, d_post)
post_both <- aj(paste(clin_post, "+ eos_alto + mono_sube"), d_post)
post_hemo <- aj("fail01 ~ eos_alto + mono_sube", d_post)
A_post <- rbind(fila("Clinical only", post_clin, d_post),
                fila("Clinical + blood count", post_both, d_post),
                fila("Blood count only", post_hemo, d_post))
A_post_lr <- lrp(post_clin, post_both)

## ====================================================================
## (B) Continuo vs dicotomizado (sobre el modelo clinico+hemograma)
## ====================================================================
pre_dic  <- pre_both
pre_cont <- aj(paste(clin_pre, "+ ig_z + plaq_z"), d_pre)
B_pre <- rbind(fila("Dichotomized (cutpoints)", pre_dic, d_pre),
               fila("Continuous (per SD)", pre_cont, d_pre))
post_dic  <- post_both
post_cont <- aj(paste(clin_post, "+ eos_z + mono_z"), d_post)
B_post <- rbind(fila("Dichotomized (cutpoints)", post_dic, d_post),
                fila("Continuous (per SD)", post_cont, d_post))

## ====================================================================
## (C) Combinado PRE + EoTx (en el subset con ambos, n=171)
##     El EoTx agrega sobre un modelo PRE?
## ====================================================================
pre_in_post  <- aj("fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo", d_post)
comb         <- aj("fail01 ~ sexo + edad_c + adenopatia_evalbas + ig_bajo + plaq_bajo + infeccion_concom_ftto + eos_alto + mono_sube", d_post)
C_tab <- rbind(fila("Pre-treatment model (in EoTx subset)", pre_in_post, d_post),
               fila("End-of-treatment model", post_both, d_post),
               fila("Combined pre + EoTx", comb, d_post))
C_lr <- lrp(pre_in_post, comb)   # agregar EoTx a un modelo PRE mejora?

## ---- Exportar + resumen ----------------------------------------------
gtsave(gt(A_pre)  %>% tab_header("(A) Hypothesis test — PRE: clinical vs +blood count vs blood count only"),  file.path(dest, "new_A_hypothesis_pre.html"))
gtsave(gt(A_post) %>% tab_header("(A) Hypothesis test — EoTx: clinical vs +blood count vs blood count only"), file.path(dest, "new_A_hypothesis_eotx.html"))
gtsave(gt(B_pre)  %>% tab_header("(B) Dichotomized vs continuous — PRE"),  file.path(dest, "new_B_cont_vs_dic_pre.html"))
gtsave(gt(B_post) %>% tab_header("(B) Dichotomized vs continuous — EoTx"), file.path(dest, "new_B_cont_vs_dic_eotx.html"))
gtsave(gt(C_tab)  %>% tab_header("(C) Combined pre + EoTx model"),         file.path(dest, "new_C_combined.html"))
saveRDS(list(A_pre=A_pre, A_pre_lr=A_pre_lr, A_post=A_post, A_post_lr=A_post_lr,
             B_pre=B_pre, B_post=B_post, C=C_tab, C_lr=C_lr), file.path(dest, "new_comparisons.rds"))

pr <- function(tab) for (i in seq_len(nrow(tab))) message(sprintf("     %-38s AIC=%.1f AUC=%.3f", tab$Model[i], tab$AIC[i], tab$AUC[i]))
message("[23] (A) HIPOTESIS PRE:");  pr(A_pre);  message("     LR hemograma-agrega-sobre-clinico: p=", A_pre_lr)
message("[23] (A) HIPOTESIS EoTx:"); pr(A_post); message("     LR hemograma-agrega-sobre-clinico: p=", A_post_lr)
message("[23] (B) Cont vs Dic PRE:");  pr(B_pre)
message("[23] (B) Cont vs Dic EoTx:"); pr(B_post)
message("[23] (C) Combinado:"); pr(C_tab); message("     LR EoTx-agrega-sobre-PRE: p=", C_lr)
message("[23] DONE. Tablas en ", dest)
