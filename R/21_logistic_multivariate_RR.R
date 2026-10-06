######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 21_logistic_multivariate_RR.R
## Modelos MULTIVARIADOS de riesgo relativo (RR) por Poisson ROBUSTA
## (family=poisson, link=log, errores estandar robustos tipo sandwich),
## portados de STATA (do-file 8) a R. Se corre sobre la imputacion multiple
## y se AGRUPA con las reglas de Rubin aplicadas a los estimadores robustos.
## Modelos separados PRE-tratamiento y FIN de tratamiento (EoTx), como en la tesis.
## Los parametros del hemograma se DICOTOMIZAN con puntos de corte (Youden,
## cutpointr), igual que en la tesis -> utilidad clinica.
##
## INPUT : data/derived/mids_pre_full.rds (n=200), data/derived/mids_post_full.rds (n=171)
## OUTPUT: outputs/21_logistic_multivariate/*.html , cutpoints.csv , RR_models.rds
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice)       ## imputacion multiple
  library(sandwich)   ## errores estandar robustos (vcov tipo Huber-White)
  library(cutpointr)  ## puntos de corte (Youden)
  library(gt)         ## tablas HTML
  library(dplyr)      ## manejo de datos
})
paso <- "21_logistic_multivariate"
dest <- out_dir(paso)

## ====================================================================
## 1. PUNTOS DE CORTE (Youden) para los parametros del hemograma clave
##    Se calculan sobre el 1er dataset imputado (estable) y se reportan.
## ====================================================================
calc_cutpoint <- function(df, var, direction) {
  # direction: ">=" si valores altos -> mas falla ; "<=" si bajos -> mas falla
  cp <- cutpointr(data = df, x = !!rlang::sym(var), class = fail01,
                  pos_class = 1, neg_class = 0,
                  method = maximize_metric, metric = youden,
                  direction = direction, silent = TRUE)
  cp$optimal_cutpoint[1]
}

d1_pre  <- complete(readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds")), 1)
d1_post <- complete(readRDS(file.path(DERIVED_DIR, "mids_post_full.rds")), 1)
d1_pre$fail01  <- as.integer(d1_pre$outcome  == "failure")
d1_post$fail01 <- as.integer(d1_post$outcome == "failure")
# Razon de cambio de monocitos (post/pre), como en la tesis (razon_cambio_monocitos)
d1_post$monocyte_ratio <- d1_post$Monocitos_post / d1_post$Monocitos_pre

cut_ig    <- calc_cutpoint(d1_pre,  "Granulocitos_pre", "<=")   # IG bajos -> mas falla
cut_plaq  <- calc_cutpoint(d1_pre,  "Plaquetas_pre",    "<=")   # plaquetas bajas -> mas falla
cut_eos   <- calc_cutpoint(d1_post, "pct_eosino_post",  ">=")   # eos% altos -> mas falla
cut_mono  <- calc_cutpoint(d1_post, "monocyte_ratio",   ">=")   # sube monocitos -> mas falla

cutpoints <- data.frame(
  parametro = c("Immature granulocytes (pre)", "Platelets (pre)",
                "Eosinophil % (EoTx)", "Monocyte ratio EoTx/pre"),
  variable  = c("Granulocitos_pre", "Plaquetas_pre", "pct_eosino_post", "monocyte_ratio"),
  direccion = c("<=", "<=", ">=", ">="),
  punto_corte = c(cut_ig, cut_plaq, cut_eos, cut_mono),
  tesis_reporto = c("<= 0.01", "(RR 0.99/unidad)", ">= 14%", ">= 1.07")
)
write.csv(cutpoints, file.path(dest, "cutpoints.csv"), row.names = FALSE)
message("[21] Puntos de corte (Youden):")
for (i in seq_len(nrow(cutpoints)))
  message(sprintf("   %-28s %s %.3f   (tesis: %s)", cutpoints$parametro[i],
                  cutpoints$direccion[i], cutpoints$punto_corte[i], cutpoints$tesis_reporto[i]))

## ====================================================================
## 2. RR por Poisson robusta con imputacion multiple (pooling de Rubin)
##    Dicotomiza con los puntos de corte dentro de cada dataset imputado.
## ====================================================================
preparar_dicotomicas <- function(d) {
  d$fail01   <- as.integer(d$outcome == "failure")
  d$edad_c   <- as.numeric(scale(d$edad, scale = FALSE))          # edad centrada
  d$ig_bajo        <- factor(ifelse(d$Granulocitos_pre <= cut_ig,  "Low (<=cut)",  "High"), levels=c("High","Low (<=cut)"))
  d$plaq_bajo      <- factor(ifelse(d$Plaquetas_pre    <= cut_plaq,"Low (<=cut)",  "High"), levels=c("High","Low (<=cut)"))
  if ("pct_eosino_post" %in% names(d)) {
    d$eos_alto     <- factor(ifelse(d$pct_eosino_post  >= cut_eos, ">=cut", "<cut"), levels=c("<cut",">=cut"))
    d$mono_sube    <- factor(ifelse((d$Monocitos_post/d$Monocitos_pre) >= cut_mono, ">=cut", "<cut"), levels=c("<cut",">=cut"))
  }
  d
}

## RR robusta MI-pooled: ajusta Poisson+log en cada imputado, saca coef y vcov
## robusto (HC0 ~ vce(robust) de Stata), y agrupa con Rubin.
rr_robusto_mi <- function(mids_obj, formula_str) {
  m <- mids_obj$m
  ests <- lapply(seq_len(m), function(i) {
    d <- preparar_dicotomicas(complete(mids_obj, i))
    f <- glm(as.formula(formula_str), data = d, family = poisson(link = "log"))
    list(b = coef(f), V = sandwich::vcovHC(f, type = "HC0"))
  })
  bmat <- sapply(ests, `[[`, "b")                       # k x m
  if (is.null(dim(bmat))) bmat <- matrix(bmat, nrow = 1, dimnames = list(names(ests[[1]]$b), NULL))
  Qbar <- rowMeans(bmat)
  Ubar <- Reduce(`+`, lapply(ests, `[[`, "V")) / m      # within-imputation
  B    <- if (m > 1) stats::cov(t(bmat)) else diag(0, length(Qbar))  # between
  Tvar <- diag(Ubar) + (1 + 1/m) * diag(B)              # Rubin total variance
  SE   <- sqrt(Tvar)
  z    <- Qbar / SE
  data.frame(term = names(Qbar), RR = exp(Qbar),
             lo = exp(Qbar - 1.96*SE), hi = exp(Qbar + 1.96*SE),
             p = 2*pnorm(-abs(z)), row.names = NULL)
}

tabla_rr <- function(rr, titulo, archivo) {
  rr <- rr[rr$term != "(Intercept)", ]
  g <- rr %>%
    mutate(`RR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", RR, lo, hi),
           `p-value` = style_pvalue_chr(p)) %>%
    select(Variable = term, `RR (95% CI)`, `p-value`) %>%
    gt() %>% tab_header(title = titulo)
  gtsave(g, file.path(dest, archivo))
  invisible(g)
}
style_pvalue_chr <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

## ---- MODELO PRE-TRATAMIENTO (n=200) ----------------------------------
mids_pre  <- readRDS(file.path(DERIVED_DIR, "mids_pre_full.rds"))
f_pre <- "fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo"
rr_pre <- rr_robusto_mi(mids_pre, f_pre)
tabla_rr(rr_pre, "Multivariate robust-Poisson RR — Pre-treatment model", "RR_pretreatment.html")

## ---- MODELO FIN DE TRATAMIENTO / EoTx (n=171) ------------------------
mids_post <- readRDS(file.path(DERIVED_DIR, "mids_post_full.rds"))
f_post <- "fail01 ~ sexo + edad_c + tratamiento + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas"
rr_post <- rr_robusto_mi(mids_post, f_post)
tabla_rr(rr_post, "Multivariate robust-Poisson RR — End-of-treatment model", "RR_eotx.html")

saveRDS(list(cutpoints = cutpoints, pre = rr_pre, eotx = rr_post),
        file.path(dest, "RR_models.rds"))

## ---- Resumen en consola (comparar con Tabla 3 de la tesis) -----------
mostrar <- function(rr, etiqueta) {
  rr <- rr[rr$term != "(Intercept)", ]
  message("[21] ", etiqueta, ":")
  for (i in seq_len(nrow(rr)))
    message(sprintf("     %-30s RR=%.2f (%.2f-%.2f) p=%.3f",
                    rr$term[i], rr$RR[i], rr$lo[i], rr$hi[i], rr$p[i]))
}
mostrar(rr_pre,  "PRE-tratamiento")
mostrar(rr_post, "FIN de tratamiento (EoTx)")
message("[21] DONE. Tablas en ", dest)
