######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 15_feature_engineering.R
## A partir de los CONTEOS imputados (mids_pre / mids_post) recalculo las
## variables derivadas del hemograma para que queden internamente consistentes:
##   - porcentajes (abundancia relativa = conteo / total de leucocitos)
##   - los 15 indices/ratios entre lineas celulares (como en la tesis)
##   - granulocitos INMADUROS (IG) se conservan tal cual (no son suma de nada)
##   - variaciones post - pre (solo en el escenario POST)
## NO se imputan las derivadas sueltas: se derivan de los conteos imputados.
## Se reconstruye el objeto mice (as.mids) para poder hacer pooling en las
## regresiones de inferencia.
##
## INPUT : data/derived/mids_pre.rds , data/derived/mids_post.rds
## OUTPUT: data/derived/mids_pre_full.rds , data/derived/mids_post_full.rds
######################################################################

source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice)   ## imputacion multiple y pooling
  library(dplyr)  ## manejo de datos
})

## Componentes maduros (su suma = leucocito total, que NO usamos como predictor)
comp_madur <- c("Neutrofilos", "Linfocitos", "Monocitos", "Eosinofilos", "Basofilos")

## ---- Funcion que agrega las variables derivadas a un data.frame -------
agregar_derivadas <- function(df, sufijo) {
  # sufijo = "pre" o "post"
  g <- function(stem) df[[paste0(stem, "_", sufijo)]]

  # Total de leucocitos (suma de los 5 maduros) SOLO para calcular porcentajes
  total <- g("Neutrofilos") + g("Linfocitos") + g("Monocitos") +
           g("Eosinofilos") + g("Basofilos")

  # Porcentajes = abundancia relativa respecto al total (David: 2 eos en 3 no es
  # lo mismo que 2 en 1000) -> informacion distinta al conteo absoluto
  df[[paste0("pct_neutro_", sufijo)]]  <- 100 * g("Neutrofilos")  / total
  df[[paste0("pct_linfo_",  sufijo)]]  <- 100 * g("Linfocitos")   / total
  df[[paste0("pct_mono_",   sufijo)]]  <- 100 * g("Monocitos")    / total
  df[[paste0("pct_eosino_", sufijo)]]  <- 100 * g("Eosinofilos")  / total
  df[[paste0("pct_baso_",   sufijo)]]  <- 100 * g("Basofilos")    / total

  # Los 15 indices/ratios entre lineas celulares (igual que en la tesis)
  # IG = granulocitos inmaduros (Granulocitos), se usa en los ratios "granu"
  df[[paste0("i_neu_linfo_",   sufijo)]] <- g("Neutrofilos") / g("Linfocitos")   # NLR
  df[[paste0("i_eos_linfo_",   sufijo)]] <- g("Eosinofilos") / g("Linfocitos")
  df[[paste0("i_mono_linfo_",  sufijo)]] <- g("Monocitos")   / g("Linfocitos")
  df[[paste0("i_baso_linfo_",  sufijo)]] <- g("Basofilos")   / g("Linfocitos")
  df[[paste0("i_granu_linfo_", sufijo)]] <- g("Granulocitos")/ g("Linfocitos")   # IG / linfo (seguro: linfo>0)
  # NOTA: se OMITEN i_neu_granu, i_eos_granu, i_baso_granu (dividir por IG) porque
  # los granulocitos inmaduros son 0 en ~9% -> ratio indefinido (Inf). IG se conserva
  # como conteo y como IG/linfocitos. Todos los demas denominadores nunca son 0.
  df[[paste0("i_eos_mono_",    sufijo)]] <- g("Eosinofilos") / g("Monocitos")
  df[[paste0("i_eos_neu_",     sufijo)]] <- g("Eosinofilos") / g("Neutrofilos")
  df[[paste0("i_eos_baso_",    sufijo)]] <- g("Eosinofilos") / g("Basofilos")
  df[[paste0("i_neu_mono_",    sufijo)]] <- g("Neutrofilos") / g("Monocitos")
  df[[paste0("i_neu_baso_",    sufijo)]] <- g("Neutrofilos") / g("Basofilos")
  df[[paste0("i_baso_mono_",   sufijo)]] <- g("Basofilos")   / g("Monocitos")
  df[[paste0("i_monoeosneu_lin_", sufijo)]] <- (g("Monocitos") + g("Eosinofilos") +
                                                g("Neutrofilos")) / g("Linfocitos")
  df
}

## Variaciones post - pre (solo escenario POST, que tiene ambos tiempos)
agregar_variaciones <- function(df) {
  conteos <- c(comp_madur, "Granulocitos", "Globulos_rojos", "Hemoglobina", "Hematocrito")
  for (v in conteos) {
    pre <- df[[paste0(v, "_pre")]]; pos <- df[[paste0(v, "_post")]]
    if (!is.null(pre) && !is.null(pos)) df[[paste0("var_", v)]] <- pos - pre
  }
  # variacion de los ratios clave
  for (r in c("i_neu_linfo", "i_mono_linfo", "i_granu_linfo")) {
    pre <- df[[paste0(r, "_pre")]]; pos <- df[[paste0(r, "_post")]]
    if (!is.null(pre) && !is.null(pos)) df[[paste0("var_", r)]] <- pos - pre
  }
  df
}

## ---- PRE: agregar derivadas y reconstruir el objeto mice --------------
long_pre <- complete(readRDS(file.path(DERIVED_DIR, "mids_pre.rds")), "long", include = TRUE)
long_pre <- agregar_derivadas(long_pre, "pre")
mids_pre_full <- as.mids(long_pre)
saveRDS(mids_pre_full, file.path(DERIVED_DIR, "mids_pre_full.rds"))
message("[15] PRE  full features: ", ncol(long_pre), " cols (mids_pre_full.rds)")

## ---- POST: derivadas pre + post + variaciones -------------------------
long_post <- complete(readRDS(file.path(DERIVED_DIR, "mids_post.rds")), "long", include = TRUE)
long_post <- agregar_derivadas(long_post, "pre")
long_post <- agregar_derivadas(long_post, "post")
long_post <- agregar_variaciones(long_post)
mids_post_full <- as.mids(long_post)
saveRDS(mids_post_full, file.path(DERIVED_DIR, "mids_post_full.rds"))
message("[15] POST full features: ", ncol(long_post), " cols (mids_post_full.rds)")

## ---- Chequeo de sanidad: derivadas sin NA/Inf en el 1er imputado ------
chk <- complete(mids_post_full, 1)
der <- grep("^pct_|^i_|^var_", names(chk), value = TRUE)
problemas <- der[sapply(der, function(v) any(is.na(chk[[v]]) | is.infinite(chk[[v]])))]
message("[15][sanity] derivadas con NA/Inf (imputado 1): ",
        if (length(problemas)) paste(problemas, collapse = ", ") else "ninguna")
message("[15] DONE.")
