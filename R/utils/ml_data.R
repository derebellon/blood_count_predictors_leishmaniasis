######################################################################
## utils/ml_data.R
## Prepara los data.frame CRUDOS (con NA) con el set de features para ML,
## para que la imputacion ocurra DENTRO del fold (sin fuga). A diferencia de
## la inferencia (mids), aqui las features derivadas (%/ratios/variaciones) se
## calculan de los conteos crudos; la imputacion train-fit se hace sobre el set
## completo dentro de cada fold (missingness pequena). Incluye las ratios X/IG
## floored (piso 0.01) SOLO para ML.
## Devuelve: $pre (n=200), $post (n=171, con hemograma post) y las listas de
## predictores por escenario.
######################################################################

cargar_datos_ml <- function() {
  source("R/00_config.R")
  raw <- read.csv(ANALYSIS_CSV, stringsAsFactors = FALSE, check.names = TRUE)
  raw$fail01 <- as.integer(raw[[OUTCOME_VAR]] == OUTCOME_EVENT)

  ## Recodes estructurales (igual que inferencia)
  raw$especie_corta[is.na(raw$especie_corta) | raw$especie_corta %in% c("", "NA")] <- "Not isolated/Unknown"
  raw$comorbilidades <- factor(raw$comorbilidades, levels = c("No", "Yes"))

  ## Derivadas desde conteos CRUDOS (NA se propaga, se imputa en el fold)
  derivar <- function(df, s) {
    g <- function(x) df[[paste0(x, "_", s)]]
    total <- g("Neutrofilos") + g("Linfocitos") + g("Monocitos") + g("Eosinofilos") + g("Basofilos")
    df[[paste0("pct_neutro_", s)]]  <- 100 * g("Neutrofilos") / total
    df[[paste0("pct_linfo_",  s)]]  <- 100 * g("Linfocitos")  / total
    df[[paste0("pct_mono_",   s)]]  <- 100 * g("Monocitos")   / total
    df[[paste0("pct_eosino_", s)]]  <- 100 * g("Eosinofilos") / total
    df[[paste0("pct_baso_",   s)]]  <- 100 * g("Basofilos")   / total
    df[[paste0("i_neu_linfo_",  s)]] <- g("Neutrofilos") / g("Linfocitos")
    df[[paste0("i_eos_linfo_",  s)]] <- g("Eosinofilos") / g("Linfocitos")
    df[[paste0("i_mono_linfo_", s)]] <- g("Monocitos")   / g("Linfocitos")
    df[[paste0("i_baso_linfo_", s)]] <- g("Basofilos")   / g("Linfocitos")
    df[[paste0("i_granu_linfo_", s)]]<- g("Granulocitos")/ g("Linfocitos")
    df[[paste0("i_eos_mono_",   s)]] <- g("Eosinofilos") / g("Monocitos")
    df[[paste0("i_eos_neu_",    s)]] <- g("Eosinofilos") / g("Neutrofilos")
    df[[paste0("i_eos_baso_",   s)]] <- g("Eosinofilos") / g("Basofilos")
    df[[paste0("i_neu_mono_",   s)]] <- g("Neutrofilos") / g("Monocitos")
    df[[paste0("i_neu_baso_",   s)]] <- g("Neutrofilos") / g("Basofilos")
    df[[paste0("i_baso_mono_",  s)]] <- g("Basofilos")   / g("Monocitos")
    df[[paste0("i_monoeosneu_lin_", s)]] <- (g("Monocitos")+g("Eosinofilos")+g("Neutrofilos")) / g("Linfocitos")
    # ratios X/IG con piso 0.01 (SOLO ML)
    ig <- pmax(g("Granulocitos"), 0.01)
    df[[paste0("i_neu_granu_", s)]] <- g("Neutrofilos") / ig
    df[[paste0("i_eos_granu_", s)]] <- g("Eosinofilos") / ig
    df[[paste0("i_baso_granu_",s)]] <- g("Basofilos")   / ig
    df
  }
  raw <- derivar(raw, "pre")
  raw <- derivar(raw, "post")
  ## Variaciones post - pre
  for (v in c("Neutrofilos","Linfocitos","Monocitos","Eosinofilos","Basofilos",
              "Granulocitos","Globulos_rojos","Hemoglobina","Hematocrito")) {
    raw[[paste0("var_", v)]] <- raw[[paste0(v, "_post")]] - raw[[paste0(v, "_pre")]]
  }
  raw$mono_ratio <- raw$Monocitos_post / pmax(raw$Monocitos_pre, 0.01)

  ## Tipos
  factores <- c("sexo","etnia","tiempo_evolucion_dicotomica","lesiones_categorica",
                "tipo_lesion_eval_base_corta","adenopatia_evalbas","infeccion_concom_evalbas",
                "especie_corta","tratamiento","rango_dosis_medicamento",
                "variacion_lesion_post","infeccion_concom_ftto")
  for (v in intersect(factores, names(raw))) raw[[v]] <- factor(raw[[v]])

  ## ---- Listas de predictores por escenario -------------------------
  clinicas <- c("edad","sexo","etnia","tiempo_evolucion_dicotomica","imc",
                "lesiones_categorica","tipo_lesion_eval_base_corta","adenopatia_evalbas",
                "infeccion_concom_evalbas","comorbilidades","especie_corta","tratamiento",
                "rango_dosis_medicamento")
  base_pre <- c("Neutrofilos_pre","Linfocitos_pre","Monocitos_pre","Eosinofilos_pre",
                "Basofilos_pre","Granulocitos_pre","Globulos_rojos_pre","Hemoglobina_pre",
                "Hematocrito_pre","Plaquetas_pre")
  deriv_pre <- grep("_pre$", names(raw), value = TRUE)
  deriv_pre <- deriv_pre[grepl("^pct_|^i_", deriv_pre)]
  hemo_pre_ml <- c(base_pre, deriv_pre)

  clinicas_post <- c("variacion_lesion_post","infeccion_concom_ftto")
  base_post <- c("Neutrofilos_post","Linfocitos_post","Monocitos_post","Eosinofilos_post",
                 "Basofilos_post","Granulocitos_post","Globulos_rojos_post",
                 "Hemoglobina_post","Hematocrito_post")
  deriv_post <- grep("_post$", names(raw), value = TRUE)
  deriv_post <- deriv_post[grepl("^pct_|^i_", deriv_post)]
  variaciones <- c(grep("^var_", names(raw), value = TRUE), "mono_ratio")
  hemo_post_ml <- c(base_post, deriv_post, variaciones)

  pre  <- raw[, c("fail01", clinicas, hemo_pre_ml)]
  tiene_post <- raw$hemo_pos_realizado %in% c("1", 1)
  post <- raw[tiene_post, c("fail01", clinicas, hemo_pre_ml, clinicas_post, hemo_post_ml)]

  list(pre = pre, post = post,
       predictores = list(clinicas = clinicas, hemo_pre = hemo_pre_ml,
                          clinicas_post = clinicas_post, hemo_post = hemo_post_ml))
}
