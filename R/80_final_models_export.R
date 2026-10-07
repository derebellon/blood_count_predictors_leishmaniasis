######################################################################
## RESEARCH PROJECT: "Blood count parameters as early predictors of
## therapeutic outcome in cutaneous leishmaniasis"
## 80_final_models_export.R
## Entrena los modelos FINALES con TODA la data (no un subset) sobre el set
## CURADO, y los exporta a tool/models.json para correr en el navegador:
##  - logistica (parsimoniosa, la mejor): intercepto + coeficientes + medias
##  - LASSO: coeficientes a lambda.min
##  - XGBoost: arboles en JSON (xgb.dump)
##  + estadisticos de imputacion (mediana/moda de entrenamiento) para datos
##    incompletos, + desempeno validado (AUC OOS por CV) para mostrar al medico.
## Valida los modelos aplicandolos a nuestros propios pacientes.
##
## INPUT : via utils/ml_data.R
## OUTPUT: tool/models.json ; outputs/80_final_models/validation.txt
######################################################################

source("R/00_config.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(glmnet); library(xgboost); library(jsonlite); library(pROC) })
dest <- out_dir("80_final_models")

datos <- cargar_datos_ml()

## Predictores del set CURADO que usa la herramienta (entradas crudas del lab)
pre_vars  <- c("edad", "sexo", "adenopatia_evalbas", "infeccion_concom_evalbas",
               "Granulocitos_pre", "Plaquetas_pre")
post_vars <- c("edad", "sexo", "infeccion_concom_ftto", "adenopatia_evalbas",
               "pct_eosino_post", "mono_ratio")

## Imputacion mediana/moda sobre TODA la data (para entrenar el modelo final y
## para rellenar datos incompletos en la herramienta).
stats_imput <- function(df, vars) {
  out <- list()
  for (v in vars) {
    if (is.numeric(df[[v]])) out[[v]] <- list(tipo = "num", valor = stats::median(df[[v]], na.rm = TRUE))
    else out[[v]] <- list(tipo = "cat", valor = names(sort(table(df[[v]]), decreasing = TRUE))[1])
  }
  out
}
aplicar_imput <- function(df, imp) {
  for (v in names(imp)) df[[v]][is.na(df[[v]])] <- imp[[v]]$valor
  df
}

exportar_escenario <- function(df, vars, etiqueta) {
  imp <- stats_imput(df, vars)
  d <- aplicar_imput(df, imp)
  for (v in vars) if (is.character(d[[v]])) d[[v]] <- factor(d[[v]])
  form <- stats::as.formula(paste("~", paste(vars, collapse = " + ")))
  X <- stats::model.matrix(form, d)[, -1, drop = FALSE]
  y <- d$fail01
  ## SIN pesos de clase: para la herramienta queremos PROBABILIDADES CALIBRADAS
  ## a la prevalencia real (15.5%), no infladas. La discriminacion (AUC) es ~igual.
  base <- mean(y)

  ## --- Logistica (final, calibrada) ---
  glm_fit <- suppressWarnings(glm(y ~ ., data = data.frame(y = y, X), family = binomial))
  glm_coef <- coef(glm_fit)
  col_means <- colMeans(X)                       # para la contribucion por variable

  ## --- LASSO ---
  la <- glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1, nfolds = 10, type.measure = "auc")
  la_coef <- as.matrix(coef(la, s = "lambda.min"))[, 1]

  ## --- XGBoost (arboles -> UN solo JSON array) ---
  xgb <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", eval_metric = "auc", max_depth = 2,
                  eta = 0.05, subsample = 0.8, colsample_bytree = 0.6, min_child_weight = 5,
                  lambda = 1, alpha = 0.5, base_score = base),
    data = xgboost::xgb.DMatrix(X, label = y), nrounds = 80, verbose = 0)
  xgb_json <- paste(xgboost::xgb.dump(xgb, dump_format = "json"), collapse = "")

  ## --- Validacion en nuestros pacientes (resustitucion, referencia) ---
  p_glm <- predict(glm_fit, type = "response")
  auc_glm <- as.numeric(pROC::auc(pROC::roc(y, p_glm, quiet = TRUE)))

  list(
    features = colnames(X),
    logistic = list(intercept = unname(glm_coef[1]),
                    coef = as.list(glm_coef[-1]),
                    means = as.list(col_means)),
    lasso = list(intercept = unname(la_coef["(Intercept)"]),
                 coef = as.list(la_coef[setdiff(names(la_coef), "(Intercept)")])),
    xgboost = list(trees = xgb_json, base_score = base),
    imputation = imp,
    auc_resubstitution = round(auc_glm, 3)
  )
}

message("[80] Entrenando modelos finales PRE (toda la data, n=", nrow(datos$pre), ")...")
pre  <- exportar_escenario(datos$pre,  pre_vars,  "PRE")
message("[80] Entrenando modelos finales EoTx (toda la data, n=", nrow(datos$post), ")...")
post <- exportar_escenario(datos$post, post_vars, "EoTx")

modelos <- list(
  meta = list(
    outcome = "Therapeutic failure in cutaneous leishmaniasis (final outcome, assessed up to week 26)",
    note = "Models trained on the full dataset. Honest out-of-sample discrimination (repeated CV): PRE ROC-AUC ~0.75, EoTx ~0.70.",
    cv_auc = list(pre = 0.75, eotx = 0.70)
  ),
  pre = pre, eotx = post
)
write_json(modelos, "tool/models.json", auto_unbox = TRUE, pretty = TRUE, digits = 8)

writeLines(c(
  paste0("PRE  AUC resustitucion (optimista): ", pre$auc_resubstitution, "  | CV OOS ~0.75"),
  paste0("EoTx AUC resustitucion (optimista): ", post$auc_resubstitution, " | CV OOS ~0.70"),
  paste0("features PRE : ", paste(pre$features, collapse = ", ")),
  paste0("features EoTx: ", paste(post$features, collapse = ", "))
), file.path(dest, "validation.txt"))

message("[80] PRE  features: ", paste(pre$features, collapse = ", "))
message("[80] EoTx features: ", paste(post$features, collapse = ", "))
message("[80] AUC resustitucion PRE=", pre$auc_resubstitution, " EoTx=", post$auc_resubstitution, " (OOS por CV ~0.75/0.70)")
message("[80] DONE. Exportado a tool/models.json")
