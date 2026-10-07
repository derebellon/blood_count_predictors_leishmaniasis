######################################################################
## 34_rf_families.R  -  Random forest por FAMILIAS de variables
## Pedido de D. Rebellon: RF solo con CONTEOS vs solo RATIOS vs solo
## PROPORCIONES (pre y post), para ver si emerge un parametro que los
## otros metodos no habian seleccionado. Reporta AUC fuera de muestra +
## top importancia por familia.
## OUTPUT: outputs/34_rf_families/rf_families.csv  (+ top importancias)
######################################################################
source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(ranger); library(dplyr) })
dest <- out_dir("34_rf_families")

datos <- cargar_datos_ml()
clin_pre  <- c("edad","sexo","adenopatia_evalbas","infeccion_concom_evalbas")
clin_post <- c("edad","sexo","infeccion_concom_ftto","adenopatia_evalbas")

fam <- function(df, sufijo) {
  nm <- names(df)
  list(
    counts      = grep(paste0("(Neutrofilos|Linfocitos|Monocitos|Eosinofilos|Basofilos|Granulocitos|Globulos_rojos|Hemoglobina|Hematocrito|Plaquetas)_", sufijo, "$"), nm, value = TRUE),
    proportions = grep(paste0("^pct_.*_", sufijo, "$"), nm, value = TRUE),
    ratios      = grep(paste0("^i_.*_", sufijo, "$"), nm, value = TRUE)
  )
}
fit_rf <- function(X,y,w) ranger::ranger(x=as.data.frame(X),y=factor(y,levels=c(0,1)),probability=TRUE,
                                         num.trees=1000,min.node.size=10,respect.unordered.factors="order")
pred_rf<- function(m,X) predict(m,as.data.frame(X))$predictions[,"1"]

evalua <- function(df, clin, vars, etiqueta) {
  vv <- c(clin, vars)
  res <- evaluar_cv(df, vv, fit_rf, pred_rf, k=CV_FOLDS, repeticiones=10, balance="none")
  ## importancia (ranger) sobre el set imputado completo
  d2 <- df
  for (v in vv) if (is.numeric(d2[[v]])) d2[[v]][is.na(d2[[v]])] <- median(d2[[v]],na.rm=TRUE)
  for (v in vv) if (!is.numeric(d2[[v]])) { m<-names(sort(table(d2[[v]]),decreasing=TRUE))[1]; d2[[v]][is.na(d2[[v]])]<-m }
  rf <- ranger::ranger(fail01 ~ ., data=d2[,c("fail01",vv)], num.trees=1000, importance="permutation", probability=FALSE)
  imp <- sort(rf$variable.importance, decreasing=TRUE)
  top <- paste(names(head(imp,5)), collapse="; ")
  data.frame(Scenario=etiqueta, ROC_AUC=round(res$resumen["ROC_AUC","media"],3),
             PR_AUC=round(res$resumen["PR_AUC","media"],3), Top5_importance=top)
}

fpre  <- fam(datos$pre,  "pre")
fpost <- fam(datos$post, "post")
tabla <- rbind(
  evalua(datos$pre,  clin_pre,  fpre$counts,      "PRE counts only"),
  evalua(datos$pre,  clin_pre,  fpre$proportions, "PRE proportions only"),
  evalua(datos$pre,  clin_pre,  fpre$ratios,      "PRE ratios only"),
  evalua(datos$post, clin_post, fpost$counts,     "EoTx counts only"),
  evalua(datos$post, clin_post, fpost$proportions,"EoTx proportions only"),
  evalua(datos$post, clin_post, fpost$ratios,     "EoTx ratios only")
)
write.csv(tabla, file.path(dest,"rf_families.csv"), row.names=FALSE)
message("[34] RF por familias (AUC fuera de muestra + top importancia):")
for (i in seq_len(nrow(tabla))) message(sprintf("  %-24s AUC=%.3f PR=%.3f | %s",
   tabla$Scenario[i], tabla$ROC_AUC[i], tabla$PR_AUC[i], tabla$Top5_importance[i]))
message("[34] DONE.")
