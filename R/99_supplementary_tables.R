######################################################################
## 99_supplementary_tables.R  -  Tablas SUPLEMENTARIAS del paper
## S1: incidencia (acumulada) de falla por momento del seguimiento, IC95% (Wilson)
## S2: caracteristicas del hemograma (conteos, %, ratios clave) por desenlace + total
## S4: puntos de corte (Youden) de los parametros seleccionados
## S5: asociaciones de los parametros DICOTOMIZADOS con falla (RR univariado)
## OUTPUT: outputs/99_supplementary/S*.md  (+ Supplementary_tables.md combinado)
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(dplyr); library(gtsummary); library(knitr) })
dest<-out_dir("99_supplementary")
d<-read.csv(ANALYSIS_CSV,check.names=TRUE)
d$Outcome<-ifelse(d$estado_final=="Therapeutic failure","Failure","Cure")

## ---- S1: incidencia acumulada de falla por momento ------------------
isTF<-function(x) !is.na(x) & x=="Therapeutic failure"
cum_eot <- isTF(d$estado_fin_tto)
cum_w8  <- cum_eot | isTF(d$estado_sem8)
cum_w13 <- cum_w8  | isTF(d$estado_sem13)
cum_w26 <- cum_w13 | isTF(d$estado_sem26) | isTF(d$estado_final)
N<-nrow(d)
wilson<-function(k,n){ pt<-prop.test(k,n,correct=FALSE); c(est=100*k/n, lo=100*pt$conf.int[1], hi=100*pt$conf.int[2]) }
rows<-list(
  c("End of treatment", sum(cum_eot)),
  c("Week 8",  sum(cum_w8)),
  c("Week 13", sum(cum_w13)),
  c("Week 26 (final)", sum(cum_w26)))
s1<-do.call(rbind,lapply(rows,function(r){k<-as.integer(r[2]);w<-wilson(k,N)
  data.frame(Timepoint=r[1],Failures=k,N=N,`Cumulative incidence (%)`=sprintf("%.2f (%.2f-%.2f)",w["est"],w["lo"],w["hi"]),check.names=FALSE)}))
writeLines(c("## Table S1. Cumulative incidence of therapeutic failure by follow-up timepoint\n",
  "Cumulative proportion of the cohort (N=200) identified as therapeutic failure by each timepoint; 95% CI by the Wilson (score) method.\n",
  knitr::kable(s1,format="pipe",row.names=FALSE)), file.path(dest,"S1.md"))
message("[99] S1 ok")

## ---- S2: hemograma por desenlace + total ----------------------------
pct<-function(s) {tot<-d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]; tot}
d$`Eosinophils % (pre-Tx)`<-100*d$Eosinofilos_pre/pct("pre")
d$`Eosinophils % (EoTx)`<-100*d$Eosinofilos_post/pct("post")
d$`Monocyte ratio (EoTx/pre)`<-d$Monocitos_post/pmax(d$Monocitos_pre,0.01)
s2df<-d[,c("Outcome",
  "Neutrofilos_pre","Linfocitos_pre","Monocitos_pre","Eosinofilos_pre","Basofilos_pre","Granulocitos_pre","Plaquetas_pre","Hemoglobina_pre","Hematocrito_pre",
  "Neutrofilos_post","Linfocitos_post","Monocitos_post","Eosinofilos_post","Granulocitos_post",
  "Eosinophils % (pre-Tx)","Eosinophils % (EoTx)","Monocyte ratio (EoTx/pre)")]
lab2<-list(
 Neutrofilos_pre~"Neutrophils, pre-Tx (x10^3/uL)",Linfocitos_pre~"Lymphocytes, pre-Tx",Monocitos_pre~"Monocytes, pre-Tx",
 Eosinofilos_pre~"Eosinophils, pre-Tx",Basofilos_pre~"Basophils, pre-Tx",Granulocitos_pre~"Immature granulocytes, pre-Tx",
 Plaquetas_pre~"Platelets, pre-Tx",Hemoglobina_pre~"Haemoglobin, pre-Tx",Hematocrito_pre~"Haematocrit, pre-Tx",
 Neutrofilos_post~"Neutrophils, EoTx",Linfocitos_post~"Lymphocytes, EoTx",Monocitos_post~"Monocytes, EoTx",
 Eosinofilos_post~"Eosinophils, EoTx",Granulocitos_post~"Immature granulocytes, EoTx")
tb2<-tryCatch(as.character(gtsummary::as_kable(
  gtsummary::tbl_summary(s2df,by=Outcome,missing="no",label=lab2,
    statistic=list(gtsummary::all_continuous()~"{median} ({p25}, {p75})")) |>
    gtsummary::add_overall() |> gtsummary::add_p(), format="pipe")),
  error=function(e) paste("(S2 error:",conditionMessage(e),")"))
writeLines(c("## Table S2. Blood count characteristics by therapeutic outcome\n",
  "Median (IQR). Counts in x10^3 cells/uL unless stated. P-values by Wilcoxon rank-sum.\n",tb2),
  file.path(dest,"S2.md"))
message("[99] S2 ok")

## ---- S4: puntos de corte --------------------------------------------
cp<-readRDS("outputs/21_logistic_multivariate/RR_models.rds")$cutpoints
s4<-data.frame(Parameter=cp$parametro,Direction=cp$direccion,`Cut-off`=round(cp$punto_corte,2),check.names=FALSE)
writeLines(c("## Table S4. Youden-derived cut-off points for the selected blood count parameters\n",
  knitr::kable(s4,format="pipe",row.names=FALSE)),file.path(dest,"S4.md"))
message("[99] S4 ok")

## ---- S5: asociaciones de los parametros DICOTOMIZADOS (RR univariado) --
suppressPackageStartupMessages({ library(mice); library(sandwich) })
cpm<-readRDS("outputs/21_logistic_multivariate/RR_models.rds")$cutpoints
ci<-cpm$punto_corte[cpm$variable=="Granulocitos_pre"]; cpp<-cpm$punto_corte[cpm$variable=="Plaquetas_pre"]
ce<-cpm$punto_corte[cpm$variable=="pct_eosino_post"]; cm<-cpm$punto_corte[cpm$variable=="monocyte_ratio"]
dp<-complete(readRDS(file.path(DERIVED_DIR,"mids_pre_full.rds")),1); dp$fail01<-as.integer(dp$outcome=="failure")
do_<-complete(readRDS(file.path(DERIVED_DIR,"mids_post_full.rds")),1); do_$fail01<-as.integer(do_$outcome=="failure"); do_$mr<-do_$Monocitos_post/do_$Monocitos_pre
rr1<-function(x,y){f<-glm(y~x,family=poisson(link="log"));V<-sandwich::vcovHC(f,type="HC0");b<-coef(f)[2];se<-sqrt(diag(V))[2]
  sprintf("%.2f (%.2f-%.2f)",exp(b),exp(b-1.96*se),exp(b+1.96*se))}
s5<-data.frame(
  Parameter=c("Immature granulocytes <= 0.01 x10^3/uL (pre-Tx)","Platelets <= 269 x10^3/uL (pre-Tx)","Eosinophils >= 14% (EoTx)","Monocyte ratio (EoTx/pre) >= 1.07"),
  `Unadjusted RR (95% CI)`=c(
    rr1(as.integer(dp$Granulocitos_pre<=ci),dp$fail01),
    rr1(as.integer(dp$Plaquetas_pre<=cpp),dp$fail01),
    rr1(as.integer(do_$pct_eosino_post>=ce),do_$fail01),
    rr1(as.integer(do_$mr>=cm),do_$fail01)),check.names=FALSE)
writeLines(c("## Table S5. Univariate associations of the dichotomised blood count parameters with therapeutic failure\n",
  "Unadjusted relative risks (robust Poisson) for each parameter categorised at its Youden cut-off.\n",
  knitr::kable(s5,format="pipe",row.names=FALSE)),file.path(dest,"S5.md"))
message("[99] S5 ok")

## ---- S6: random forest por familias ---------------------------------
rff<-read.csv("outputs/34_rf_families/rf_families.csv",check.names=TRUE)
rff<-rff[,c("Scenario","ROC_AUC","PR_AUC","Top5_importance")]
writeLines(c("## Table S6. Random forest fitted to separate parameter families (counts, percentages, ratios)\n",
  "Out-of-sample ROC-AUC and PR-AUC (repeated cross-validation) and the top-5 variables by permutation importance.\n",
  knitr::kable(rff,format="pipe",row.names=FALSE)),file.path(dest,"S6.md"))
message("[99] S6 ok")

## ---- combinar -------------------------------------------------------
writeLines(c(readLines(file.path(dest,"S1.md")),"\n---\n",readLines(file.path(dest,"S2.md")),
  "\n---\n",readLines(file.path(dest,"S4.md")),"\n---\n",readLines(file.path(dest,"S5.md")),
  "\n---\n",readLines(file.path(dest,"S6.md"))),file.path(dest,"Supplementary_tables.md"))
message("[99] DONE.")
