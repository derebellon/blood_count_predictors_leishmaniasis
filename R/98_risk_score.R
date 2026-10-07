######################################################################
## 98_risk_score.R  -  Score predictivo de FALLA facil de implementar
## Convierte los modelos multivariados (pre y EoTx) en un puntaje ENTERO
## (estilo Framingham/Sullivan): puntos_i = round(beta_i / beta_ref).
## Reporta punto de corte optimo por Youden, AUC (IC), sens/espec/VPP/VPN.
## Predictores dicotomizados (implementables al lado de la cama).
## OUTPUT: outputs/98_risk_score/{score_points_pre,score_points_eotx,perf}.csv
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(mice); library(cutpointr); library(pROC); library(dplyr) })
dest<-out_dir("98_risk_score")

cp<-readRDS("outputs/21_logistic_multivariate/RR_models.rds")$cutpoints
cut_ig<-cp$punto_corte[cp$variable=="Granulocitos_pre"]; cut_plaq<-cp$punto_corte[cp$variable=="Plaquetas_pre"]
cut_eos<-cp$punto_corte[cp$variable=="pct_eosino_post"]; cut_mono<-cp$punto_corte[cp$variable=="monocyte_ratio"]

d_pre <-complete(readRDS(file.path(DERIVED_DIR,"mids_pre_full.rds")),1)
d_post<-complete(readRDS(file.path(DERIVED_DIR,"mids_post_full.rds")),1)
d_pre$fail01<-as.integer(d_pre$outcome=="failure"); d_post$fail01<-as.integer(d_post$outcome=="failure")
d_post$monocyte_ratio<-d_post$Monocitos_post/d_post$Monocitos_pre

## punto de corte de EDAD (joven = mas riesgo) por Youden
cut_age<-cutpointr(d_pre,edad,fail01,pos_class=1,neg_class=0,method=maximize_metric,metric=youden,direction="<=",silent=TRUE)$optimal_cutpoint[1]

bin_pre<-function(d) data.frame(
  male            = as.integer(d$sexo=="Male"),
  young           = as.integer(d$edad<=cut_age),
  lymphadenopathy = as.integer(d$adenopatia_evalbas=="Yes"),
  coinfection     = as.integer(d$infeccion_concom_evalbas=="Yes"),
  low_IG          = as.integer(d$Granulocitos_pre<=cut_ig),
  low_platelets   = as.integer(d$Plaquetas_pre<=cut_plaq),
  fail01          = d$fail01)
bin_post<-function(d){ b<-data.frame(
  male            = as.integer(d$sexo=="Male"),
  young           = as.integer(d$edad<=cut_age),
  lymphadenopathy = as.integer(d$adenopatia_evalbas=="Yes"),
  coinfection_eot = as.integer(d$infeccion_concom_ftto=="Yes"),
  high_eos        = as.integer(d$pct_eosino_post>=cut_eos),
  mono_rise       = as.integer(d$monocyte_ratio>=cut_mono),
  fail01          = d$fail01); b }

make_score<-function(bin,etq){
  preds<-setdiff(names(bin),"fail01")
  f<-glm(as.formula(paste("fail01~",paste(preds,collapse="+"))),data=bin,family=binomial)
  beta<-coef(f)[preds]; beta[is.na(beta)]<-0
  ref<-min(beta[beta>0.01]); if(!is.finite(ref)) ref<-max(abs(beta))
  pts<-round(beta/ref); pts[pts<0]<-0
  score<-as.integer(as.matrix(bin[,preds]) %*% pts)
  r<-pROC::roc(bin$fail01,score,quiet=TRUE,levels=c(0,1),direction="<"); a<-as.numeric(pROC::auc(r)); ci<-as.numeric(pROC::ci.auc(r))
  co<-cutpointr(data.frame(s=score,y=bin$fail01),s,y,pos_class=1,neg_class=0,method=maximize_metric,metric=youden,direction=">=",silent=TRUE)
  thr<-co$optimal_cutpoint[1]; cls<-as.integer(score>=thr)
  tp<-sum(cls==1&bin$fail01==1);fp<-sum(cls==1&bin$fail01==0);fn<-sum(cls==0&bin$fail01==1);tn<-sum(cls==0&bin$fail01==0)
  perf<-data.frame(Model=etq,AUC=round(a,3),AUC_lo=round(ci[1],3),AUC_hi=round(ci[3],3),
    Youden_cutoff=thr,Max_score=sum(pts),
    Sens_fail=round(tp/(tp+fn),3),Spec_fail=round(tn/(tn+fp),3),
    PPV=round(tp/(tp+fp),3),NPV=round(tn/(tn+fn),3))
  tabla<-data.frame(Variable=preds,Points=as.integer(pts))
  list(points=tabla,perf=perf,score=score,y=bin$fail01)
}

S_pre <-make_score(bin_pre(d_pre),  "Pre-treatment score")
S_post<-make_score(bin_post(d_post),"End-of-treatment score")

lab<-c(male="Male sex",young=sprintf("Age <= %g years",cut_age),lymphadenopathy="Regional lymphadenopathy",
  coinfection="Concomitant infection (pre-Tx)",coinfection_eot="Concomitant infection (EoTx)",
  low_IG="Immature granulocytes <= 0.01 x10^3/uL",low_platelets=sprintf("Platelets <= %g x10^3/uL",cut_plaq),
  high_eos="Eosinophils >= 14%",mono_rise="Monocyte ratio (EoTx/pre) >= 1.07")
relab<-function(t){t$Variable<-ifelse(t$Variable%in%names(lab),lab[t$Variable],t$Variable);t}
write.csv(relab(S_pre$points), file.path(dest,"score_points_pre.csv"),row.names=FALSE)
write.csv(relab(S_post$points),file.path(dest,"score_points_eotx.csv"),row.names=FALSE)
write.csv(rbind(S_pre$perf,S_post$perf),file.path(dest,"score_performance.csv"),row.names=FALSE)

message("[98] SCORE PRE  - puntos:"); print(relab(S_pre$points),row.names=FALSE)
message("[98] SCORE EoTx - puntos:"); print(relab(S_post$points),row.names=FALSE)
message("[98] Desempeno:"); print(rbind(S_pre$perf,S_post$perf),row.names=FALSE)
message("[98] DONE.")
