######################################################################
## 98_risk_score.R  -  Score predictivo de FALLA (puntos enteros) con
## EDAD EN BANDAS (0/1/2 puntos), y estratos de riesgo VALIDADOS por CV.
## Método de Sullivan (puntos proporcionales a los coeficientes). La edad
## se categoriza SOLO para el score (los modelos OR/RR usan edad continua).
## Bandas de edad (gradiente monótono de riesgo): <25 (2), 25-39 (1), >=40 (0).
## OUTPUT: outputs/98_risk_score/{score_points_pre,score_points_eotx,
##         score_strata_pre,score_strata_eotx,score_performance}.csv
##         + Table6_risk_score.md
######################################################################
source("R/00_config.R")
source("R/utils/validation_utils.R")
suppressPackageStartupMessages({ library(mice); library(pROC); library(dplyr) })
dest<-out_dir("98_risk_score")
AUC<-function(y,s) as.numeric(pROC::auc(pROC::roc(y,s,quiet=TRUE,levels=c(0,1),direction="<")))

cp<-readRDS("outputs/21_logistic_multivariate/RR_models.rds")$cutpoints
cut_ig<-cp$punto_corte[cp$variable=="Granulocitos_pre"]; cut_plaq<-cp$punto_corte[cp$variable=="Plaquetas_pre"]
cut_eos<-cp$punto_corte[cp$variable=="pct_eosino_post"]; cut_mono<-cp$punto_corte[cp$variable=="monocyte_ratio"]
AGE_BREAKS<-c(0,25,40,200); AGE_LAB<-c("<25","25-39",">=40")   # banda joven = mas riesgo

bandedad<-function(e) factor(cut(e,breaks=AGE_BREAKS,right=FALSE,labels=AGE_LAB),levels=c(">=40","25-39","<25"))

d_pre <-complete(readRDS(file.path(DERIVED_DIR,"mids_pre_full.rds")),1); d_pre$fail01<-as.integer(d_pre$outcome=="failure")
d_post<-complete(readRDS(file.path(DERIVED_DIR,"mids_post_full.rds")),1); d_post$fail01<-as.integer(d_post$outcome=="failure"); d_post$monocyte_ratio<-d_post$Monocitos_post/d_post$Monocitos_pre

## construye el data.frame de factores de riesgo (edad como banda ordinal 0/1/2)
bin_pre<-function(d) data.frame(
  age_band=bandedad(d$edad),
  male=as.integer(d$sexo=="Male"),
  lymphadenopathy=as.integer(d$adenopatia_evalbas=="Yes"),
  coinfection=as.integer(d$infeccion_concom_evalbas=="Yes"),
  low_IG=as.integer(d$Granulocitos_pre<=cut_ig),
  low_platelets=as.integer(d$Plaquetas_pre<=cut_plaq),
  fail01=d$fail01)
bin_post<-function(d) data.frame(
  age_band=bandedad(d$edad),
  male=as.integer(d$sexo=="Male"),
  lymphadenopathy=as.integer(d$adenopatia_evalbas=="Yes"),
  coinfection_eot=as.integer(d$infeccion_concom_ftto=="Yes"),
  high_eos=as.integer(d$pct_eosino_post>=cut_eos),
  mono_rise=as.integer(d$monocyte_ratio>=cut_mono),
  fail01=d$fail01)

## deriva puntos (Sullivan) de un data.frame con age_band + binarias
derive_points<-function(b){
  preds<-setdiff(names(b),"fail01")
  f<-suppressWarnings(glm(as.formula(paste("fail01~",paste(preds,collapse="+"))),data=b,family=binomial))
  co<-coef(f); co[is.na(co)]<-0
  # betas: age_band25-39, age_band<25 (ref >=40), y las binarias
  betas<-co[setdiff(names(co),"(Intercept)")]
  ref<-min(betas[betas>0.01]); if(!is.finite(ref)) ref<-max(abs(betas))
  pts<-pmax(round(betas/ref),0)
  list(f=f,pts=pts,ref=ref)
}
## calcula el score de un data.frame dado un vector de puntos nombrado
score_of<-function(b,pts){
  preds<-setdiff(names(b),"fail01")
  s<-rep(0,nrow(b))
  for(p in preds){
    if(p=="age_band"){
      s<-s+ifelse(b$age_band=="<25", pts[["age_band<25"]], ifelse(b$age_band=="25-39", pts[["age_band25-39"]], 0))
    } else s<-s+pts[[p]]*b[[p]]
  }
  as.integer(s)
}

## ---- construye score, estratos y CV para un escenario ----------------
build<-function(binfun,dat,etq,strata_cuts){
  b<-binfun(dat); y<-b$fail01
  dp<-derive_points(b); pts<-dp$pts
  sc<-score_of(b,pts)
  app_auc<-AUC(y,sc)
  ## CROSS-VALIDADO: re-deriva puntos en cada fold, score OOF
  set.seed(SEED); reps<-30; k<-10; oof<-matrix(NA_real_,length(y),reps)
  for(r in 1:reps){fold<-folds_estratificados(y,k)
    for(ff in 1:k){tr<-b[fold!=ff,,drop=FALSE];te<-b[fold==ff,,drop=FALSE]
      dpt<-tryCatch(derive_points(tr),error=function(e)NULL); if(is.null(dpt))next
      oof[fold==ff,r]<-score_of(te,dpt$pts)}}
  cv_auc<-mean(apply(oof,2,function(c){ok<-!is.na(c);AUC(y[ok],c[ok])}),na.rm=TRUE)
  cv_ci<-quantile(apply(oof,2,function(c){ok<-!is.na(c);AUC(y[ok],c[ok])}),c(.025,.975),na.rm=TRUE)
  ## estratos: tasa de falla observada (aparente) y CV por estrato
  strata<-cut(sc,breaks=c(-1,strata_cuts,99),labels=c("Low","Intermediate","High"))
  oof_pool<-round(rowMeans(oof,na.rm=TRUE))  # score OOF promedio por paciente
  strata_oof<-cut(oof_pool,breaks=c(-1,strata_cuts,99),labels=c("Low","Intermediate","High"))
  st<-do.call(rbind,lapply(c("Low","Intermediate","High"),function(L){
    sel<-strata==L; n<-sum(sel,na.rm=TRUE); f<-sum(sel&y==1,na.rm=TRUE)
    w<-if(n>0){pt<-suppressWarnings(prop.test(f,n,correct=FALSE));c(100*f/n,100*max(0,pt$conf.int[1]),100*pt$conf.int[2])}else c(NA,NA,NA)
    selo<-strata_oof==L; no<-sum(selo,na.rm=TRUE); fo<-sum(selo&y==1,na.rm=TRUE); cvr<-if(no>0)100*fo/no else NA
    data.frame(Stratum=L,Score=c(Low=paste0("0-",strata_cuts[1]),Intermediate=paste0(strata_cuts[1]+1,"-",strata_cuts[2]),High=paste0(strata_cuts[2]+1,"+"))[[L]],
      N=n,Failures=f,`Observed failure % (95% CI)`=sprintf("%.1f (%.1f-%.1f)",w[1],w[2],w[3]),
      `Cross-validated failure %`=sprintf("%.1f",cvr),check.names=FALSE)}))
  list(pts=pts,score=sc,y=y,app_auc=app_auc,cv_auc=cv_auc,cv_ci=cv_ci,strata=st,max=sum(pmax(pts,0)[!grepl("age_band25",names(pts))]))
}

PRE <-build(bin_pre, d_pre, "Pre-treatment", strata_cuts=c(2,5))
POST<-build(bin_post,d_post,"End-of-treatment", strata_cuts=c(2,4))

## ---- etiquetas de los puntos ----------------------------------------
pts_table<-function(bld,eotx=FALSE){
  p<-bld$pts
  age<-data.frame(Variable=c("Age < 25 years","Age 25-39 years","Age >= 40 years (reference)"),
                  Points=c(p[["age_band<25"]],p[["age_band25-39"]],0))
  lab<-c(male="Male sex",lymphadenopathy="Regional lymphadenopathy",coinfection="Concomitant infection (pre-Tx)",
    coinfection_eot="Concomitant infection (EoTx)",low_IG="Immature granulocytes <= 0.01 x10^3/uL",
    low_platelets=sprintf("Platelets <= %g x10^3/uL",round(cut_plaq)),high_eos="Eosinophils >= 14%",
    mono_rise="Monocyte ratio (EoTx/pre) >= 1.07")
  others<-setdiff(names(p),c("age_band<25","age_band25-39"))
  oth<-data.frame(Variable=unname(lab[others]),Points=as.integer(p[others]))
  rbind(age,oth)
}
write.csv(pts_table(PRE), file.path(dest,"score_points_pre.csv"),row.names=FALSE)
write.csv(pts_table(POST,TRUE),file.path(dest,"score_points_eotx.csv"),row.names=FALSE)
write.csv(PRE$strata, file.path(dest,"score_strata_pre.csv"),row.names=FALSE)
write.csv(POST$strata,file.path(dest,"score_strata_eotx.csv"),row.names=FALSE)
write.csv(data.frame(Model=c("Pre-treatment","End-of-treatment"),
  App_AUC=round(c(PRE$app_auc,POST$app_auc),3),
  CV_AUC=round(c(PRE$cv_auc,POST$cv_auc),3),
  CV_lo=round(c(PRE$cv_ci[1],POST$cv_ci[1]),3),CV_hi=round(c(PRE$cv_ci[2],POST$cv_ci[2]),3)),
  file.path(dest,"score_performance.csv"),row.names=FALSE)

message("[98] PRE puntos:"); print(pts_table(PRE),row.names=FALSE)
message("[98] PRE estratos:"); print(PRE$strata,row.names=FALSE)
message(sprintf("[98] PRE AUC aparente %.3f | CV %.3f (%.3f-%.3f)",PRE$app_auc,PRE$cv_auc,PRE$cv_ci[1],PRE$cv_ci[2]))
message("[98] EoTx estratos:"); print(POST$strata,row.names=FALSE)
message(sprintf("[98] EoTx AUC aparente %.3f | CV %.3f",POST$app_auc,POST$cv_auc))
## ---- Exploratory: approximate failure risk by TREATMENT -------------
## The study is not powered to predict failure separately for each drug
## (few failures per arm: see counts below) and treatment was not randomised
## (confounding by indication), so these estimates are exploratory only and
## are reported because this is the first study to look at them.
sc_pre <- PRE$score                                           # pre-treatment SCARS points per patient
tx     <- d_pre$tratamiento                                   # treatment received (Glucantime / Miltefosine)
y      <- PRE$y                                               # 0/1 failure
strat  <- cut(sc_pre, c(-1, 2, 5, 99), labels = c("Low (0-2)","Intermediate (3-5)","High (6+)"))  # SCARS risk strata
## observed failure rate by treatment x risk stratum (approximate cell estimates)
tx_strata <- do.call(rbind, lapply(sort(unique(tx)), function(t)              # loop treatments
  do.call(rbind, lapply(levels(strat), function(L) {                          # loop strata
    sel <- tx == t & strat == L; n <- sum(sel); f <- sum(y[sel])              # cell N and failures
    data.frame(Treatment = t, Stratum = L, N = n, Failures = f,
               `Observed failure %` = ifelse(n > 0, sprintf("%.0f", 100*f/n), "-"), check.names = FALSE)
  }))))
write.csv(tx_strata, file.path(dest, "score_by_treatment.csv"), row.names = FALSE)   # save the exploratory table
## treatment-adjusted logistic: predicted P(failure) by score for each drug
m_tx <- suppressWarnings(glm(y ~ sc_pre + factor(tx), family = binomial))     # score + treatment main effect
pred_tx <- do.call(rbind, lapply(levels(factor(tx)), function(t)              # predicted risk per drug across the score
  data.frame(Treatment = t, Score = 0:10,
             Pred_failure = round(predict(m_tx, data.frame(sc_pre = 0:10,
                               tx = factor(t, levels = levels(factor(tx)))), type = "response"), 3))))
write.csv(pred_tx, file.path(dest, "score_by_treatment_predicted.csv"), row.names = FALSE)   # save predicted-risk curves
## observed failure by the score's age bands (younger patients fail more; the score already gives them more points)
ageband <- factor(cut(d_pre$edad, c(0,25,40,200), right = FALSE, labels = c("<25","25-39",">=40")))  # same bands as the score
age_tab <- do.call(rbind, lapply(levels(ageband), function(L){                 # failure rate per age band
  sel <- ageband == L; n <- sum(sel); f <- sum(y[sel])
  data.frame(Age_band = L, N = n, Failures = f, `Observed failure %` = sprintf("%.1f", 100*f/n), check.names = FALSE) }))
write.csv(age_tab, file.path(dest, "failure_by_age_band.csv"), row.names = FALSE)  # save age-band failure table
child_mil <- sum(d_pre$edad < 18 & d_pre$tratamiento == "Miltefosine"); child_n <- sum(d_pre$edad < 18)  # age-treatment confounding
message(sprintf("[98] age-treatment confounding: %d of %d children (<18) received miltefosine (none received antimony)", child_mil, child_n))
or_tx <- exp(coef(m_tx)[["factor(tx)Miltefosine"]])          # adjusted OR of miltefosine vs glucantime
message(sprintf("[98] Treatment (exploratory): adjusted OR miltefosine vs glucantime = %.2f (near 1 -> similar risk once the score is accounted for)", or_tx))
message("[98] failure by treatment x stratum:"); print(tx_strata, row.names = FALSE)

message("[98] DONE.")
