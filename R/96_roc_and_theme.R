######################################################################
## 96_roc_and_theme.R
## (a) Version en CURVAS ROC del valor incremental (alternativa a las barras)
## (b) Comparacion de tema: gris (actual) vs theme_classic
## Paleta: cura/protector/clinico = #2D3A47 ; falla/riesgo/+hemograma = #B46A72
######################################################################
source("R/00_config.R")
source("R/utils/validation_utils.R")
source("R/utils/ml_data.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(pROC); library(dplyr) })
destf<-file.path("figures","paper"); if(!dir.exists(destf)) dir.create(destf,recursive=TRUE)
SLATE<-"#2D3A47"; ROSE<-"#B46A72"

datos<-cargar_datos_ml()
clin_pre<-c("edad","sexo","adenopatia_evalbas","infeccion_concom_evalbas")
clin_post<-c("edad","sexo","infeccion_concom_ftto","adenopatia_evalbas")

## ---- OOF prob promedio (logistica) por CV repetida --------------------
oof_logit<-function(df,vars,reps=10,k=10){
  set.seed(SEED); for(v in vars) if(is.character(df[[v]])) df[[v]]<-factor(df[[v]])
  form<-as.formula(paste("~",paste(vars,collapse="+"))); y<-df$fail01; acc<-matrix(NA_real_,nrow(df),reps)
  for(r in seq_len(reps)){ fold<-folds_estratificados(y,k)
    for(f in seq_len(k)){ tr<-df[fold!=f,,drop=FALSE]; te<-df[fold==f,,drop=FALSE]
      imp<-imputar_train(tr,te,vars); Xtr<-model.matrix(form,imp$train)[,-1,drop=FALSE]
      Xte<-model.matrix(form,imp$test)[,-1,drop=FALSE]; Xte<-Xte[,colnames(Xtr),drop=FALSE]
      dtr<-data.frame(y=imp$train$fail01,Xtr); m<-suppressWarnings(glm(y~.,data=dtr,family=binomial))
      acc[fold==f,r]<-as.numeric(predict(m,newdata=data.frame(Xte),type="response")) } }
  list(y=y,p=rowMeans(acc,na.rm=TRUE))
}
roc_df<-function(o,etq){ r<-pROC::roc(o$y,o$p,quiet=TRUE,levels=c(0,1),direction="<")
  data.frame(fpr=1-r$specificities,tpr=r$sensitivities,
             Set=sprintf("%s (AUC %.2f)",etq,as.numeric(pROC::auc(r)))) }

pre_c <-oof_logit(datos$pre, clin_pre);                              pre_b <-oof_logit(datos$pre, c(clin_pre,"Granulocitos_pre","Plaquetas_pre"))
post_c<-oof_logit(datos$post,clin_post);                            post_b<-oof_logit(datos$post,c(clin_post,"pct_eosino_post","mono_ratio"))

roc_panel<-function(dc,db,titulo){
  d<-rbind(roc_df(dc,"Clinical only"),roc_df(db,"Clinical + blood count"))
  d$Set<-factor(d$Set,levels=unique(d$Set))   # clinico-solo primero, +hemograma despues
  ggplot(d,aes(fpr,tpr,color=Set))+geom_abline(slope=1,intercept=0,linetype="dashed",color="grey60")+
    geom_path(linewidth=1.1)+coord_equal()+
    scale_color_manual(values=c(SLATE,ROSE),name=NULL)+
    labs(title=titulo,x="1 - specificity",y="Sensitivity")+
    theme_grey(base_size=12)+theme(legend.position=c(0.98,0.03),legend.justification=c(1,0),
      legend.background=element_rect(fill=scales::alpha("white",0.7),color=NA),plot.title=element_text(face="bold"))
}
Froc<-roc_panel(pre_c,pre_b,"A. Pre-treatment")+roc_panel(post_c,post_b,"B. End of treatment")
ggsave(file.path(destf,"Figure4alt_ROC_curves.png"),Froc,width=11,height=5.2,dpi=300)
message("[96] ROC curves ok")

## ---- Comparacion de TEMA: gris vs classic ----------------------------
mm<-read.csv("outputs/91_master_metrics/master_metrics.csv",check.names=TRUE)
lg<-mm[mm$Model=="Parsimonious logistic",]; pick<-function(s){r<-lg[lg$Scenario==s,];c(r$ROC_AUC,r$AUC_lo,r$AUC_hi)}
f4<-data.frame(Moment=factor(c("Pre-treatment","Pre-treatment","End of treatment","End of treatment"),levels=c("Pre-treatment","End of treatment")),
  Set=factor(c("Clinical only","Clinical + blood count","Clinical only","Clinical + blood count"),levels=c("Clinical only","Clinical + blood count")),AUC=NA,lo=NA,hi=NA)
f4[1,3:5]<-pick("PRE clinical only");f4[2,3:5]<-pick("PRE clinical + blood count");f4[3,3:5]<-pick("EoTx clinical only");f4[4,3:5]<-pick("EoTx clinical + blood count")
d<-read.csv(ANALYSIS_CSV,check.names=TRUE); d$Outcome<-factor(ifelse(d$estado_final=="Therapeutic failure","Failure","Cure"),levels=c("Cure","Failure"))
tot<-function(s) d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]
eos<-rbind(data.frame(Outcome=d$Outcome,Moment="Pre-Tx",val=100*d$Eosinofilos_pre/tot("pre")),
           data.frame(Outcome=d$Outcome,Moment="EoTx",  val=100*d$Eosinofilos_post/tot("post")))
eos$Moment<-factor(eos$Moment,levels=c("Pre-Tx","EoTx")); eos<-eos[is.finite(eos$val),]
bar_plot<-function(th) ggplot(f4,aes(Moment,AUC,fill=Set))+geom_col(position=position_dodge(0.7),width=0.6,color="grey25")+
  geom_errorbar(aes(ymin=lo,ymax=hi),position=position_dodge(0.7),width=0.15)+
  scale_fill_manual(values=c("Clinical only"=SLATE,"Clinical + blood count"=ROSE),name=NULL)+
  coord_cartesian(ylim=c(0.5,0.82))+labs(title="AUC by model",x=NULL,y="ROC-AUC")+th+theme(legend.position="top")
box_plot<-function(th) ggplot(eos,aes(Moment,val,fill=Outcome))+geom_boxplot(outlier.shape=NA,alpha=0.9,width=0.6,color="grey20",position=position_dodge(0.7))+
  geom_point(aes(color=Outcome),position=position_jitterdodge(jitter.width=0.18,dodge.width=0.7),size=0.6,alpha=0.35)+
  scale_fill_manual(values=c(Cure=SLATE,Failure=ROSE),name=NULL)+scale_color_manual(values=c(Cure=SLATE,Failure=ROSE),guide="none")+
  labs(title="Eosinophils (%) by outcome",x=NULL,y="Eosinophils (%)")+th+theme(legend.position="top")
g_grey<-theme_grey(base_size=11); g_cls<-theme_classic(base_size=11)
comp<-(bar_plot(g_grey)+box_plot(g_grey)+plot_annotation(title="theme_grey (current)")) /
      (bar_plot(g_cls)+box_plot(g_cls))
## patchwork no anida plot_annotation bien; armo 2x2 directo
comp<-(bar_plot(g_grey)+ggtitle("AUC - theme_grey (current)"))+(box_plot(g_grey)+ggtitle("Boxplot - theme_grey (current)"))+
      (bar_plot(g_cls)+ggtitle("AUC - theme_classic"))+(box_plot(g_cls)+ggtitle("Boxplot - theme_classic"))+
      plot_layout(ncol=2)
ggsave(file.path(destf,"theme_comparison.png"),comp,width=12,height=9,dpi=300)
message("[96] theme comparison ok")
message("[96] DONE.")
