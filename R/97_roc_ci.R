######################################################################
## 97_roc_ci.R  -  Curvas ROC con INTERVALO DE CONFIANZA (banda bootstrap)
## (1) 2 paneles (pre / EoTx): clinico vs clinico+hemograma, con banda IC.
## (2) Las 5 curvas en UNA sola grafica (pre clin/+hemo, EoTx clin/+hemo,
##     combinada pre+EoTx), color=set, tipo de linea=momento, con IC.
## Paleta: clinico=#2D3A47 (slate) ; +hemograma/combinada=#B46A72 (rosa)
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
specs<-seq(0,1,by=0.02)
roc_band<-function(o,etq){
  set.seed(SEED); r<-pROC::roc(o$y,o$p,quiet=TRUE,levels=c(0,1),direction="<")
  ci<-pROC::ci.se(r,specificities=specs,boot.n=500,progress="none")
  a<-as.numeric(pROC::auc(r)); ca<-as.numeric(pROC::ci.auc(r))
  data.frame(fpr=1-specs, lo=ci[,1], mid=ci[,2], hi=ci[,3],
             Curve=sprintf("%s (AUC %.2f, 95%% CI %.2f-%.2f)",etq,a,ca[1],ca[3]))
}

message("[97] OOF + ROC/CI ...")
o_pc<-oof_logit(datos$pre, clin_pre);                                   o_pb<-oof_logit(datos$pre, c(clin_pre,"Granulocitos_pre","Plaquetas_pre"))
o_ec<-oof_logit(datos$post,clin_post);                                  o_eb<-oof_logit(datos$post,c(clin_post,"pct_eosino_post","mono_ratio"))
o_cmb<-oof_logit(datos$post,c(clin_post,"Granulocitos_pre","Plaquetas_pre","pct_eosino_post","mono_ratio"))

## ---- (1) 2 paneles con banda IC -------------------------------------
panel_ci<-function(dc,db,titulo){
  d<-rbind(roc_band(dc,"Clinical only"),roc_band(db,"Clinical + blood count"))
  d$Curve<-factor(d$Curve,levels=unique(d$Curve))
  ggplot(d,aes(fpr,mid,color=Curve,fill=Curve))+
    geom_abline(slope=1,intercept=0,linetype="dashed",color="grey60")+
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.18,color=NA)+geom_line(linewidth=1.05)+
    scale_color_manual(values=c(SLATE,ROSE),name=NULL)+scale_fill_manual(values=c(SLATE,ROSE),name=NULL)+
    coord_equal()+labs(title=titulo,x="1 - specificity",y="Sensitivity")+
    theme_grey(base_size=12)+theme(legend.position=c(0.97,0.03),legend.justification=c(1,0),
      legend.background=element_rect(fill=scales::alpha("white",0.7),color=NA),plot.title=element_text(face="bold"))
}
F1<-panel_ci(o_pc,o_pb,"A. Pre-treatment")+panel_ci(o_ec,o_eb,"B. End of treatment")
ggsave(file.path(destf,"Figure4_ROC_withCI.png"),F1,width=11,height=5.3,dpi=300)
message("[97] ROC con IC (2 paneles) ok")

## ---- (2) Las 5 curvas en una sola ------------------------------------
lab5<-function(o,etq){b<-roc_band(o,etq); b}
pc<-lab5(o_pc,"Pre, clinical only"); pb<-lab5(o_pb,"Pre, clinical + blood")
ec<-lab5(o_ec,"EoTx, clinical only"); eb<-lab5(o_eb,"EoTx, clinical + blood"); cm<-lab5(o_cmb,"Combined pre+EoTx")
D<-rbind(pc,pb,ec,eb,cm); D$Curve<-factor(D$Curve,levels=unique(D$Curve))
## color = set (slate clinico / rosa +hemograma) ; linetype = momento
col5<-c(SLATE,ROSE,SLATE,ROSE,ROSE); lty5<-c("solid","solid","dashed","dashed","dotted")
names(col5)<-levels(D$Curve); names(lty5)<-levels(D$Curve)
F2<-ggplot(D,aes(fpr,mid,color=Curve,linetype=Curve,fill=Curve))+
  geom_abline(slope=1,intercept=0,linetype="dashed",color="grey60")+
  geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.08,color=NA)+geom_line(linewidth=1.05)+
  scale_color_manual(values=col5,name=NULL)+scale_fill_manual(values=col5,guide="none")+
  scale_linetype_manual(values=lty5,name=NULL)+coord_equal()+
  labs(title="ROC curves - parsimonious logistic model (out-of-sample, 95% CI bands)",
       x="1 - specificity",y="Sensitivity",
       caption="Colour = predictor set (slate = clinical only; rose = clinical + blood count).  Line type = moment (solid = pre-Tx; dashed = EoTx; dotted = combined pre+EoTx).")+
  theme_grey(base_size=12)+theme(legend.position=c(0.985,0.02),legend.justification=c(1,0),
    legend.background=element_rect(fill=scales::alpha("white",0.75),color=NA),
    plot.title=element_text(face="bold",size=12),plot.caption=element_text(size=8,color="grey30"))
ggsave(file.path(destf,"Figure_ROC_combined5.png"),F2,width=8.6,height=7.4,dpi=300)
message("[97] ROC combinada (5 curvas) ok")
message("[97] DONE.")
