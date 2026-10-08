######################################################################
## 9A_figures_suppl.R  -  Figura 2 (distribuciones + ROC por parametro de
## corte) y Figura S2 (colinealidad de los parametros Pre-Tx).
## Paleta: cura=#2D3A47 (slate), falla=#B46A72 (rosa). Tema gris.
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(pROC); library(dplyr) })
destf<-file.path("figures","paper"); if(!dir.exists(destf)) dir.create(destf,recursive=TRUE)
SLATE<-"#2D3A47"; ROSE<-"#B46A72"
tema<-theme_grey(base_size=12)+theme(plot.title=element_text(face="bold",size=11),strip.background=element_rect(fill="grey85",color=NA),strip.text=element_text(face="bold"),legend.position="top")

d<-read.csv(ANALYSIS_CSV,check.names=TRUE)
d$Outcome<-factor(ifelse(d$estado_final=="Therapeutic failure","Failure","Cure"),levels=c("Cure","Failure"))
tot<-function(s) d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]
d$eos_post<-100*d$Eosinofilos_post/tot("post"); d$mr<-d$Monocitos_post/pmax(d$Monocitos_pre,0.01); d$y<-as.integer(d$Outcome=="Failure")

params<-list(
  list(v="Granulocitos_pre",lab="Immature granulocytes (pre-Tx)"),
  list(v="Plaquetas_pre",    lab="Platelets (pre-Tx)"),
  list(v="eos_post",         lab="Eosinophils % (EoTx)"),
  list(v="mr",               lab="Monocyte ratio (EoTx/pre)"))

## Panel A: distribuciones (boxplot + jitter) por desenlace
boxdf<-do.call(rbind,lapply(params,function(p) data.frame(Outcome=d$Outcome,val=d[[p$v]],Param=p$lab)))
boxdf<-boxdf[is.finite(boxdf$val),]
pA<-ggplot(boxdf,aes(Outcome,val,fill=Outcome))+geom_boxplot(outlier.shape=NA,alpha=0.9,width=0.6,color="grey20")+
  geom_jitter(aes(color=Outcome),width=0.12,size=0.6,alpha=0.35)+
  scale_fill_manual(values=c(Cure=SLATE,Failure=ROSE),guide="none")+scale_color_manual(values=c(Cure=SLATE,Failure=ROSE),guide="none")+
  facet_wrap(~Param,scales="free_y",nrow=1)+labs(title="A. Distribution of the selected parameters by outcome",x=NULL,y=NULL)+tema

## Panel B: curvas ROC individuales con AUC
rocdf<-do.call(rbind,lapply(params,function(p){ok<-is.finite(d[[p$v]])
  r<-pROC::roc(d$y[ok],d[[p$v]][ok],quiet=TRUE); a<-as.numeric(pROC::auc(r))
  data.frame(fpr=1-r$specificities,tpr=r$sensitivities,Param=sprintf("%s (AUC %.2f)",p$lab,a))}))
pB<-ggplot(rocdf,aes(fpr,tpr,color=Param))+geom_abline(slope=1,intercept=0,linetype="dashed",color="grey60")+
  geom_path(linewidth=0.9)+coord_equal()+scale_color_manual(values=c("#2D3A47","#B46A72","#7E9AA6","#8C5A61"),name=NULL)+
  labs(title="B. ROC of each selected parameter",x="1 - specificity",y="Sensitivity")+tema+
  theme(legend.position=c(0.98,0.02),legend.justification=c(1,0),legend.text=element_text(size=8),legend.background=element_rect(fill=scales::alpha("white",0.7),color=NA))
F2<-(pA/pB)+plot_layout(heights=c(1,1.3))
ggsave(file.path(destf,"Figure2_param_distributions_ROC.png"),F2,width=11,height=8.5,dpi=300)
message("[9A] Figure 2 ok")

## Figura S2: colinealidad (correlacion) de los parametros Pre-Tx
prevars<-c("Neutrofilos_pre","Linfocitos_pre","Monocitos_pre","Eosinofilos_pre","Basofilos_pre","Granulocitos_pre","Plaquetas_pre","Hemoglobina_pre","Hematocrito_pre")
lab<-c("Neutrophils","Lymphocytes","Monocytes","Eosinophils","Basophils","Immature granulocytes","Platelets","Haemoglobin","Haematocrit")
M<-cor(d[,prevars],use="pairwise.complete.obs",method="spearman"); dimnames(M)<-list(lab,lab)
cd<-as.data.frame(as.table(M)); names(cd)<-c("x","y","r")
cd$x<-factor(cd$x,levels=lab); cd$y<-factor(cd$y,levels=rev(lab))
FS2<-ggplot(cd,aes(x,y,fill=r))+geom_tile(color="white")+geom_text(aes(label=sprintf("%.2f",r)),size=2.7,color="grey15")+
  scale_fill_gradient2(low="#2D3A47",mid="white",high="#B46A72",midpoint=0,limits=c(-1,1),name="Spearman")+
  labs(title="Figure S2. Correlation (collinearity) among Pre-Tx blood count parameters",x=NULL,y=NULL)+
  theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=45,hjust=1),plot.title=element_text(face="bold",size=11))
ggsave(file.path(destf,"FigureS2_collinearity.png"),FS2,width=8.5,height=7,dpi=300)
message("[9A] Figure S2 ok")
message("[9A] DONE.")
