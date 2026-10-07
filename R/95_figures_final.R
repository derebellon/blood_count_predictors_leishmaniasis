######################################################################
## 95_figures_final.R  -  TODAS las figuras del paper con el acabado y la
## paleta pedidos por D. Rebellon:
##   - Cura = #2D3A47 (slate) ; Falla/riesgo = #B46A72 (rosa)
##   - Boxplot + puntos jittered (NO violin), tema gris (estilo de referencia)
##   - Orden de modelos: logistica, LASSO, random forest, XGBoost, superlearner
## Regenera F3-F7 de forma consistente.
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(dplyr) })
destf <- file.path("figures","paper"); if(!dir.exists(destf)) dir.create(destf,recursive=TRUE)

SLATE <- "#2D3A47"; ROSE <- "#B46A72"
PAL <- c(Cure=SLATE, Failure=ROSE)
## tema estilo referencia (panel gris, grilla blanca)
tema <- theme_grey(base_size=12)+
  theme(plot.title=element_text(face="bold",size=12),
        plot.caption=element_text(size=8,color="grey30"),
        strip.background=element_rect(fill="grey85",color=NA),
        strip.text=element_text(face="bold"),
        legend.position="top")
orden_modelos <- c("Parsimonious logistic","LASSO","Random Forest","XGBoost","SuperLearner")

## ---------- etiquetador legible (hemograma + momento) -----------------
pretty_feat <- function(x){
  cell <- c(Neutrofilos="Neutrophils",Linfocitos="Lymphocytes",Monocitos="Monocytes",
            Eosinofilos="Eosinophils",Basofilos="Basophils",Granulocitos="Immature granulocytes",
            Globulos_rojos="Red blood cells",Hemoglobina="Haemoglobin",Hematocrito="Haematocrit",Plaquetas="Platelets")
  rc <- c(neu="neutrophil",eos="eosinophil",mono="monocyte",baso="basophil",linfo="lymphocyte",
          lin="lymphocyte",granu="IG",monoeosneu="(mono+eos+neu)")
  pc <- c(neutro="Neutrophil",linfo="Lymphocyte",mono="Monocyte",eosino="Eosinophil",baso="Basophil")
  one<-function(f){
    if(f=="edad") return("Age"); if(f=="imc") return("Body mass index")
    if(f=="mono_ratio") return("Monocyte ratio (EoTx/pre)")
    if(grepl("^sexo",f)) return("Male sex")
    if(grepl("^infeccion_concom_evalbas",f)) return("Concomitant infection (pre-Tx)")
    if(grepl("^infeccion_concom_ftto",f)) return("Concomitant infection (EoTx)")
    if(grepl("^adenopatia",f)) return("Lymphadenopathy (pre-Tx)")
    if(grepl("^var_",f)){ cn<-sub("^var_","",f); nm<-ifelse(cn%in%names(cell),cell[cn],cn); return(paste0("Δ ",nm," (EoTx−pre)")) }
    mom <- if(grepl("_pre$",f)) " (pre-Tx)" else if(grepl("_post$",f)) " (EoTx)" else ""
    base <- sub("_(pre|post)$","",f)
    if(grepl("^pct_",base)){ k<-sub("^pct_","",base); nm<-ifelse(k%in%names(pc),pc[k],k); return(paste0(nm," %",mom)) }
    if(grepl("^i_",base)){ comp<-strsplit(sub("^i_","",base),"_")[[1]]
      a<-ifelse(comp[1]%in%names(rc),rc[comp[1]],comp[1]); b<-ifelse(comp[2]%in%names(rc),rc[comp[2]],comp[2])
      lab<-paste0(a,"/",b," ratio"); substr(lab,1,1)<-toupper(substr(lab,1,1)); return(paste0(lab,mom)) }
    if(base%in%names(cell)) return(paste0(cell[base],mom)); f
  }
  vapply(x,one,character(1))
}

## ==================== F3: AUC de todos los modelos ====================
mm <- read.csv("outputs/91_master_metrics/master_metrics.csv",check.names=TRUE)
f3_panel <- function(scn,titulo){
  d<-mm[mm$Scenario==scn,]; d<-d[match(orden_modelos,d$Model),]
  d$Model<-factor(d$Model,levels=rev(orden_modelos))
  ggplot(d,aes(ROC_AUC,Model))+
    geom_errorbarh(aes(xmin=AUC_lo,xmax=AUC_hi),height=0.18,color="grey30")+
    geom_point(size=3.1,color=ROSE)+
    geom_text(aes(label=sprintf("%.2f",ROC_AUC)),vjust=-0.9,size=3.3,color="grey20")+
    scale_x_continuous(limits=c(0.45,0.9),breaks=seq(0.5,0.9,0.1))+
    geom_vline(xintercept=0.5,linetype="dashed",color="grey50")+
    labs(title=titulo,x="Out-of-sample ROC-AUC (95% CI)",y=NULL)+tema
}
F3<-f3_panel("PRE clinical + blood count","A. Pre-treatment (clinical + blood count)")+
    f3_panel("EoTx clinical + blood count","B. End of treatment (clinical + blood count)")
ggsave(file.path(destf,"Figure3_AUC_all_models.png"),F3,width=11,height=4.2,dpi=300)
message("[95] F3 ok")

## ==================== F4: valor incremental ===========================
lg<-mm[mm$Model=="Parsimonious logistic",]
pick<-function(s){r<-lg[lg$Scenario==s,];c(r$ROC_AUC,r$AUC_lo,r$AUC_hi)}
f4<-data.frame(Moment=c("Pre-treatment","Pre-treatment","End of treatment","End of treatment","Combined\nPre + EoTx"),
  Set=c("Clinical only","Clinical + blood count","Clinical only","Clinical + blood count","Clinical + blood count"),AUC=NA,lo=NA,hi=NA)
f4[1,3:5]<-pick("PRE clinical only");f4[2,3:5]<-pick("PRE clinical + blood count")
f4[3,3:5]<-pick("EoTx clinical only");f4[4,3:5]<-pick("EoTx clinical + blood count");f4[5,3:5]<-pick("Combined PRE+EoTx blood count")
f4$Moment<-factor(f4$Moment,levels=c("Pre-treatment","End of treatment","Combined\nPre + EoTx"))
f4$Set<-factor(f4$Set,levels=c("Clinical only","Clinical + blood count"))
F4<-ggplot(f4,aes(Moment,AUC,fill=Set))+
  geom_col(position=position_dodge(0.7),width=0.62,color="grey25")+
  geom_errorbar(aes(ymin=lo,ymax=hi),position=position_dodge(0.7),width=0.18)+
  geom_text(aes(label=sprintf("%.2f",AUC)),position=position_dodge(0.7),vjust=-0.6,size=3.3,color="grey20")+
  scale_fill_manual(values=c("Clinical only"=SLATE,"Clinical + blood count"=ROSE),name=NULL)+
  coord_cartesian(ylim=c(0.5,0.85))+
  labs(title="Incremental value of blood count parameters (parsimonious logistic model)",
       x=NULL,y="Out-of-sample ROC-AUC (95% CI)")+tema
ggsave(file.path(destf,"Figure4_incremental_value.png"),F4,width=9,height=5,dpi=300)
message("[95] F4 ok")

## ==================== F5: SHAP (RF antes de XGBoost) ==================
shap_panel<-function(csv,titulo,topn=10){
  s<-read.csv(csv,check.names=TRUE)
  s$dir<-ifelse(grepl("^\\+",s$direction),"Higher → more failure","Higher → less failure")
  s<-s[order(-s$mean_abs_shap),][seq_len(min(topn,nrow(s))),]
  s$lab<-pretty_feat(s$feature); s$lab<-factor(s$lab,levels=rev(s$lab))
  ggplot(s,aes(mean_abs_shap,lab,fill=dir))+geom_col(width=0.72)+
    scale_fill_manual(values=c("Higher → more failure"=ROSE,"Higher → less failure"=SLATE),name=NULL)+
    labs(title=titulo,x="Mean absolute SHAP value (impact on predicted failure)",y=NULL)+tema
}
F5<-(shap_panel("outputs/70_interpretability_shap/rf_shap_importance_pre.csv","A. Random forest - Pre-treatment")+
     shap_panel("outputs/70_interpretability_shap/rf_shap_importance_eotx.csv","B. Random forest - End of treatment"))/
    (shap_panel("outputs/70_interpretability_shap/shap_importance_pre.csv","C. XGBoost - Pre-treatment")+
     shap_panel("outputs/70_interpretability_shap/shap_importance_eotx.csv","D. XGBoost - End of treatment"))+
    plot_annotation(caption="Pre-Tx = before treatment; EoTx = end of treatment; Δ = change (EoTx minus pre-treatment). Ratios are cell-count ratios.")
ggsave(file.path(destf,"Figure5_SHAP_ml.png"),F5,width=13,height=9.2,dpi=300)
message("[95] F5 ok")

## ==================== F6: forest plot RR ==============================
rr<-readRDS("outputs/21_logistic_multivariate/RR_models.rds")
lab<-c("sexoMale"="Male sex","edad_c"="Age (per year)","adenopatia_evalbasYes"="Lymphadenopathy (baseline)",
  "infeccion_concom_evalbasYes"="Concomitant infection (baseline)","infeccion_concom_fttoYes"="Concomitant infection (EoTx)",
  "ig_bajoLow (<=cut)"="Immature granulocytes <=0.01 (pre)","plaq_bajoLow (<=cut)"="Low platelets (pre)",
  "eos_alto>=cut"="Eosinophils >=14% (EoTx)","mono_sube>=cut"="Monocyte ratio >=1.07 (EoTx)","tratamientoMiltefosine"="Miltefosine (vs Glucantime)")
prep<-function(df,m){df<-df[df$term!="(Intercept)",];df$lab<-ifelse(df$term%in%names(lab),lab[df$term],df$term);df$Moment<-m;df}
fdf<-rbind(prep(rr$pre,"Pre-treatment model"),prep(rr$eotx,"EoTx model"))
## orden pedido: Pre-treatment ARRIBA, EoTx ABAJO
fdf$Moment<-factor(fdf$Moment,levels=c("Pre-treatment model","EoTx model"))
fdf$lab<-factor(fdf$lab,levels=rev(unique(fdf$lab)))
F6<-ggplot(fdf,aes(RR,lab))+geom_vline(xintercept=1,linetype="dashed",color="grey50")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=0.2,color="grey30")+
  geom_point(aes(color=RR>1),size=3)+
  scale_color_manual(values=c("TRUE"=ROSE,"FALSE"=SLATE),guide="none")+
  scale_x_log10(breaks=c(0.25,0.5,1,2,4,8))+facet_wrap(~Moment,scales="free_y",ncol=1)+
  labs(title="Adjusted risk ratios for therapeutic failure (final multivariate models)",
       x="Adjusted RR (log scale, 95% CI)",y=NULL)+tema
ggsave(file.path(destf,"Figure6_forest_RR.png"),F6,width=9,height=6.5,dpi=300)
message("[95] F6 ok")

## ==================== F7: boxplot + puntos (acabado pedido) ===========
d<-read.csv(ANALYSIS_CSV,check.names=TRUE)
d$Outcome<-factor(ifelse(d$estado_final=="Therapeutic failure","Failure","Cure"),levels=c("Cure","Failure"))
tot<-function(s) d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]
d$eos_pct_pre<-100*d$Eosinofilos_pre/tot("pre"); d$eos_pct_post<-100*d$Eosinofilos_post/tot("post")
d$mono_ratio<-d$Monocitos_post/pmax(d$Monocitos_pre,0.01)
dyn<-rbind(
  data.frame(Outcome=d$Outcome,Moment="Pre-Tx",val=d$eos_pct_pre,Param="Eosinophils (%)"),
  data.frame(Outcome=d$Outcome,Moment="EoTx",  val=d$eos_pct_post,Param="Eosinophils (%)"),
  data.frame(Outcome=d$Outcome,Moment="Pre-Tx",val=d$Granulocitos_pre,Param="Immature granulocytes (x10^3/uL)"),
  data.frame(Outcome=d$Outcome,Moment="EoTx",  val=d$Granulocitos_post,Param="Immature granulocytes (x10^3/uL)"))
dyn$Moment<-factor(dyn$Moment,levels=c("Pre-Tx","EoTx"))
sng<-rbind(
  data.frame(Outcome=d$Outcome,val=d$mono_ratio,Param="Monocyte ratio (EoTx/pre)"),
  data.frame(Outcome=d$Outcome,val=d$Plaquetas_pre,Param="Platelets, pre-Tx (x10^3/uL)"))
pA<-ggplot(dyn[is.finite(dyn$val),],aes(Moment,val,fill=Outcome))+
  geom_boxplot(outlier.shape=NA,alpha=0.9,width=0.6,color="grey20",position=position_dodge(0.7))+
  geom_point(aes(color=Outcome),position=position_jitterdodge(jitter.width=0.18,dodge.width=0.7),size=0.7,alpha=0.35)+
  scale_fill_manual(values=PAL,name=NULL)+scale_color_manual(values=PAL,guide="none")+
  facet_wrap(~Param,scales="free_y")+labs(title="A. Dynamics pre-treatment vs end of treatment",x=NULL,y=NULL)+tema
pB<-ggplot(sng[is.finite(sng$val),],aes(Outcome,val,fill=Outcome))+
  geom_boxplot(outlier.shape=NA,alpha=0.9,width=0.55,color="grey20")+
  geom_jitter(aes(color=Outcome),width=0.12,size=0.7,alpha=0.35)+
  scale_fill_manual(values=PAL,guide="none")+scale_color_manual(values=PAL,guide="none")+
  facet_wrap(~Param,scales="free_y")+labs(title="B. Single-timepoint biomarkers (no paired follow-up measurement)",x=NULL,y=NULL)+tema
F7<-(pA/pB)+plot_annotation(caption="Platelets (pre-Tx only) and the monocyte ratio (a single EoTx/pre value) have no paired pre-vs-EoTx series, so they are shown by outcome in panel B.")
ggsave(file.path(destf,"Figure7_dynamics_by_outcome.png"),F7,width=11,height=8,dpi=300)
message("[95] F7 ok")
message("[95] DONE - todas las figuras con paleta slate/rosa + acabado boxplot+puntos.")
