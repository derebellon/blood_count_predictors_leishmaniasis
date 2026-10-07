######################################################################
## 94_figures_v2.R  -  Refinamiento de figuras (pedidos de D. Rebellon)
## - SHAP (F5): etiquetas legibles + momento de medicion explicito
## - F7: reestructurada y clara (dinamica vs punto unico); boxplot y violin+puntos
## - Comparacion de PALETAS de color para elegir
## OUTPUT: figures/paper/Figure5_SHAP_ml.png (mejorada),
##         figures/paper/Figure7_boxplot.png, Figure7_violin.png,
##         figures/paper/palette_options.png
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({ library(ggplot2); library(patchwork); library(dplyr) })
destf <- file.path("figures","paper"); if(!dir.exists(destf)) dir.create(destf,recursive=TRUE)

## ---------- Etiquetador legible de features del hemograma --------------
pretty_feat <- function(x){
  cell <- c(Neutrofilos="Neutrophils",Linfocitos="Lymphocytes",Monocitos="Monocytes",
            Eosinofilos="Eosinophils",Basofilos="Basophils",Granulocitos="Immature granulocytes",
            Globulos_rojos="Red blood cells",Hemoglobina="Haemoglobin",Hematocrito="Haematocrit",
            Plaquetas="Platelets")
  rc <- c(neu="neutrophil",eos="eosinophil",mono="monocyte",baso="basophil",linfo="lymphocyte",
          lin="lymphocyte",granu="IG",monoeosneu="(mono+eos+neu)")
  pc <- c(neutro="Neutrophil",linfo="Lymphocyte",mono="Monocyte",eosino="Eosinophil",baso="Basophil")
  one <- function(f){
    if(f=="edad") return("Age")
    if(f=="imc") return("Body mass index")
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
    if(base%in%names(cell)) return(paste0(cell[base],mom))
    f
  }
  vapply(x, one, character(1))
}

## ---------- PALETAS disponibles (Cura vs Falla) ------------------------
paletas <- list(
  "Clinical blue/red" = c(Cure="#2E86C1", Failure="#C0392B"),
  "Okabe-Ito"         = c(Cure="#0072B2", Failure="#D55E00"),
  "NEJM style"        = c(Cure="#0072B5", Failure="#BC3C29"),
  "Teal/orange"       = c(Cure="#1B9E77", Failure="#D95F02"),
  "Slate/coral"       = c(Cure="#4C6A92", Failure="#E07A5F")
)
tema <- theme_bw(base_size=12)+theme(panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold",size=12),
        strip.background=element_rect(fill="grey92",color=NA), legend.position="top")

## ---------- F5: SHAP con etiquetas legibles ---------------------------
shap_panel <- function(csv, titulo, topn=10, pal=paletas[["Clinical blue/red"]]){
  s<-read.csv(csv,check.names=TRUE)
  s$dir<-ifelse(grepl("^\\+",s$direction),"Higher → more failure","Higher → less failure")
  s<-s[order(-s$mean_abs_shap),][seq_len(min(topn,nrow(s))),]
  s$lab<-pretty_feat(s$feature); s$lab<-factor(s$lab, levels=rev(s$lab))
  ggplot(s,aes(x=mean_abs_shap,y=lab,fill=dir))+geom_col(width=0.72)+
    scale_fill_manual(values=c("Higher → more failure"=unname(pal["Failure"]),
                               "Higher → less failure"=unname(pal["Cure"])),name=NULL)+
    labs(title=titulo,x="Mean absolute SHAP value (impact on predicted failure)",y=NULL)+tema
}
F5 <- (shap_panel("outputs/70_interpretability_shap/rf_shap_importance_pre.csv","A. Random forest - Pre-treatment") +
       shap_panel("outputs/70_interpretability_shap/rf_shap_importance_eotx.csv","B. Random forest - End of treatment")) /
      (shap_panel("outputs/70_interpretability_shap/shap_importance_pre.csv","C. XGBoost - Pre-treatment") +
       shap_panel("outputs/70_interpretability_shap/shap_importance_eotx.csv","D. XGBoost - End of treatment")) +
      plot_annotation(caption="Pre-Tx = before treatment; EoTx = end of treatment; Δ = change (EoTx minus pre-treatment). Ratios are cell-count ratios.")
ggsave(file.path(destf,"Figure5_SHAP_ml.png"),F5,width=13,height=9.2,dpi=300)
message("[94] F5 ok")

## ---------- Datos para F7 ---------------------------------------------
d<-read.csv(ANALYSIS_CSV,check.names=TRUE)
d$Outcome<-factor(ifelse(d$estado_final=="Therapeutic failure","Failure","Cure"),levels=c("Cure","Failure"))
tot<-function(s) d[[paste0("Neutrofilos_",s)]]+d[[paste0("Linfocitos_",s)]]+d[[paste0("Monocitos_",s)]]+d[[paste0("Eosinofilos_",s)]]+d[[paste0("Basofilos_",s)]]
d$eos_pct_pre<-100*d$Eosinofilos_pre/tot("pre"); d$eos_pct_post<-100*d$Eosinofilos_post/tot("post")
d$mono_ratio<-d$Monocitos_post/pmax(d$Monocitos_pre,0.01)
## Dinamica (pre vs EoTx): eosinofilo % e IG  (los que tienen las dos mediciones)
dyn<-rbind(
  data.frame(Outcome=d$Outcome,Moment="Pre-Tx", val=d$eos_pct_pre, Param="Eosinophils (%)"),
  data.frame(Outcome=d$Outcome,Moment="EoTx",   val=d$eos_pct_post,Param="Eosinophils (%)"),
  data.frame(Outcome=d$Outcome,Moment="Pre-Tx", val=d$Granulocitos_pre, Param="Immature granulocytes (x10^3/uL)"),
  data.frame(Outcome=d$Outcome,Moment="EoTx",   val=d$Granulocitos_post,Param="Immature granulocytes (x10^3/uL)"))
dyn$Moment<-factor(dyn$Moment,levels=c("Pre-Tx","EoTx"))
## Punto unico (por desenlace): razon de monocitos (EoTx/pre) y plaquetas pre
sng<-rbind(
  data.frame(Outcome=d$Outcome,val=d$mono_ratio,  Param="Monocyte ratio (EoTx/pre)"),
  data.frame(Outcome=d$Outcome,val=d$Plaquetas_pre,Param="Platelets, pre-Tx (x10^3/uL)"))

make_F7 <- function(estilo, pal){
  geomd <- if(estilo=="violin")
    list(geom_violin(trim=FALSE,alpha=0.5,width=0.8),
         geom_boxplot(width=0.14,outlier.shape=NA,alpha=0.9),
         geom_jitter(width=0.08,size=0.5,alpha=0.35))
  else list(geom_boxplot(outlier.size=0.6,width=0.6))
  p1<-ggplot(dyn[is.finite(dyn$val),],aes(Moment,val,fill=Outcome))+geomd+
    scale_fill_manual(values=pal,name=NULL)+facet_wrap(~Param,scales="free_y")+
    labs(title="A. Dynamics pre-treatment vs end of treatment",x=NULL,y=NULL)+tema
  p2<-ggplot(sng[is.finite(sng$val),],aes(Outcome,val,fill=Outcome))+geomd+
    scale_fill_manual(values=pal,guide="none")+facet_wrap(~Param,scales="free_y")+
    labs(title="B. Single-timepoint biomarkers (no paired follow-up measurement)",x=NULL,y=NULL)+tema
  (p1/p2)+plot_layout(heights=c(1,1))+
    plot_annotation(caption="Platelets (pre-Tx only) and the monocyte ratio (a single EoTx/pre value) have no paired pre-vs-EoTx series, so they are shown by outcome in panel B.")
}
ggsave(file.path(destf,"Figure7_boxplot.png"), make_F7("box",   paletas[["Clinical blue/red"]]), width=11,height=8,dpi=300)
ggsave(file.path(destf,"Figure7_violin.png"),  make_F7("violin",paletas[["Clinical blue/red"]]), width=11,height=8,dpi=300)
message("[94] F7 (box+violin) ok")

## ---------- Comparacion de PALETAS ------------------------------------
demo<-dyn[dyn$Param=="Eosinophils (%)" & is.finite(dyn$val),]
plist<-lapply(names(paletas),function(nm){
  ggplot(demo,aes(Moment,val,fill=Outcome))+geom_boxplot(outlier.size=0.5,width=0.6)+
    scale_fill_manual(values=paletas[[nm]],name=NULL)+
    labs(title=nm,x=NULL,y="Eosinophils (%)")+tema+theme(legend.position="bottom")
})
Fpal<-patchwork::wrap_plots(plist,ncol=3)+plot_annotation(title="Palette options (Cure vs Failure) - example on eosinophil %")
ggsave(file.path(destf,"palette_options.png"),Fpal,width=13,height=8,dpi=300)
message("[94] palettes ok")
message("[94] DONE.")
