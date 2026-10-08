######################################################################
## 93_paper_tables.R  -  Tablas del paper (T1-T4) en markdown
## T1: basales por desenlace + columna Total (gtsummary add_overall)
## T2: OR univariado (por DE) de clinicas + hemograma (gtsummary ya hechos)
## T3: multivariado 3 columnas (clinico / +Pre-Tx / +EoTx), RR ajustado
## T4: regresion LASSO (variables seleccionadas + coeficientes, pre y post)
## OUTPUT: outputs/93_paper_tables/T1..T4.md  (+ Tables_T1_T4.md combinado)
######################################################################
source("R/00_config.R")
suppressPackageStartupMessages({
  library(mice); library(sandwich); library(dplyr); library(gtsummary); library(glmnet); library(knitr)
})
source("R/utils/ml_data.R")
dest <- out_dir("93_paper_tables")

## etiquetador legible para los terminos del LASSO (hemograma + clinicas)
pretty_lasso <- function(x){
  cell<-c(Neutrofilos="Neutrophils",Linfocitos="Lymphocytes",Monocitos="Monocytes",Eosinofilos="Eosinophils",
          Basofilos="Basophils",Granulocitos="Immature granulocytes",Globulos_rojos="Red blood cells",
          Hemoglobina="Haemoglobin",Hematocrito="Haematocrit",Plaquetas="Platelets")
  rc<-c(neu="neutrophil",eos="eosinophil",mono="monocyte",baso="basophil",linfo="lymphocyte",lin="lymphocyte",granu="IG",monoeosneu="(mono+eos+neu)")
  pc<-c(neutro="Neutrophil",linfo="Lymphocyte",mono="Monocyte",eosino="Eosinophil",baso="Basophil")
  fixed<-c(edad="Age",imc="Body mass index",sexoMale="Male sex",mono_ratio="Monocyte ratio (EoTx/pre)",
    adenopatia_evalbasYes="Lymphadenopathy (pre-Tx)",infeccion_concom_evalbasYes="Concomitant infection (pre-Tx)",
    infeccion_concom_fttoYes="Concomitant infection (EoTx)",tratamientoMiltefosine="Miltefosine (vs Glucantime)",
    comorbilidadesYes="Comorbidities",
    sexo="Sex",etnia="Ethnicity",comorbilidades="Comorbidities",especie_corta="Leishmania species",
    tratamiento="Treatment",numero_lesiones="Number of lesions",lesiones_categorica="Number of lesions",
    tiempo_evolucion_dicotomica="Time of lesion evolution",tiempo_sintom_semanas="Time of evolution (weeks)",
    tipo_lesion_eval_base_corta="Lesion type (baseline)",adenopatia_evalbas="Regional lymphadenopathy",
    infeccion_concom_evalbas="Concomitant infection (pre-Tx)",infeccion_concom_ftto="Concomitant infection (EoTx)",
    peso="Weight",talla="Height",antecedente_leish="Prior leishmaniasis")
  one<-function(f){
    if(f %in% names(fixed)) return(fixed[[f]])
    if(grepl("^var_",f)){cn<-sub("^var_","",f);nm<-ifelse(cn%in%names(cell),cell[[cn]],cn);return(paste0("Δ ",nm," (EoTx-pre)"))}
    if(grepl("^(etnia|especie_corta|variacion_lesion_post|tipo_lesion|rango_dosis|lesiones_categorica|tiempo_evolucion)",f))
      return(gsub("_"," ",f))
    mom<-if(grepl("_pre$",f))" (pre-Tx)" else if(grepl("_post$",f))" (EoTx)" else ""
    base<-sub("_(pre|post)$","",f)
    if(grepl("^pct_",base)){k<-sub("^pct_","",base);nm<-ifelse(k%in%names(pc),pc[[k]],k);return(paste0(nm," %",mom))}
    if(grepl("^i_",base)){cc<-strsplit(sub("^i_","",base),"_")[[1]];a<-ifelse(cc[1]%in%names(rc),rc[[cc[1]]],cc[1]);b<-ifelse(cc[2]%in%names(rc),rc[[cc[2]]],cc[2]);lab<-paste0(a,"/",b," ratio");substr(lab,1,1)<-toupper(substr(lab,1,1));return(paste0(lab,mom))}
    if(base%in%names(cell)) return(paste0(cell[[base]],mom))
    f
  }
  vapply(x,one,character(1))
}

## ============ T3: maquinaria RR (reusa 21) ============================
rrmods <- readRDS("outputs/21_logistic_multivariate/RR_models.rds")
cp <- rrmods$cutpoints
cut_ig  <- cp$punto_corte[cp$variable=="Granulocitos_pre"]
cut_plaq<- cp$punto_corte[cp$variable=="Plaquetas_pre"]
cut_eos <- cp$punto_corte[cp$variable=="pct_eosino_post"]
cut_mono<- cp$punto_corte[cp$variable=="monocyte_ratio"]
preparar_dic <- function(d){
  d$fail01<-as.integer(d$outcome=="failure"); d$edad_c<-as.numeric(scale(d$edad,scale=FALSE))
  d$ig_bajo<-factor(ifelse(d$Granulocitos_pre<=cut_ig,"Low (<=cut)","High"),levels=c("High","Low (<=cut)"))
  d$plaq_bajo<-factor(ifelse(d$Plaquetas_pre<=cut_plaq,"Low (<=cut)","High"),levels=c("High","Low (<=cut)"))
  if("pct_eosino_post"%in%names(d)){
    d$eos_alto<-factor(ifelse(d$pct_eosino_post>=cut_eos,">=cut","<cut"),levels=c("<cut",">=cut"))
    d$mono_sube<-factor(ifelse((d$Monocitos_post/d$Monocitos_pre)>=cut_mono,">=cut","<cut"),levels=c("<cut",">=cut"))
  }
  d
}
rr_mi <- function(mids_obj, formula_str){
  m<-mids_obj$m
  ests<-lapply(seq_len(m),function(i){d<-preparar_dic(complete(mids_obj,i))
    f<-glm(as.formula(formula_str),data=d,family=poisson(link="log"))
    list(b=coef(f),V=sandwich::vcovHC(f,type="HC0"))})
  bmat<-sapply(ests,`[[`,"b"); if(is.null(dim(bmat))) bmat<-matrix(bmat,nrow=1,dimnames=list(names(ests[[1]]$b),NULL))
  Qbar<-rowMeans(bmat); Ubar<-Reduce(`+`,lapply(ests,`[[`,"V"))/m
  B<-if(m>1) stats::cov(t(bmat)) else diag(0,length(Qbar))
  Tvar<-diag(Ubar)+(1+1/m)*diag(B); SE<-sqrt(Tvar); z<-Qbar/SE
  data.frame(term=names(Qbar),cell=sprintf("%.2f (%.2f-%.2f)",exp(Qbar),exp(Qbar-1.96*SE),exp(Qbar+1.96*SE)),
             p=2*pnorm(-abs(z)),row.names=NULL)
}
mids_pre<-readRDS(file.path(DERIVED_DIR,"mids_pre_full.rds"))
mids_post<-readRDS(file.path(DERIVED_DIR,"mids_post_full.rds"))
f_clin<-"fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas"
f_pre <-"fail01 ~ sexo + edad_c + adenopatia_evalbas + infeccion_concom_evalbas + ig_bajo + plaq_bajo"
f_post<-"fail01 ~ sexo + edad_c + tratamiento + infeccion_concom_ftto + eos_alto + mono_sube + adenopatia_evalbas"
A<-rr_mi(mids_pre,f_clin); B<-rr_mi(mids_pre,f_pre); C<-rr_mi(mids_post,f_post)
lab<-c("sexoMale"="Male sex","edad_c"="Age (per year)","adenopatia_evalbasYes"="Lymphadenopathy (baseline)",
  "infeccion_concom_evalbasYes"="Concomitant infection (baseline)","infeccion_concom_fttoYes"="Concomitant infection (EoTx)",
  "ig_bajoLow (<=cut)"="Immature granulocytes <=0.01 (pre)","plaq_bajoLow (<=cut)"="Low platelets (pre)",
  "eos_alto>=cut"="Eosinophils >=14% (EoTx)","mono_sube>=cut"="Monocyte ratio >=1.07 (EoTx)",
  "tratamientoMiltefosine"="Miltefosine (vs Glucantime)")
fmt<-function(df){df<-df[df$term!="(Intercept)",]; df$lab<-ifelse(df$term%in%names(lab),lab[df$term],df$term); df}
A<-fmt(A);B<-fmt(B);C<-fmt(C)
rows<-unique(c(A$lab,B$lab,C$lab))
g<-function(df,l){v<-df$cell[df$lab==l]; if(length(v)) v else "-"}
t3<-data.frame(Variable=rows,
  `Clinical only`=sapply(rows,function(l)g(A,l)),
  `+ Pre-Tx blood count`=sapply(rows,function(l)g(B,l)),
  `+ EoTx blood count`=sapply(rows,function(l)g(C,l)),check.names=FALSE)
t3md<-c("## Table 3. Multivariate models of therapeutic failure (adjusted RR, robust Poisson, multiple imputation)\n",
  "Adjusted risk ratios (95% CI). Column A = clinical variables only; B = clinical + Pre-Tx blood counts; C = clinical + EoTx blood counts.\n",
  knitr::kable(t3,format="pipe",row.names=FALSE))
writeLines(t3md, file.path(dest,"T3.md"))
message("[93] T3 ok")

## ============ T2: OR univariado (gtsummary ya hechos) =================
uni<-readRDS("outputs/20_logistic_univariate/univariate_OR.rds")
t2md<-c("## Table 2. Univariate associations with therapeutic failure (OR per 1-SD, logistic, multiple imputation)\n")
relabel_md<-function(k){
  clin<-"\\b(edad|sexo|etnia|comorbilidades|especie_corta|tratamiento|numero_lesiones|lesiones_categorica|tiempo_evolucion_dicotomica|tiempo_sintom_semanas|tipo_lesion_eval_base_corta|adenopatia_evalbas|infeccion_concom_evalbas|infeccion_concom_ftto|peso|talla|antecedente_leish|imc)\\b"
  pat<-paste0("i_[a-z0-9]+_[a-z0-9]+_(pre|post)|pct_[a-z]+_(pre|post)|var_[A-Za-z]+|(Neutrofilos|Linfocitos|Monocitos|Eosinofilos|Basofilos|Granulocitos|Globulos_rojos|Hemoglobina|Hematocrito|Plaquetas)_(pre|post)|mono_ratio|",clin)
  toks<-unique(unlist(regmatches(k,gregexpr(pat,k))))
  if(length(toks)){toks<-toks[order(-nchar(toks))]; for(t in toks) k<-gsub(t,pretty_lasso(t),k,fixed=TRUE)}
  k
}
render<-function(tb,sub){
  k<-tryCatch(as.character(gtsummary::as_kable(tb,format="pipe")),error=function(e)NULL)
  if(is.null(k)) k<-tryCatch({tt<-gtsummary::as_tibble(tb); knitr::kable(tt,format="pipe")},error=function(e)"(no renderizable)")
  k<-relabel_md(k)
  c(paste0("\n**",sub,"**\n"),k)
}
t2md<-c(t2md,
  render(uni$clinical,"Clinical variables"),
  render(uni$hemogram_pre,"Pre-treatment blood count"),
  render(uni$hemogram_post,"End-of-treatment blood count"),
  render(uni$variation,"Pre-to-EoTx variation"))
writeLines(t2md, file.path(dest,"T2.md"))
message("[93] T2 ok")

## ============ T1: basales por desenlace + Total =======================
d<-read.csv(ANALYSIS_CSV,check.names=TRUE)
d$Outcome<-ifelse(d$estado_final=="Therapeutic failure","Failure","Cure")
vars1<-intersect(c("edad","sexo","etnia","imc","tiempo_evolucion_dicotomica","lesiones_categorica",
  "tipo_lesion_eval_base_corta","adenopatia_evalbas","infeccion_concom_evalbas","comorbilidades",
  "especie_corta","tratamiento"),names(d))
lab1<-list(edad~"Age (years)",sexo~"Sex",etnia~"Ethnicity",imc~"Body mass index (kg/m2)",
  tiempo_evolucion_dicotomica~"Time of lesion evolution",lesiones_categorica~"Number of lesions",
  tipo_lesion_eval_base_corta~"Lesion type (baseline)",adenopatia_evalbas~"Regional lymphadenopathy",
  infeccion_concom_evalbas~"Concomitant infection (baseline)",comorbilidades~"Comorbidities",
  especie_corta~"Leishmania species",tratamiento~"Treatment")
lab1<-lab1[sapply(lab1,function(f) as.character(f[[2]])%in%vars1)]
t1<-tryCatch({
  tb<-gtsummary::tbl_summary(d[,c("Outcome",vars1)],by=Outcome,missing="no",label=lab1) |>
      gtsummary::add_overall() |> gtsummary::add_p()
  as.character(gtsummary::as_kable(tb,format="pipe"))
},error=function(e) paste("(T1 error:",conditionMessage(e),")"))
writeLines(c("## Table 1. Baseline characteristics by therapeutic outcome (with overall column)\n",t1),
           file.path(dest,"T1.md"))
message("[93] T1 ok")

## ============ T4: LASSO (variables seleccionadas + coef) ==============
datos<-cargar_datos_ml(); pr<-datos$predictores
imp_all<-function(df,vars){for(v in vars){if(is.numeric(df[[v]]))df[[v]][is.na(df[[v]])]<-median(df[[v]],na.rm=TRUE)
  else{m<-names(sort(table(df[[v]]),decreasing=TRUE))[1];df[[v]][is.na(df[[v]])]<-m}};df}
lasso_coef<-function(df,vars,etq){
  df<-imp_all(df,vars); for(v in vars) if(is.character(df[[v]])) df[[v]]<-factor(df[[v]])
  X<-model.matrix(as.formula(paste("~",paste(vars,collapse="+"))),df)[,-1,drop=FALSE]; y<-df$fail01
  set.seed(SEED); cv<-glmnet::cv.glmnet(X,y,family="binomial",alpha=1,nfolds=10,type.measure="auc",maxit=1e6)
  ## lambda.1se = parsimonioso y estable (evita la solucion degenerada de lambda.min
  ## cuando hay muchas features correlacionadas y pocos eventos).
  co<-as.matrix(coef(cv,s="lambda.1se")); co<-data.frame(term=rownames(co),coef=round(co[,1],3))
  co<-co[co$coef!=0 & co$term!="(Intercept)",]; co<-co[order(-abs(co$coef)),]
  if(nrow(co)==0){ co<-as.matrix(coef(cv,s="lambda.min")); co<-data.frame(term=rownames(co),coef=round(co[,1],3))
    co<-co[co$coef!=0 & co$term!="(Intercept)",]; co<-co[order(-abs(co$coef)),] }
  if(nrow(co)==0) co<-data.frame(term="(none selected)",coef=NA)
  co<-head(co,15)
  co$term<-pretty_lasso(co$term)
  names(co)<-c("Variable","Coefficient")
  c(paste0("\n**",etq,"** (lambda.1se)\n"),knitr::kable(co,format="pipe",row.names=FALSE))
}
t4md<-c("## Table 4. LASSO regression: variables retained and coefficients (log-odds of failure)\n",
  "Penalised logistic regression (alpha=1), lambda.1se by 10-fold CV maximising AUC (parsimonious, stable solution). Features with non-zero coefficients.\n",
  lasso_coef(datos$pre, c(pr$clinicas,pr$hemo_pre),"Pre-treatment (clinical + blood count)"),
  lasso_coef(datos$post,c(pr$clinicas,pr$clinicas_post,pr$hemo_pre,pr$hemo_post),"End of treatment (clinical + blood count)"))
writeLines(t4md, file.path(dest,"T4.md"))
message("[93] T4 ok")

## ============ combinar ================================================
all<-c(readLines(file.path(dest,"T1.md")),"\n---\n",readLines(file.path(dest,"T2.md")),"\n---\n",
       readLines(file.path(dest,"T3.md")),"\n---\n",readLines(file.path(dest,"T4.md")))
writeLines(all, file.path(dest,"Tables_T1_T4.md"))
message("[93] DONE. Tablas en ", dest)
