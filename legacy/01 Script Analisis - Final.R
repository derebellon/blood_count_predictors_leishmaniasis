######################################################################
## RESEARCH PROJECT: "Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia"
## PHASE 1 ANALYSIS SCRIPT – MSc THESIS
## PRINCIPAL INVESTIGATOR & SCRIPT DEVELOPER: DAVID ESTEBAN REBELLON SANCHEZ
## PROJECT SUPERVISORS: MARIA ADELAIDA GOMEZ & LYDA OSORIO
## FINAL MANUSCRIPT CO-AUTHORS WHO APPROVED THE ANALYSIS: David E. Rebellón-Sánchez, Lina Giraldo-Parra, Jimena Jojoa, Jonny A. García-Luna, Lyda Osorio, María Adelaida Gómez
## SPONSORING INSTITUTION: CENTRO INTERNACIONAL DE ENTRENAMIENTO E INVESTIGACIONES MEDICAS (CIDEIM) AND UNIVERSIDAD DEL VALLE
######################################################################

#### SCRIPT FOR DESCRIPTIVE AND EXPLORATORY ANALYSES CONDUCTED DURING THE MSc THESIS
# INPUT: CLEANED DATABASES
# PROCESS: DESCRIPTIVE ANALYSIS, CUTOFF OPTIMIZATION, AND MULTIPLE REGRESSION APPROACHES (INCLUDING COMPLEMENTARY MODELS TO STATA OUTPUTS)
# OUTPUT: FIGURES, TABLES, AND PERFORMANCE METRICS — FULL REPORT STORED IN "Reports/Reporte_tesis_maestria_15Jul2022.pdf"

######################################################################
# NOTE: 
# As mentioned in the README file, this project was conducted in two analytical phases. 
# The first phase involved an extensive descriptive and exploratory analysis, which was fully reported in my MSc thesis entitled “Blood count parameters as early biomarkers for therapeutic outcome in cutaneous leishmaniasis: a retrospective cohort in Colombia”. This work was recognized as a meritorious thesis by the Universidad del Valle, granting me the MSc degree in Epidemiology.
# The second phase consisted of validating the initial findings through a refined analytical strategy in R. A subset of clinically relevant variables—selected based on their performance in phase one—was integrated with complete blood count parameters to form the primary analytical dataset. This phase included multiple imputation of missing data, the development and validation of machine learning models, and a comparative performance assessment. Only the validated results from this second phase were used in the final manuscript submitted for publication.
######################################################################


## Instalacion de paquetes

#install.packages("readxl") ## Abrir exceles
#install.packages("PASWR") ## Tiene funcion EDA que me facilita analisis exploratorio
#install.packages("DataExplorer") ## Multiples funciones analisis exploratorio y construccion de reporte
#install.packages("skimr") ## Me sirve para calcular medias de un grupo seleccionado
#install.packages("janitor") ## Tiene funcion tabyl para tablas exploratoria
#install.packages("ggthemes") ## Para personalizar reportes
#install.packages("ggplot2") ## Para personalizar reportes
#install.packages("GGally") ## Para personalizar reportes
#install.packages ("knitr") ## Para personalizar graficos
#install.packages("gtsummary") ## Para generar tablas reporte
#install.packages("dplyr") ## Para manejo de datos en los analisis 
#install.packages("tidyverse") ## Para generacion de nuevos dataframe
#install.packages("tidyr") ## Para generacion de nuevos dataframe
#install.packages ("reshape2") ## Para reorganizar dataframes (wide or long)
#install.packages ("stats") ## Estadisticas basicas
#install.packages("textshape") ## Para reshaping text 
#install.packages ("FactoMineR") ## Para realizacion de componentes principales
#install.packages("missMDA") ## Para manejo de missing data
#install.packages ("ggfortify") ## Visualizacion grafia de analisis estadisticos (en mi caso para el componentes principales)
#install.packages ("factoextra") ## Para completar y revalorar el analisis de componentes principales
#install.packages("mdatools")
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("mixOmics")


##Limpieza del ambiente
rm(list = ls())

## Carga de librerias
library(readxl) ## Abrir exceles
library(PASWR) ## Tiene funcion EDA que me facilita analisis exploratorio
library(DataExplorer) ## Multiples funciones analisis exploratorio y construccion de reporte
library(skimr) ## Me sirve para calcular medias de un grupo seleccionado
library(janitor) ## Tiene funcion tabyl para tablas exploratoria
library(ggthemes) ## Para personalizar reportes
library(ggplot2) ## Para personalizar reportes
library(GGally) ## Para personalizar reportes
library (knitr) ## Para personalizar graficos
library(gtsummary) ## Para generar tablas reporte
library(dplyr) ## Para manejo de datos en los analisis 
library(tidyverse) ## Para generacion de nuevos dataframe
library(tidyr) ## Para generacion de nuevos dataframe
library (reshape2) ## Para reorganizar dataframes (wide or long)
library (stats) ## Estadisticas basicas
library(textshape) ## Para reshaping text 
library (FactoMineR) ## Para realizacion de componentes principales
library(missMDA) ## Para manejo de missing data
library (ggfortify) ## Visualizacion grafia de analisis estadisticos (en mi caso para el componentes principales)
library (factoextra) ## Para completar y revalorar el analisis de componentes principales
library(mdatools)
library(mixOmics)
library(cutpointr) ### para puntos de corte
library(generalhoslem) ## hosmer lemeshow
library(lmtest) # LR test
library(pROC)



## Carga de los datos
load("data/Clean_data_RData/participantes.RData")
load("data/Clean_data_RData/criterios_inclusion.RData")
load("data/Clean_data_RData/evaluacion_base.RData")
load("data/Clean_data_RData/hemopre.RData")
load("data/Clean_data_RData/fin_tto.RData")
load("data/Clean_data_RData/hemopos.RData")
load("data/Clean_data_RData/sem_8.RData")
load("data/Clean_data_RData/sem_13.RData")
load("data/Clean_data_RData/sem_26.RData")
load("data/Clean_data_RData/visita_adic.RData")
load("data/Clean_data_RData/especie_parasit.RData")
load("data/Clean_data_RData/estado_fin.RData")
load("data/Clean_data_RData/gran_base_resumen.RData")
load("data/Clean_data_RData/Big_hemo.RData")
load("data/Clean_data_RData/hemogramas_centrados.RData")
load("data/Clean_data_RData/Base_regresion_final.RData")

# Datos de figura 1
table(evaluacion_base$visita_eval_realizada_eb)
table(fin_tto$visita_eval_realizada_ftto)
table(sem_8$visita_eval_realizada_s8)
table(sem_13$visita_eval_realizada_s13)
table(sem_26$visita_eval_realizada_s26)
prop.table(table(sem_26$visita_eval_realizada_s26))

## Figura S1. Tiempo transcurrido entre evaluacion de base y visitas

## Calculos dias entre evaluacion de base y visitas

general_dias <- ggplot()+
  geom_histogram(data = fin_tto, aes( x = dias_transcurridos_ftto),
                 binwidth = 1,
                 fill = "SteelBlue")+
  geom_vline(data = fin_tto, aes(xintercept = 20), linetype = 2)+
  geom_vline(data = fin_tto, aes(xintercept = 35), linetype = 2)+
  
  geom_histogram(data = sem_8, aes(x = dias_transcurridos_s8), binwidth = 1, fill = "#00A087B2") +
  geom_vline(aes(xintercept = 42), linetype = 2, color = "#00A087B2")+
  geom_vline(aes(xintercept = 63), linetype = 2, color = "#00A087B2")+
  
  geom_histogram(data = sem_13, aes(x = dias_transcurridos_s13), binwidth = 1, fill = "#3C5488B2") +
  geom_vline(aes(xintercept = 76), linetype = 2, color = "#3C5488B2")+
  geom_vline(aes(xintercept = 104), linetype = 2, color = "#3C5488B2")+
  
  geom_histogram(data = sem_26, aes(x = dias_transcurridos_s26), binwidth = 1, fill = "#DC0000B2") +
  geom_vline(aes(xintercept = 166), linetype = 2, color = "#DC0000B2")+
  geom_vline(aes(xintercept = 194), linetype = 2, color = "#DC0000B2")+
  
  scale_x_continuous(breaks = seq(0,250, by = 30))+
  labs(x = "Dias transcurridos hasta la visita desde la evaluacion de base",
       y = "Número de pacientes que acudieron",
       title = "Numero de pacientes por visita y tiempo transcurrido desde la evaluacion de base",
       caption = "
       Visita de fin de tratamiento (Azul). 
       Visita de semana 8 (Verde). 
       Visita de semana 13 (Purpura). 
       Visita de semana 26 (Rojo).
       ")+ 
  theme_classic()+
  theme(plot.caption = element_text(hjust = 0))

general_dias


#### Grafica por grupo de semanas

g_fin_tto <- ggplot()+
  geom_histogram(data = fin_tto, aes( x = dias_transcurridos_ftto),
                 binwidth = 1,
                 fill = "SteelBlue")+
  geom_vline(data = fin_tto, aes(xintercept = 20), linetype = 2)+
  geom_vline(data = fin_tto, aes(xintercept = 35), linetype = 2)+
  scale_x_continuous(breaks = seq(0,60, by = 10))+
  labs(x = "Dias Transcurridos hasta la visita",
       y = "Número de Pacientes",
       title = "Visita de Fin de Tratamiento")+
  theme_classic()


g_sem8 <- ggplot()+
  geom_histogram(data = sem_8, aes(x = dias_transcurridos_s8), binwidth = 1, fill = "#00A087B2") +
  geom_vline(aes(xintercept = 42), linetype = 2, color = "#00A087B2")+
  geom_vline(aes(xintercept = 63), linetype = 2, color = "#00A087B2")+
  scale_x_continuous(breaks = seq(40,80, by = 10))+
  labs(x = "Dias Transcurridos hasta la visita",
       y = "Número de Pacientes",
       title = "Visita Semana 8")+
  theme_classic()


g_sema13 <- ggplot()+
  geom_histogram(data = sem_13, aes(x = dias_transcurridos_s13), binwidth = 1, fill = "#3C5488B2") +
  geom_vline(aes(xintercept = 76), linetype = 2, color = "#3C5488B2")+
  geom_vline(aes(xintercept = 104), linetype = 2, color = "#3C5488B2")+
  scale_x_continuous(breaks = seq(80,150, by = 10))+
  labs(x = "Dias Transcurridos hasta la visita",
       y = "Número de Pacientes",
       title = "Visita Semana 13")+
  theme_classic()


g_sema26 <- ggplot()+
  geom_histogram(data = sem_26, aes(x = dias_transcurridos_s26), binwidth = 1, fill = "#DC0000B2") +
  geom_vline(aes(xintercept = 166), linetype = 2, color = "#DC0000B2")+
  geom_vline(aes(xintercept = 194), linetype = 2, color = "#DC0000B2")+
  scale_x_continuous(breaks = seq(140,250, by = 10))+
  labs(x = "Dias Transcurridos hasta la visita",
       y = "Número de Pacientes",
       title = "Visita Semana 26")+
  theme_classic()

f1 <- gridExtra::grid.arrange(g_fin_tto, g_sem8, g_sema13, g_sema26)

figura1a <- gridExtra::grid.arrange(f1, general_dias)


#######################################################
### Diferencia de tiempo de los hemogramas
#######################################################

g_tiempo_hemogramas <- ggplot()+
  geom_histogram(data = hemopos, aes( x = tiempo_dias_entre_hemogramas),
                 binwidth = 1,
                 fill = "SteelBlue")+
  geom_vline(data = hemopos, aes(xintercept = 18), linetype = 2)+
  geom_vline(data = hemopos, aes(xintercept = 35), linetype = 2)+
  scale_x_continuous(breaks = seq(0,60, by = 10))+
  labs(x = "Dias Transcurridos hasta el segundo hemograma",
       y = "Número de Pacientes",
       title = "Tiempo entre hemogramas")+
  theme_classic()

g_tiempo_hemogramas


## Base tabla 1

tble1 <- 
  gran_base_resumen %>%
  dplyr::select(edad, edad_categorica, sexo, etnia, antecedente_leish,
         tiempo_sintom_semanas, peso, talla, imc, imc_categorico, numero_lesiones, lesiones_categorica,
         tipo_lesion_evalbas, adenopatia_evalbas, infeccion_concom_evalbas,
         comorbilidades, numero_comorbilidades, comorb_cardio,
         comorb_respira, comorb_gastroint, comorb_endocrin, comorb_neuro,
         comorb_otra, hemo_pre_realizado, hemo_pos_realizado,
         especie, tratamiento, dosis_glu_pres, dosis_glu_anormal, dosis_mil_pres, dosis_mil_anormal, rango_dosis_medicamento,
         acude_fin_tto, dias_fin_tto, variacion_lesion_post,variacion_lesion_post2, adenopatia_fin_tto, 
         infeccion_concom_fintto, adherencia_tto, estado_fin_tto, 
         acude_sem8,dias_sem8, estado_sem8, 
         acude_sem13, dias_sem13, estado_sem13,
         acude_sem26, dias_sem26, estado_sem26,
         acude_va, dias_va, estado_va,
         estado_final) %>% 
  tbl_summary(
    missing_text = "No dato / No aplica",
    label = list(edad = "Edad", edad_categorica = "Categoria edad", sexo = "Sexo",  etnia = "Etnia", antecedente_leish = "Antecedente de leihsmaniasis",
                 tiempo_sintom_semanas = "Tiempo de evolucion en semanas", peso = "Peso en kg", talla = "Talla en cm", imc= "Indice de masa corporal",
                 imc_categorico = "Categorias - Incide de masa corporal",
                 numero_lesiones= "Numero de lesiones", lesiones_categorica = "Categoria - numero de lesiones",
                 tipo_lesion_evalbas = "Tipo de lesion principal en evaluacion de base",
                 adenopatia_evalbas= "Presencia de adenopatia en evaluacion de base", infeccion_concom_evalbas ="Infeccion concomitante en evaluacion de base",
                 comorbilidades = "Antecedente de comorbilidades", numero_comorbilidades = "Numero de comorbilidades", comorb_cardio = "Comorbilidad cardiovascular",
                 comorb_respira = "Comorbilidad respiratoria", comorb_gastroint = "Comorbilidad gastrointestinal", comorb_endocrin = "Comorbilidad endocrina",
                 comorb_neuro = "Comorbilidad neurologica", comorb_otra = "Otro tipo de comorbilidad", 
                 hemo_pre_realizado = "Hemograma pre-tratamiento realizado", hemo_pos_realizado = "Hemograma pos-tratamiento realizado",
                 especie= "Especie parasitaria", 
                 tratamiento = "Tratamiento ordenado", dosis_glu_pres = "Dosis de Glucantime (mg/kg/dia)", dosis_glu_anormal = "Rango terapeutico de la dosis de glucantime",
                 dosis_mil_pres = "Dosis de Miltefosine (mg/kg/dia)", dosis_mil_anormal = "Rango terapeutico de la dosis de miltefosine", rango_dosis_medicamento = "Rango terapeutico de medicamento antileishmanial",
                 acude_fin_tto = "Evalaucion de fin de tratamiento realizada", dias_fin_tto = "Dias transcurridos hasta valoracion de fin de tratamiento", 
                 variacion_lesion_post = "Variacion la lesion a fin de tratamiento (Pre - Post)", variacion_lesion_post2 = "Variacion la lesion a fin de tratamiento (Pre - Post)", adenopatia_fin_tto = "Adenopatia a fin de tratamiento", 
                 infeccion_concom_fintto = "Infeccion concomientante al final de tratamiento", adherencia_tto = "Porcentaje de adherencia al tratamiento",estado_fin_tto = "Estado en visita de fin de tratamiento", 
                 acude_sem8 = "Valoracion de semana 8 realizada", dias_sem8 = "Dias transcurridos hasta valoracion de semana 8", estado_sem8 = "Estado a semana 8", 
                 acude_sem13 = "Valoracion de semana 13 realizada", dias_sem13 = "Dias transcurridos hasta valoracion de semana 13", estado_sem13 = "Estado a semana 13",
                 acude_sem26 = "Valoracion de semana 26 realizada", dias_sem26 = "Dias transcurridos hasta valoracion de semana 26", estado_sem26 = "Estado a semana 26",
                 acude_va = "Valoracion adicional realizada", dias_va = "Dias transcurridos hasta valoracion adicional", estado_va = "Estado en visita adicional",
                 estado_final = "Estado final"),
    type = c("edad", "tiempo_sintom_semanas", "peso", "talla", "imc", "numero_lesiones", "numero_comorbilidades", "dias_fin_tto", "dias_sem8", "dias_sem13", "dias_sem26", "dias_va", "adherencia_tto") ~ "continuous",
    statistic = list(adherencia_tto  ~ "{mean} ({sd})"),
  ) 

tble1  

###########################
## ANALISIS HEMOGRAMAS ####
###########################


## Primero creo una copia de mi dataframe de hemogramas para poder recuperarlo en caso de que se modifique por algun comando mas abajo
## y que pueda modificar para que no me genere problemas al sacar tablas  o correr el analisis de componentes principales

df <- Big_hemo #Creo mi dataframe copia de hemogramas

## ¿Las relaciones de las lineas celulares son diferenciales entre curas y fallas?

## Primero lo exploro con un scatter plot matrix, coloreado segun el estado final

scatter_hemopre <- hemopre %>% mutate (estado_final = factor(gran_base_resumen$estado_final)) %>% 
  ggpairs(columns = c("recuen_leuco_pre_tto","recuen_neutro_pre_tto", "recuen_linfo_pre_tto", "recuen_mono_pre_tto", "recuen_eosi_pre_tto", "recuen_baso_pre_tto", "recuen_granu_pre_tto"), 
          aes(color = estado_final),
          title = "Comportamiento de las lineas celulares pre tratamiento",
          columnLabels = c("Leucocitos", "Neutrofilos", "Linfocitos", "Monocitos", "Eosinofilos", "Basofilos", "Granulocitos inmaduros")
  )

scatter_hemopos <- hemopos %>% mutate (estado_final = factor(gran_base_resumen$estado_final)) %>%
  ggpairs(columns = c("recuen_leuco_post_tto", "recuen_neutro_post_tto", "recuen_linfo_post_tto", "recuen_mono_post_tto", "recuen_eosi_post_tto","recuen_baso_post_tto", "recuen_granu_post_tto"),
          aes(color = estado_final),
          columnLabels = c("Leucocitos", "Neutrofilos", "Linfocitos", "Monocitos", "Eosinofilos", "Basofilos", "Granulocitos inmaduros"),
          title = "Comportamiento de las lineas celulares a final de tratamiento"
  )

scatter_hemopre2 <- Big_hemo %>%  
  ggpairs(columns = c("Leucocitos_pre","Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre", "Eosinofilos_pre", "Basofilos_pre", "Granulocitos_pre",
                      "i_neu_linfo_pre", "i_eos_linfo_pre", "i_mono_linfo_pre", "i_baso_linfo_pre", "i_granu_linfo_pre",
                      "i_neu_granu_pre", "i_eos_granu_pre", "i_baso_granu_pre", "i_eos_mono_pre", "i_eos_neu_pre", "i_eos_baso_pre", "i_monoeosneu_lin_pre"
  ), 
  aes(color = Estado_final),
  title = "Comportamiento de las lineas celulares pre tratamiento",
  columnLabels = c("Leucocitos", "Neutrofilos", "Linfocitos", "Monocitos", "Eosinofilos", "Basofilos", "Granulocitos",
                   "neu_linfo", "eos_linfo", "mono_linfo", "baso_linfo", "granu_linfo",
                   "neu_granu", "eos_granu", "baso_granu", "eos_mono", "eos_neu", "eos_baso", "monoeosneu_lin"
  )
  )

scatter_hemopos2 <- Big_hemo %>%  
  ggpairs(columns = c("Leucocitos_post","Neutrofilos_post", "Linfocitos_post", "Monocitos_post", "Eosinofilos_post", "Basofilos_post", "Granulocitos_post",
                      "i_neu_linfo_post", "i_eos_linfo_post", "i_mono_linfo_post", "i_baso_linfo_post", "i_granu_linfo_post",
                      "i_neu_granu_post", "i_eos_granu_post", "i_baso_granu_post", "i_eos_mono_post", "i_eos_neu_post", "i_eos_baso_post", "i_monoeosneu_lin_post"
  ), 
  aes(color = Estado_final),
  title = "Comportamiento de las lineas celulares pre tratamiento",
  columnLabels = c("Leucocitos", "Neutrofilos", "Linfocitos", "Monocitos", "Eosinofilos", "Basofilos", "Granulocitos",
                   "neu_linfo", "eos_linfo", "mono_linfo", "baso_linfo", "granu_linfo",
                   "neu_granu", "eos_granu", "baso_granu", "eos_mono", "eos_neu", "eos_baso", "monoeosneu_lin"
  )
  )

scatter_hemopre2
scatter_hemopos2


## ¿Que comportamiento tienen los parametros del hemograma de base? 
tble2a <- 
  df %>%
  select(Leucocitos_pre, Neutrofilos_pre, Linfocitos_pre, Monocitos_pre, Eosinofilos_pre, 
         Basofilos_pre, Granulocitos_pre, Granulocitos_pre_exp, Globulos_rojos_pre, Hemoglobina_pre, Hematocrito_pre, Plaquetas_pre,
         porcentaje_neutro_pre, porcentaje_linfo_pre, porcentaje_mono_pre, porcentaje_eosino_pre, porcentaje_baso_pre,
         porcentaje_granulo_pre, i_neu_linfo_pre, i_eos_linfo_pre, i_mono_linfo_pre, i_baso_linfo_pre, i_granu_linfo_pre,
         i_neu_granu_pre, i_eos_granu_pre, i_baso_granu_pre, i_eos_mono_pre, i_eos_neu_pre, i_eos_baso_pre, 
         i_neu_mono_pre, i_neu_baso_pre, i_baso_mono_pre, i_monoeosneu_lin_pre,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(Leucocitos_pre = "Leucocitos pretratamiento", Neutrofilos_pre = "Neutrofilos pretratamiento", 
                 Linfocitos_pre = "Linfocitos pretratamiento", Monocitos_pre = "Monocitos pretratamiento", 
                 Eosinofilos_pre = "Eosinofilos pretratamiento", Basofilos_pre = "Basofilos pretratamiento", 
                 Granulocitos_pre = "Granulocitos inmaduros pretratamiento", Granulocitos_pre_exp = "Granulocitos pretratamiento exponenciados", Globulos_rojos_pre = "Globulos rojos pretratamiento", 
                 Hemoglobina_pre = "Hemoglobina pretratamiento", Hematocrito_pre = "Hematocrito pretratamiento", 
                 Plaquetas_pre = "Plaquetas pretratamiento", i_eos_baso_pre = "Indice eosinofilos/basofilos pretratamiento",
                 porcentaje_neutro_pre =  "% neutrofilos pretratamiento", porcentaje_linfo_pre = "% linfocitos pretratamiento", 
                 porcentaje_mono_pre = "% monocitos pretratamiento", porcentaje_eosino_pre = "% eosinofilos pretratamiento", 
                 porcentaje_baso_pre = "% basofilos pretratamiento", porcentaje_granulo_pre = "% granulocitos pretratamiento",
                 i_neu_linfo_pre = "Indice neutrofilos / linfocitos pretratamiento", i_eos_linfo_pre ="Indice eosinofilos / linfocitos pretratamiento", 
                 i_mono_linfo_pre = "Indice monocitos / linfocitos pretratamiento", i_baso_linfo_pre = "Indice basofilos / linfocitos pretratamiento", 
                 i_granu_linfo_pre = "Indice granulocitos exponenciados / linfocitos pretratamiento", i_neu_granu_pre = "Indice neutrofilos / granulocitos exponenciados pretratamiento",
                 i_eos_granu_pre = "Indice eosinofilos / granulocitos exponenciados pretratamiento", i_baso_granu_pre ="Indice basofilos / granulocitos exponenciados pretratamiento",
                 i_eos_mono_pre = "Indice eosinofilos / monocitos pretratamiento", i_eos_neu_pre = "Indice eosinofilos / neutrofilos pretratamiento",
                 i_neu_mono_pre = "Indice neutrofilos / monocitos pretratamiento", i_neu_baso_pre = "Indice neutrofilos / basofilos pretratamiento", 
                 i_baso_mono_pre = "Indice basofilos / monocitos pretratamiento",
                 i_monoeosneu_lin_pre = "Indice (monocitos + eosinofilos + neutrofilos) / lindfocitos)"),
  ) 

tble2a

## ¿Que comportamiento tienen los parametros del hemograma postratamiento?

tble2b <- 
  df %>%
  select (Leucocitos_post, Neutrofilos_post, Linfocitos_post, Monocitos_post, Eosinofilos_post, 
          Basofilos_post, Granulocitos_post, Granulocitos_post_exp, Globulos_rojos_post, Hemoglobina_post, Hematocrito_post, 
          porcentaje_neutro_post, porcentaje_linfo_post, porcentaje_mono_post, porcentaje_eosino_post, porcentaje_baso_post,
          porcentaje_granulo_post, i_neu_linfo_post, i_eos_linfo_post, i_mono_linfo_post, i_baso_linfo_post, i_granu_linfo_post,
          i_neu_granu_post, i_eos_granu_post, i_baso_granu_post, i_eos_mono_post, i_eos_neu_post, i_eos_baso_post,
          i_neu_mono_post, i_neu_baso_post, i_baso_mono_post, i_monoeosneu_lin_post,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(Leucocitos_post = "Leucocitos postratamiento", Neutrofilos_post = "Neutrofilos postratamiento", 
                 Linfocitos_post = "Linfocitos postratamiento", Monocitos_post = "Monocitos postratamiento", 
                 Eosinofilos_post = "Eosinofilos postratamiento", Basofilos_post = "Basofilos postratamiento", 
                 Granulocitos_post = "Granulocitos inmaduros postratamiento", Granulocitos_post_exp = "Granulocitos postratamiento exponenciados", Globulos_rojos_post = "Globulos rojos postratamiento", 
                 Hemoglobina_post = "Hemoglobina postratamiento", Hematocrito_post = "Hematocrito postratamiento", 
                 Plaquetas_post = "Plaquetas postratamiento", i_eos_baso_post = "Indice eosinofilos/basofilos postratamiento",
                 porcentaje_neutro_post =  "% neutrofilos postratamiento", porcentaje_linfo_post = "% linfocitos postratamiento", 
                 porcentaje_mono_post = "% monocitos postratamiento", porcentaje_eosino_post = "% eosinofilos postratamiento", 
                 porcentaje_baso_post = "% basofilos postratamiento", porcentaje_granulo_post = "% granulocitos postratamiento",
                 i_neu_linfo_post = "Indice neutrofilos / linfocitos postratamiento", i_eos_linfo_post ="Indice eosinofilos / linfocitos postratamiento", 
                 i_mono_linfo_post = "Indice monocitos / linfocitos postratamiento", i_baso_linfo_post = "Indice basofilos / linfocitos postratamiento", 
                 i_granu_linfo_post = "Indice granulocitos exponenciados / linfocitos postratamiento", i_neu_granu_post = "Indice neutrofilos / granulocitos exponenciados postratamiento",
                 i_eos_granu_post = "Indice eosinofilos / granulocitos exponenciados postratamiento", i_baso_granu_post ="Indice basofilos / granulocitos exponenciados postratamiento",
                 i_eos_mono_post = "Indice eosinofilos / monocitos postratamiento", i_eos_neu_post = "Indice eosinofilos / neutrofilos postratamiento",
                 i_neu_mono_post = "Indice neutrofilos / monocitos postratamiento", i_neu_baso_post = "Indice neutrofilos / basofilos postratamiento", 
                 i_baso_mono_post = "Indice basofilos / monocitos postratamiento",
                 i_monoeosneu_lin_post = "Indice (monocitos + eosinofilos + neutrofilos) / lindfocitos postratamiento"),
  )   

tble2b


## ¿La forma en que varia influye?

tble2c <- 
  df %>%
  select (variacion_leucocitos, variacion_neutrofilos, variacion_linfocitos, variacion_monocitos,
          variacion_eosinofilos, variacion_basofilos, variacion_granulocitos, variacion_globulos_rojos,
          variacion_hemoglobina, variacion_hematocrito, variacion_porcentaje_neutro, variacion_porcentaje_linfo,
          variacion_porcentaje_mono, variacion_porcentaje_eosino, variacion_porcentaje_baso, variacion_porcentaje_granulo,
          variacion_i_neu_linfo, variacion_i_eos_linfo, variacion_i_mono_linfo, variacion_i_baso_linfo, 
          variacion_i_granu_linfo, variacion_i_neu_granu, variacion_i_eos_granu, variacion_i_baso_granu,
          variacion_i_eos_mono, variacion_i_eos_neu, variacion_i_eos_baso, 
          variacion_i_neu_mono, variacion_i_neu_baso, variacion_i_baso_mono, variacion_i_monoeosneu_lin,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(variacion_leucocitos = "Ratio leucocitos post / pre",
                 variacion_neutrofilos = "Ratio neutrofilos post / pre", 
                 variacion_linfocitos = "Ratio linfocitos post / pre",
                 variacion_monocitos = "Ratio monocitos post / pre",
                 variacion_eosinofilos = "Ratio eosinofilos post / pre",
                 variacion_basofilos = "Ratio basofilos post / pre",
                 variacion_granulocitos = "Ratio granulocitos post / pre",
                 variacion_globulos_rojos = "Ratio globulos rojos post / pre",
                 variacion_hemoglobina = "Ratio hemoglobina post / pre",
                 variacion_hematocrito = "Ratio hematocrito post / pre",
                 variacion_porcentaje_neutro = "Ratio porcentaje de neutrofilos post / pre",
                 variacion_porcentaje_linfo = "Ratio porcentaje de linfocitos post / pre",
                 variacion_porcentaje_mono = "Ratio porcentaje de monocitos post / pre",
                 variacion_porcentaje_eosino = "Ratio porcentaje de eosinofilos post / pre",
                 variacion_porcentaje_baso = "Ratio porcentaje de basofilos post / pre",
                 variacion_porcentaje_granulo = "Ratio porcentaje de granulocitos post / pre",
                 variacion_i_neu_linfo = "Variacion indice neutrofilos/linfocitos (post / pre)", 
                 variacion_i_eos_linfo = "Variacion indice eosinofilos/linfocitos (post / pre)",
                 variacion_i_mono_linfo = "Variacion indice monocitos/linfocitos (post / pre)",
                 variacion_i_baso_linfo = "Variacion indice basofilos/linfocitos (post / pre)",
                 variacion_i_granu_linfo = "Variacion indice granulocitos exponenciados/linfocitos (post / pre)",
                 variacion_i_neu_granu = "Variacion indice neutrofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_granu = "Variacion indice eosinofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_baso_granu = "Variacion indice basofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_mono = "Variacion indice eosinofilos/monocitos (post / pre)",
                 variacion_i_eos_neu = "Variacion indice eosinofilos/neutrofilos (post / pre)",
                 variacion_i_eos_baso = "Variacion indice eosinofilos/basofilos (post / pre)",
                 variacion_i_neu_mono = "Variacion indice neutrofilos/monocitos (post / pre)", 
                 variacion_i_neu_baso = "Variacion indice neutrofilos/basofilos (post / pre)", 
                 variacion_i_baso_mono = "Variacion indice basofilos/monocitos (post / pre)",
                 variacion_i_monoeosneu_lin = "Variacion indice compuesto ((monocitos + neutrofilos + eosinofilos) / linfos ) (post / pre)"),
  )   

tble2c


## TABLA DE INCIDENCIAS POR SEMANA

tble_incidencias <- gran_base_resumen %>% 
  dplyr::select(estado_sem8, estado_sem13, estado_sem26, estado_final) %>% 
  tbl_summary(
    missing_text = "No dato / No aplica",) %>% 
  add_ci

    
tble_incidencias  


gran_base_resumen <- gran_base_resumen %>% 
  mutate(estado_sem_8_modificado= case_when(
    gran_base_resumen$estado_sem8 == "Mejoria" ~ "0",
    gran_base_resumen$estado_sem8 == "Sin cambios" ~ "0",
    gran_base_resumen$estado_sem8 == "Curacion aparente" ~ "0",
    gran_base_resumen$estado_sem8 == "Falla terapeutica" ~ "1"))


gran_base_resumen$estado_sem_8_modificado <- factor(gran_base_resumen$estado_sem_8_modificado,
                                                    levels = c("0", "1"),
                                                    labels= c("Mejoria", "Falla terapeutica"))

gran_base_resumen <- gran_base_resumen %>% 
  mutate(estado_sem_13_modificado= case_when(
    gran_base_resumen$estado_sem13 == "Mejoria" ~ "0",
    gran_base_resumen$estado_sem13 == "Sin cambios" ~ "0",
    gran_base_resumen$estado_sem13 == "Cura definitiva" ~ "0",
    gran_base_resumen$estado_sem13 == "Cura aparente" ~ "0",
    gran_base_resumen$estado_sem13 == "Falla terapeutica" ~ "1"))


gran_base_resumen$estado_sem_13_modificado <- factor(gran_base_resumen$estado_sem_13_modificado,
                                                    levels = c("0", "1"),
                                                    labels= c("Cura aparente", "Falla terapeutica"))

gran_base_resumen <- gran_base_resumen %>% 
  mutate(estado_sem_26_modificado= case_when(
    gran_base_resumen$estado_sem26 == "Mejoria" ~ "0",
    gran_base_resumen$estado_sem26 == "Cura definitiva" ~ "0",
    gran_base_resumen$estado_sem26 == "Cura aparente" ~ "0",
    gran_base_resumen$estado_sem26 == "Falla terapeutica" ~ "1"))


gran_base_resumen$estado_sem_26_modificado <- factor(gran_base_resumen$estado_sem_26_modificado,
                                                     levels = c("0", "1"),
                                                     labels= c("Cura definitiva", "Falla terapeutica"))

tble_incidencias2 <- gran_base_resumen %>% 
  dplyr::select(estado_sem_8_modificado, estado_sem_13_modificado, estado_sem_26_modificado, estado_final) %>% 
  tbl_summary(
    missing_text = "No dato / No aplica",) %>% 
  add_ci

tble_incidencias2

### ANALISIS BIVARIADOS ###

### Tabla 3 ##

tble3a <- 
  gran_base_resumen %>%
  dplyr::select(edad, edad_categorica, sexo, etnia, antecedente_leish,
         tiempo_sintom_semanas, tiempo_evolucion_dicotomica, peso, talla, imc, imc_categorico, numero_lesiones, lesiones_categorica,
         tipo_lesion_evalbas, adenopatia_evalbas, infeccion_concom_evalbas,
         comorbilidades, numero_comorbilidades, comorb_cardio,
         comorb_respira, comorb_gastroint, comorb_endocrin, comorb_neuro,
         comorb_otra, hemo_pre_realizado, hemo_pos_realizado,
         especie, tratamiento, dosis_glu_pres, dosis_glu_anormal, dosis_mil_pres, dosis_mil_anormal, rango_dosis_medicamento,
         acude_fin_tto, dias_fin_tto, variacion_lesion_post,variacion_lesion_post2, adenopatia_fin_tto, 
         infeccion_concom_fintto, adherencia_tto, estado_fin_tto, 
         acude_sem8,dias_sem8, estado_sem8, 
         acude_sem13, dias_sem13, estado_sem13,
         acude_sem26, dias_sem26, estado_sem26,
         acude_va, dias_va, estado_va,
         estado_final) %>% 
  tbl_summary(
    by = estado_final,
    percent = "row",
    missing_text = "No dato / No aplica",
    label = list(edad = "Edad", edad_categorica = "Categoria edad", sexo = "Sexo",  etnia = "Etnia", antecedente_leish = "Antecedente de leihsmaniasis",
                 tiempo_sintom_semanas = "Tiempo de evolucion en semanas", tiempo_evolucion_dicotomica = "Tiempo de evolucion mayor a cuatro semanas", peso = "Peso en kg", talla = "Talla en cm", imc= "Indice de masa corporal",
                 imc_categorico = "Categorias - Incide de masa corporal",
                 numero_lesiones= "Numero de lesiones", lesiones_categorica = "Categoria - numero de lesiones",
                 tipo_lesion_evalbas = "Tipo de lesion principal en evaluacion de base",
                 adenopatia_evalbas= "Presencia de adenopatia en evaluacion de base", infeccion_concom_evalbas ="Infeccion concomitante en evaluacion de base",
                 comorbilidades = "Antecedente de comorbilidades", numero_comorbilidades = "Numero de comorbilidades", comorb_cardio = "Comorbilidad cardiovascular",
                 comorb_respira = "Comorbilidad respiratoria", comorb_gastroint = "Comorbilidad gastrointestinal", comorb_endocrin = "Comorbilidad endocrina",
                 comorb_neuro = "Comorbilidad neurologica", comorb_otra = "Otro tipo de comorbilidad", 
                 hemo_pre_realizado = "Hemograma pre-tratamiento realizado", hemo_pos_realizado = "Hemograma pos-tratamiento realizado",
                 especie= "Especie parasitaria", 
                 tratamiento = "Tratamiento ordenado", dosis_glu_pres = "Dosis de Glucantime (mg/kg/dia)", dosis_glu_anormal = "Rango terapeutico de la dosis de glucantime",
                 dosis_mil_pres = "Dosis de Miltefosine (mg/kg/dia)", dosis_mil_anormal = "Rango terapeutico de la dosis de miltefosine", rango_dosis_medicamento = "Rango terapeutico de medicamento antileishmanial",
                 acude_fin_tto = "Evalaucion de fin de tratamiento realizada", dias_fin_tto = "Dias transcurridos hasta valoracion de fin de tratamiento", 
                 variacion_lesion_post = "Variacion la lesion a fin de tratamiento (Pre - Post)", variacion_lesion_post2 = "Variacion la lesion a fin de tratamiento (Pre - Post)", adenopatia_fin_tto = "Adenopatia a fin de tratamiento", 
                 infeccion_concom_fintto = "Infeccion concomientante al final de tratamiento", adherencia_tto = "Porcentaje de adherencia al tratamiento",estado_fin_tto = "Estado en visita de fin de tratamiento", 
                 acude_sem8 = "Valoracion de semana 8 realizada", dias_sem8 = "Dias transcurridos hasta valoracion de semana 8", estado_sem8 = "Estado a semana 8", 
                 acude_sem13 = "Valoracion de semana 13 realizada", dias_sem13 = "Dias transcurridos hasta valoracion de semana 13", estado_sem13 = "Estado a semana 13",
                 acude_sem26 = "Valoracion de semana 26 realizada", dias_sem26 = "Dias transcurridos hasta valoracion de semana 26", estado_sem26 = "Estado a semana 26",
                 acude_va = "Valoracion adicional realizada", dias_va = "Dias transcurridos hasta valoracion adicional", estado_va = "Estado en visita adicional",
                 estado_final = "Estado final"),
    type = c("edad", "tiempo_sintom_semanas", "peso", "talla", "imc", "numero_lesiones", "numero_comorbilidades", "dias_fin_tto", "dias_sem8", "dias_sem13", "dias_sem26", "dias_va", "adherencia_tto") ~ "continuous",
    statistic = list(adherencia_tto  ~ "{mean} ({sd})"),
  ) %>% 
  add_p() %>%
  add_q() %>%
  add_overall()

tble3a 


## Evaluacion de RR univariados se hizo en STATA. 


## Este corre bien, pero prefería calcular los RR de manera clasica en STATA (VER MAS ABAJO)
tble3b <- gran_base_resumen %>%
  select(edad, edad_categorica, sexo, etnia, 
         tiempo_sintom_semanas, lesiones_categorica,
         adenopatia_evalbas, infeccion_concom_evalbas,
         comorbilidades, numero_comorbilidades,
         tratamiento, 
         variacion_lesion_post2,  
         estado_final.1) %>% 
  tbl_uvregression(y = estado_final.1,
                   method = glm,
                   method.args = list(family = binomial(link = "log")),
                   exponentiate = TRUE)


tble3b

## Sin embargo para las variables numericas se calculo una log binomial regression
tble_rr_peso <- gran_base_resumen %>%
  select(peso,
         estado_final.1) %>% 
  tbl_uvregression(y = estado_final.1,
                   method = glm,
                   method.args = list(family = binomial(link = "log")),
                   exponentiate = TRUE)


tble_rr_peso

tble_rr_talla <- gran_base_resumen %>%
  select(talla,
         estado_final.1) %>% 
  tbl_uvregression(y = estado_final.1,
                   method = glm,
                   method.args = list(family = binomial(link = "log")),
                   exponentiate = TRUE)


tble_rr_talla

tble_rr_imc <- gran_base_resumen %>%
  select(imc,
         estado_final.1) %>% 
  tbl_uvregression(y = estado_final.1,
                   method = glm,
                   method.args = list(family = binomial(link = "log")),
                   exponentiate = TRUE)


tble_rr_imc

tble_rr_nlesiones <- gran_base_resumen %>%
  select(numero_lesiones,
         estado_final.1) %>% 
  tbl_uvregression(y = estado_final.1,
                   method = glm,
                   method.args = list(family = binomial(link = "log")),
                   exponentiate = TRUE)


tble_rr_nlesiones


## Tengo muchas variables posiblemente asociadas, ¿sera que un PCA me puede ayudar a reducir la dimensionalidad?
#df <- na.omit(Big_hemo) #Creo un subset de mi dataframe libre de NA
df2 <- Big_hemo
df2 <- textshape::column_to_rownames(df2, loc=1) #Pongo el codigo como nombre de fila
df2 <- as.data.frame(df2) #Convierto eso en data frame
df2 <- subset(df2, select = -c(Estado_final)) # Hago un subset de mi dataframe de hemogramas y le quito la variable cualitativa de respuesta

## Intento correr un analisis de componentes
#respca <- prcomp(na.omit(df, scale = TRUE)) ## Me arrojaba error porque habian valores que tienden al infinito, ya no
## Tengo que ajustar los datos
df.center.scaled <- data.frame(apply(df2, 2, scale)) ## centro y escalo los datos para prepararlos para el pca
df.center.scaled ## Reviso los datos escalados en busqueda de valores con tendencia al infinito
df2 <- df.center.scaled

## ¿Que comportamiento tienen los parametros del hemograma de base? 

tble.s1a <- 
  df2 %>%
  select(Leucocitos_pre, Neutrofilos_pre, Linfocitos_pre, Monocitos_pre, Eosinofilos_pre, 
         Basofilos_pre, Granulocitos_pre, Granulocitos_pre_exp, Globulos_rojos_pre, Hemoglobina_pre, Hematocrito_pre, Plaquetas_pre,
         porcentaje_neutro_pre, porcentaje_linfo_pre, porcentaje_mono_pre, porcentaje_eosino_pre, porcentaje_baso_pre,
         porcentaje_granulo_pre, i_neu_linfo_pre, i_eos_linfo_pre, i_mono_linfo_pre, i_baso_linfo_pre, i_granu_linfo_pre,
         i_neu_granu_pre, i_eos_granu_pre, i_baso_granu_pre, i_eos_mono_pre, i_eos_neu_pre, i_eos_baso_pre,
         i_neu_mono_pre, i_neu_baso_pre, i_baso_mono_pre,i_monoeosneu_lin_pre,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(Leucocitos_pre = "Leucocitos pretratamiento", Neutrofilos_pre = "Neutrofilos pretratamiento", 
                 Linfocitos_pre = "Linfocitos pretratamiento", Monocitos_pre = "Monocitos pretratamiento", 
                 Eosinofilos_pre = "Eosinofilos pretratamiento", Basofilos_pre = "Basofilos pretratamiento", 
                 Granulocitos_pre = "Granulocitos inmaduros pretratamiento", Granulocitos_pre_exp = "Granulocitos pretratamiento exponenciados", Globulos_rojos_pre = "Globulos rojos pretratamiento", 
                 Hemoglobina_pre = "Hemoglobina pretratamiento", Hematocrito_pre = "Hematocrito pretratamiento", 
                 Plaquetas_pre = "Plaquetas pretratamiento", i_eos_baso_pre = "Indice eosinofilos/basofilos pretratamiento",
                 porcentaje_neutro_pre =  "% neutrofilos pretratamiento", porcentaje_linfo_pre = "% linfocitos pretratamiento", 
                 porcentaje_mono_pre = "% monocitos pretratamiento", porcentaje_eosino_pre = "% eosinofilos pretratamiento", 
                 porcentaje_baso_pre = "% basofilos pretratamiento", porcentaje_granulo_pre = "% granulocitos pretratamiento",
                 i_neu_linfo_pre = "Indice neutrofilos / linfocitos pretratamiento", i_eos_linfo_pre ="Indice eosinofilos / linfocitos pretratamiento", 
                 i_mono_linfo_pre = "Indice monocitos / linfocitos pretratamiento", i_baso_linfo_pre = "Indice basofilos / linfocitos pretratamiento", 
                 i_granu_linfo_pre = "Indice granulocitos exponenciados / linfocitos pretratamiento", i_neu_granu_pre = "Indice neutrofilos / granulocitos exponenciados pretratamiento",
                 i_eos_granu_pre = "Indice eosinofilos / granulocitos exponenciados pretratamiento", i_baso_granu_pre ="Indice basofilos / granulocitos exponenciados pretratamiento",
                 i_eos_mono_pre = "Indice eosinofilos / monocitos pretratamiento", i_eos_neu_pre = "Indice eosinofilos / neutrofilos pretratamiento",
                 i_neu_mono_pre = "Indice neutrofilos / monocitos pretratamiento", i_neu_baso_pre = "Indice neutrofilos / basofilos pretratamiento", 
                 i_baso_mono_pre = "Indice basofilos / monocitos pretratamiento",
                 i_monoeosneu_lin_pre = "Indice (monocitos + eosinofilos + neutrofilos) / lindfocitos)"),
  )  

tble.s1a

## ¿Que comportamiento tienen los parametros del hemograma postratamiento?

tble.s1b <- 
  df2 %>%
  select (Leucocitos_post, Neutrofilos_post, Linfocitos_post, Monocitos_post, Eosinofilos_post, 
          Basofilos_post, Granulocitos_post, Granulocitos_post_exp, Globulos_rojos_post, Hemoglobina_post, Hematocrito_post, 
          porcentaje_neutro_post, porcentaje_linfo_post, porcentaje_mono_post, porcentaje_eosino_post, porcentaje_baso_post,
          porcentaje_granulo_post, i_neu_linfo_post, i_eos_linfo_post, i_mono_linfo_post, i_baso_linfo_post, i_granu_linfo_post,
          i_neu_granu_post, i_eos_granu_post, i_baso_granu_post, i_eos_mono_post, i_eos_neu_post, i_eos_baso_post,
          i_neu_mono_post, i_neu_baso_post, i_baso_mono_post,
          i_monoeosneu_lin_post,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(Leucocitos_post = "Leucocitos postratamiento", Neutrofilos_post = "Neutrofilos postratamiento", 
                 Linfocitos_post = "Linfocitos postratamiento", Monocitos_post = "Monocitos postratamiento", 
                 Eosinofilos_post = "Eosinofilos postratamiento", Basofilos_post = "Basofilos postratamiento", 
                 Granulocitos_post = "Granulocitos inmaduros postratamiento", Granulocitos_post_exp = "Granulocitos postratamiento exponenciados", Globulos_rojos_post = "Globulos rojos postratamiento", 
                 Hemoglobina_post = "Hemoglobina postratamiento", Hematocrito_post = "Hematocrito postratamiento", 
                 Plaquetas_post = "Plaquetas postratamiento", i_eos_baso_post = "Indice eosinofilos/basofilos postratamiento",
                 porcentaje_neutro_post =  "% neutrofilos postratamiento", porcentaje_linfo_post = "% linfocitos postratamiento", 
                 porcentaje_mono_post = "% monocitos postratamiento", porcentaje_eosino_post = "% eosinofilos postratamiento", 
                 porcentaje_baso_post = "% basofilos postratamiento", porcentaje_granulo_post = "% granulocitos postratamiento",
                 i_neu_linfo_post = "Indice neutrofilos / linfocitos postratamiento", i_eos_linfo_post ="Indice eosinofilos / linfocitos postratamiento", 
                 i_mono_linfo_post = "Indice monocitos / linfocitos postratamiento", i_baso_linfo_post = "Indice basofilos / linfocitos postratamiento", 
                 i_granu_linfo_post = "Indice granulocitos exponenciados / linfocitos postratamiento", i_neu_granu_post = "Indice neutrofilos / granulocitos exponenciados postratamiento",
                 i_eos_granu_post = "Indice eosinofilos / granulocitos exponenciados postratamiento", i_baso_granu_post ="Indice basofilos / granulocitos exponenciados postratamiento",
                 i_eos_mono_post = "Indice eosinofilos / monocitos postratamiento", i_eos_neu_post = "Indice eosinofilos / neutrofilos postratamiento",
                 i_neu_mono_post = "Indice neutrofilos / monocitos postratamiento", i_neu_baso_post = "Indice neutrofilos / basofilos postratamiento", 
                 i_baso_mono_post = "Indice basofilos / monocitos postratamiento",
                 i_monoeosneu_lin_post = "Indice (monocitos + eosinofilos + neutrofilos) / lindfocitos postratamiento"),
  )  

tble.s1b


## ¿La forma en que varia influye?

tble.s1c <- 
  df2 %>%
  select (variacion_leucocitos, variacion_neutrofilos, variacion_linfocitos, variacion_monocitos,
          variacion_eosinofilos, variacion_basofilos, variacion_granulocitos, variacion_globulos_rojos,
          variacion_hemoglobina, variacion_hematocrito, variacion_porcentaje_neutro, variacion_porcentaje_linfo,
          variacion_porcentaje_mono, variacion_porcentaje_eosino, variacion_porcentaje_baso, variacion_porcentaje_granulo,
          variacion_i_neu_linfo, variacion_i_eos_linfo, variacion_i_mono_linfo, variacion_i_baso_linfo, 
          variacion_i_granu_linfo, variacion_i_neu_granu, variacion_i_eos_granu, variacion_i_baso_granu,
          variacion_i_eos_mono, variacion_i_eos_neu, variacion_i_eos_baso,
          variacion_i_neu_mono, variacion_i_neu_baso, variacion_i_baso_mono,
          variacion_i_monoeosneu_lin,
  ) %>%
  tbl_summary(
    missing_text = "Desconocido",
    label = list(variacion_leucocitos = "Ratio leucocitos post / pre",
                 variacion_neutrofilos = "Ratio neutrofilos post / pre", 
                 variacion_linfocitos = "Ratio linfocitos post / pre",
                 variacion_monocitos = "Ratio monocitos post / pre",
                 variacion_eosinofilos = "Ratio eosinofilos post / pre",
                 variacion_basofilos = "Ratio basofilos post / pre",
                 variacion_granulocitos = "Ratio granulocitos post / pre",
                 variacion_globulos_rojos = "Ratio globulos rojos post / pre",
                 variacion_hemoglobina = "Ratio hemoglobina post / pre",
                 variacion_hematocrito = "Ratio hematocrito post / pre",
                 variacion_porcentaje_neutro = "Ratio porcentaje de neutrofilos post / pre",
                 variacion_porcentaje_linfo = "Ratio porcentaje de linfocitos post / pre",
                 variacion_porcentaje_mono = "Ratio porcentaje de monocitos post / pre",
                 variacion_porcentaje_eosino = "Ratio porcentaje de eosinofilos post / pre",
                 variacion_porcentaje_baso = "Ratio porcentaje de basofilos post / pre",
                 variacion_porcentaje_granulo = "Ratio porcentaje de granulocitos post / pre",
                 variacion_i_neu_linfo = "Variacion indice neutrofilos/linfocitos (post / pre)", 
                 variacion_i_eos_linfo = "Variacion indice eosinofilos/linfocitos (post / pre)",
                 variacion_i_mono_linfo = "Variacion indice monocitos/linfocitos (post / pre)",
                 variacion_i_baso_linfo = "Variacion indice basofilos/linfocitos (post / pre)",
                 variacion_i_granu_linfo = "Variacion indice granulocitos exponenciados/linfocitos (post / pre)",
                 variacion_i_neu_granu = "Variacion indice neutrofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_granu = "Variacion indice eosinofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_baso_granu = "Variacion indice basofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_mono = "Variacion indice eosinofilos/monocitos (post / pre)",
                 variacion_i_eos_neu = "Variacion indice eosinofilos/neutrofilos (post / pre)",
                 variacion_i_eos_baso = "Variacion indice eosinofilos/basofilos (post / pre)",
                 variacion_i_neu_mono = "Variacion indice neutrofilos/monocitos (post / pre)", 
                 variacion_i_neu_baso = "Variacion indice neutrofilos/basofilos (post / pre)", 
                 variacion_i_baso_mono = "Variacion indice basofilos/monocitos (post / pre)",
                 variacion_i_monoeosneu_lin = "Variacion indice compuesto ((monocitos + neutrofilos + eosinofilos) / linfos ) (post / pre)"),
  )  

tble.s1c

## Corro el analisis de componentes principales
respca <- prcomp(na.omit(df2, scale = F))

#Miro los nombres de los resultados arrojados por el PCA
names (respca)

## Miro un resumen  general de los resultados del PCA
summary(respca)

#Scree plot
screeplot(respca, type="l",
          ylim=c(1,28),
          npcs = 16,
          pch = 16, col = "dodgerblue1", cex = 0.9,
          main="Screeplot del Analisis de Componentes principales 
          incluyendo los parametros del hemograma")
abline (1,0, col="red", lty=2)



## Como ya identifiqu? que los componentes 1 al 3 son los que mas influyen
## Miro la matriz de correlaciones de mis variables con esos PC
respca$rotation
# head(respca$rotation)


## Si quiero mirar en detalle el numero de componentes y dimensiones
#dim(respca$rotation)

## Si quiero mirar en detalle las desviaciones estandares de cada componente principal
#respca$sdev

## Si quiero ver en detalle la varianza explicada por cada componente
#respca$sdev^2

## Comandos para obtener otras variables de interes para mis graficas
get_pca(respca)
get_pca_var(respca)
get_pca_ind(respca)

## Scree plot mas bonito
fviz_eig(respca)

## Visualizacion de individuos y variables sobres los componentes
fviz_pca_var(respca)
fviz_pca_var(respca, col.var="cos2",
             geom.var=c("arrow", "text"),
             labelsize =4,
             repel=FALSE
)
fviz_pca_ind(respca)
fviz_pca_ind(respca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB","#E78800","#FC4E07"),
             repel = FALSE
)

## Veo cuales variables me contribuyen a la variabilidad?
#fviz_contrib(respca, choice = "ind")
fviz_contrib(respca, 
             choice = "var",
             axes = 1,
             top = 40)


fviz_contrib(respca, 
             choice = "var",
             axes=2, 
             top = 40
)

fviz_contrib(respca, 
             choice = "var",
             axes=3,
             top = 40
)

fviz_contrib(respca, 
             choice = "var",
             axes=4,
             top = 40
)

fviz_contrib(respca, 
             choice = "var",
             axes=5,
             top = 40
)

fviz_contrib(respca, 
             choice = "var",
             axes=6,
             top = 40
)

fviz_contrib(respca, 
             choice = "var",
             axes=7,
             top = 32
)

fviz_contrib(respca, 
             choice = "var",
             axes=8,
             top = 32
)

fviz_contrib(respca, 
             choice = "var",
             axes=9,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=10,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=11,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=12,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=13,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=14,
             top = 30
)

fviz_contrib(respca, 
             choice = "var",
             axes=15,
             top = 30
)

## Construccion del biplot
fviz_pca_biplot(respca, repel=FALSE,
                col.var="#2E9FDF",
                col.ind= "#696969"
)

# Si quisiera ver los 30 individuos que mas me contribuyen a las dos primeras componentes
#fviz_pca_biplot( respca, repel=FALSE, col.var="#2E9FDF", col.ind= "#696969", select.ind = list(contrib = 30)   )

#Comparo mi PCA contra un PCA estandarizado de otro paquete, obtuve los mismos resultados
#create_report(df)


## Si quiero graficar el PCA entre curas y fallas
df3 <- na.omit(Big_hemo)


library(ggsci)
#autoplot(respca)

autoplot(respca, data = df3, colour = 'Estado_final', loadings = TRUE, 
         loadings.colour = 'brown',
         repel = TRUE,
         loadings.label = FALSE, 
         loadings.label.size = 4) +
  scale_color_nejm()



autoplot(respca, data = df3, colour = 'Estado_final', loadings = TRUE, 
         loadings.colour = 'brown',
         repel = TRUE,
         loadings.label = F, 
         loadings.label.size = 2) +
  scale_color_nejm()

autoplot(respca, data = df3, colour = 'Estado_final', loadings = TRUE, 
         loadings.colour = '#7E6148B2',
         repel = TRUE,
         loadings.label = F, 
         loadings.label.size = 2) + 
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2"))

## Conclusion: PCA - No me sirve


### ¿Qué pasa si PCA es mas pequeño?

df4 <- subset(df.center.scaled, select = -c(porcentaje_neutro_pre, porcentaje_linfo_pre, porcentaje_mono_pre, porcentaje_eosino_pre, porcentaje_baso_pre,
                                            porcentaje_granulo_pre,
                                            porcentaje_neutro_post, porcentaje_linfo_post, porcentaje_mono_post, porcentaje_eosino_post, porcentaje_baso_post,
                                            porcentaje_granulo_post, 
                                            variacion_leucocitos, variacion_neutrofilos, variacion_linfocitos, variacion_monocitos,
                                            variacion_eosinofilos, variacion_basofilos, variacion_granulocitos, variacion_globulos_rojos,
                                            variacion_hemoglobina, variacion_hematocrito, variacion_porcentaje_neutro, variacion_porcentaje_linfo,
                                            variacion_porcentaje_mono, variacion_porcentaje_eosino, variacion_porcentaje_baso, variacion_porcentaje_granulo,
                                            variacion_i_neu_linfo, variacion_i_eos_linfo, variacion_i_mono_linfo, variacion_i_baso_linfo, 
                                            variacion_i_granu_linfo, variacion_i_neu_granu, variacion_i_eos_granu, variacion_i_baso_granu,
                                            variacion_i_eos_mono, variacion_i_eos_neu, variacion_i_eos_baso, variacion_i_monoeosneu_lin))   

## Corro el analisis de componentes principales
respca <- prcomp(na.omit(df4, scale = F))

#Miro los nombres de los resultados arrojados por el PCA
names (respca)

## Miro un resumen  general de los resultados del PCA
summary(respca)

#Scree plot
screeplot(respca, type="l", main="Screenplot for Hemo Data")
abline (1,0, col="red", lty=2)

## Como ya identifiqu? que los componentes 1 al 3 son los que mas influyen
## Miro la matriz de correlaciones de mis variables con esos PC
respca$rotation
# head(respca$rotation)

## Si quiero mirar en detalle el numero de componentes y dimensiones
#dim(respca$rotation)

## Si quiero mirar en detalle las desviaciones estandares de cada componente principal
#respca$sdev

## Si quiero ver en detalle la varianza explicada por cada componente
#respca$sdev^2

## Comandos para obtener otras variables de interes para mis graficas
get_pca(respca)
get_pca_var(respca)
get_pca_ind(respca)

## Scree plot mas bonito
fviz_eig(respca)

## Visualizacion de individuos y variables sobres los componentes
fviz_pca_var(respca)
fviz_pca_var(respca, col.var="cos2",
             geom.var=c("arrow", "text"),
             labelsize =4,
             repel=FALSE
)
fviz_pca_ind(respca)
fviz_pca_ind(respca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB","#E78800","#FC4E07"),
             repel = FALSE
)

## ?Cu?les individuos y variables me contribuyen a la variabilidad?
#fviz_contrib(respca, choice = "ind")
fviz_contrib(respca, choice = "var")
fviz_contrib(respca, 
             choice = "var",
             axes=2
)
fviz_contrib(respca, 
             choice = "var",
             axes=3
)
fviz_contrib(respca, 
             choice = "var",
             axes=4
)
fviz_contrib(respca, 
             choice = "var",
             axes=5
)

## Construccion del biplot
fviz_pca_biplot(respca, repel=FALSE,
                col.var="#2E9FDF",
                col.ind= "#696969"
)


#Comparo mi PCA contra un PCA estandarizado de otro paquete, obtuve los mismos resultados
#create_report(df)


## Si quiero graficar el PCA entre curas y fallas
df5 <- na.omit(Big_hemo)

#autoplot(respca)

autoplot(respca, data = df5, colour = 'Estado_final', loadings = TRUE, 
         loadings.colour = 'brown',
         repel = TRUE,
         loadings.label = TRUE, 
         loadings.label.size = 4)

## Aunque lo haga mas sencillo, sigue sin ser informativo



## Se puede hacer PLS-DA?? ---> Fuente para hacerlo https://mixomicsteam.github.io/Bookdown/plsda.html

# si quisiera reorganizar col-row - hemo_df_plsda <- as.data.frame(t(df2))


lista_plsda <- list(hemogramas = df.center.scaled, estado_fin = df$Estado_final)


X <- lista_plsda$hemogramas
Y <- lista_plsda$estado_fin

summary(Y) ## class summary
dim(X)

length(Y) ## length of class memebrship factor = number of samples

# For a quick start, we arbitrarily set the number of variables to select to 10 on each of the 2 components of PLS-DA
MyResult.splsda <- splsda(X, Y, keepX = c(10,10)) # 1 Run the method
plotIndiv(MyResult.splsda)                          # 2 Plot the samples (coloured by classes automatically)
plotVar(MyResult.splsda)                            # 3 Plot the variables
selectVar(MyResult.splsda, comp=1)$name   
selectVar(MyResult.splsda, comp=2)$name   

## Si lo que quiero es elegir variables por un punto de corte basado en la correlacion de las varaibles con cada componente, lo hago asi

MyResult.plsda <- plsda(X,Y) # 1 Run the method
plotIndiv(MyResult.plsda)    # 2 Plot the samples
plotVar(MyResult.plsda, cutoff = 0.5)      # 3 Plot the variables

auc.plsda <- auroc(MyResult.splsda) ## Me arroja curva ROC de mi modelo de clasificacion 
str(auc.plsda)

library(ggplot2)

# Assuming you have your ROC data in a data frame 'roc_data' as before
roc_data <- auc.plsda$graph.Comp2$data
roc_data$Specificity <- 1 - roc_data$Specificity  # Make sure Specificity is between 0 and 1

roc_plot <- ggplot(roc_data, aes(x = Specificity, y = Sensitivity, color = Outcome)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "grey") +
  theme_minimal(base_size = 18) +
  labs(
    x = "Specificity (%)",
    y = "Sensitivity (%)",
    title = "ROC Curve of PLS-DA using Comps 1 and 2",
    subtitle = "Definitive Cure vs Therapeutic Failure",
    color = "Outcome"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_reverse()  # Reverse the scale to display 100% specificity on the left

# Display the ROC plot
print(roc_plot)



## buscar pROC paquete ci.auc

plotLoadings(MyResult.splsda, contrib = 'max', method = 'mean') ## me deja ver matriz de correlaciones con cada desenlace
plotLoadings(MyResult.splsda, contrib = 'max', method = 'mean', comp = 2)


# Plot inicial
plotIndiv(MyResult.splsda, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, star = TRUE, title = 'sPLS-DA on SRBCT',
          X.label = 'PLS-DA 1', Y.label = 'PLS-DA 2')


## Ahora si yo quiero saber cuantos componentes elegir y cuantas variables elegir por componente, entonces deberia

MyResult.plsda2 <- plsda(X,Y, ncomp=10)
set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
MyPerf.plsda <- perf(MyResult.plsda2, validation = "Mfold", folds = 10, 
                     progressBar = FALSE, nrepeat = 50, 
                     dist=c("max.dist")) # we suggest nrepeat = 50, folds=10
plot(MyPerf.plsda, col = color.mixo(5), sd = T, legend.position = "horizontal", measure = c("overall"),
     overlay = c("measure"))
MyPerf.plsda$error.rate 
MyPerf.plsda$global.error ## Overall CV de 0,16 y 0,17 para componentes uno y dos
MyPerf.plsda$features$stable 

## Cuantas variables extraer?
list.keepX <- c(5:10,  seq(20, 100, 10))
list.keepX # to output the grid of values tested

set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
tune.splsda.srbct <- tune.splsda(X, Y, ncomp = 4, # we suggest to push ncomp a bit more, e.g. 4
                                 validation = 'Mfold',
                                 folds = 10, dist = 'max.dist', progressBar = FALSE,
                                 measure = "BER", test.keepX = list.keepX,
                                 nrepeat = 50)   # we suggest nrepeat = 50

error <- tune.splsda.srbct$error.rate
ncomp <- tune.splsda.srbct$choice.ncomp$ncomp # optimal number of components based on t-tests on the error rate
ncomp   

## Conclusion, recomienda solo usar el componente 1

select.keepX <- tune.splsda.srbct$choice.keepX[1:ncomp]  # optimal number of variables to select
select.keepX

## Recomienda extraer 5 variables de mi componente 1 Y 9 variables de mi componente 2


## Aqui va a mostrar el error eligiendo el numero de componentes sugerido
plot(tune.splsda.srbct, col = color.jet(ncomp))

#Based on those tuning results, we can run our final and tuned sPLS-DA model:
MyResult.splsda.final <- splsda(X, Y, ncomp = ncomp, keepX = select.keepX)
str(MyResult.splsda.final)

## Plot final
plotIndiv(MyResult.splsda.final, ind.names = FALSE, legend=TRUE,
          ellipse = TRUE, title="Resultado final - PLS-DA")


# Define a vector with your preferred colors
my_colors <- c("#4DBBD5B2", "#E64B35B2")

# Plot the final results using the plotIndiv function, attempting to pass the color vector
plotIndiv(MyResult.splsda.final, ind.names = FALSE, legend = TRUE, 
          ellipse = TRUE, title = "PLS-DA report", 
          col = my_colors, legend.title = "Outcome",
          legend.labels = c("Cure", "Failure"))

# Convertir los niveles del factor Y a inglés
levels(MyResult.splsda.final$Y) <- c("Cure", "Failure")

# Plot the final results using the plotIndiv function with increased font size for axis and legend
plotIndiv(MyResult.splsda.final, ind.names = FALSE, legend = TRUE, 
          ellipse = TRUE, title = "PLS-DA report", 
          col = my_colors, legend.title = "Outcome", 
          fontsize = 18)




# Variables finales
plotLoadings(MyResult.splsda.final, contrib = 'max', method = 'mean',  title = "PLS-DA - Variables explicativas del componente 1")
plotLoadings(MyResult.splsda.final, contrib = 'max', method = 'mean', comp = 2, title = "PLS-DA - Variables explicativas del componente 2")


## Me arroja curva ROC de mi modelo de clasificacion






## ANALISIS BIVARIADO DE LOS HEMOGRAMAS ##

tble4.1 <- 
  Big_hemo %>%
  select(Estado_final, Leucocitos_pre, Neutrofilos_pre, Linfocitos_pre, Monocitos_pre, Eosinofilos_pre, 
         Basofilos_pre, Granulocitos_pre, Granulocitos_pre_exp, Globulos_rojos_pre, Hemoglobina_pre, Hematocrito_pre, Plaquetas_pre,
         porcentaje_neutro_pre, porcentaje_linfo_pre, porcentaje_mono_pre, porcentaje_eosino_pre, porcentaje_baso_pre,
         porcentaje_granulo_pre, i_neu_linfo_pre, i_eos_linfo_pre, i_mono_linfo_pre, i_baso_linfo_pre, i_granu_linfo_pre,
         i_neu_granu_pre, i_eos_granu_pre, i_baso_granu_pre, i_eos_mono_pre, i_eos_neu_pre, i_eos_baso_pre, 
         i_neu_mono_pre, i_neu_baso_pre, i_baso_mono_pre, i_monoeosneu_lin_pre,
         Leucocitos_post, Neutrofilos_post, Linfocitos_post, Monocitos_post, Eosinofilos_post, 
         Basofilos_post, Granulocitos_post, Granulocitos_post_exp, Globulos_rojos_post, Hemoglobina_post, Hematocrito_post, 
         porcentaje_neutro_post, porcentaje_linfo_post, porcentaje_mono_post, porcentaje_eosino_post, porcentaje_baso_post,
         porcentaje_granulo_post, i_neu_linfo_post, i_eos_linfo_post, i_mono_linfo_post, i_baso_linfo_post, i_granu_linfo_post,
         i_neu_granu_post, i_eos_granu_post, i_baso_granu_post, i_eos_mono_post, i_eos_neu_post, i_eos_baso_post,
         i_neu_mono_post, i_neu_baso_post, i_baso_mono_post, i_monoeosneu_lin_post,
         variacion_leucocitos, variacion_neutrofilos, variacion_linfocitos, variacion_monocitos,
         variacion_eosinofilos, variacion_basofilos, variacion_granulocitos, variacion_globulos_rojos,
         variacion_hemoglobina, variacion_hematocrito, variacion_porcentaje_neutro, variacion_porcentaje_linfo,
         variacion_porcentaje_mono, variacion_porcentaje_eosino, variacion_porcentaje_baso, variacion_porcentaje_granulo,
         variacion_i_neu_linfo, variacion_i_eos_linfo, variacion_i_mono_linfo, variacion_i_baso_linfo, 
         variacion_i_granu_linfo, variacion_i_neu_granu, variacion_i_eos_granu, variacion_i_baso_granu,
         variacion_i_eos_mono, variacion_i_eos_neu, variacion_i_eos_baso,
         variacion_i_neu_mono, variacion_i_neu_baso, variacion_i_baso_mono,
         variacion_i_monoeosneu_lin,
         ) %>%
  tbl_summary(
    by = Estado_final,
    missing_text = "Desconocido",
    label = list(Leucocitos_pre = "Leucocitos pretratamiento", Neutrofilos_pre = "Neutrofilos pretratamiento", 
                 Linfocitos_pre = "Linfocitos pretratamiento", Monocitos_pre = "Monocitos pretratamiento", 
                 Eosinofilos_pre = "Eosinofilos pretratamiento", Basofilos_pre = "Basofilos pretratamiento", 
                 Granulocitos_pre = "Granulocitos inmaduros pretratamiento", Granulocitos_pre_exp = "Granulocitos pretratamiento exponenciados", Globulos_rojos_pre = "Globulos rojos pretratamiento", 
                 Hemoglobina_pre = "Hemoglobina pretratamiento", Hematocrito_pre = "Hematocrito pretratamiento", 
                 Plaquetas_pre = "Plaquetas pretratamiento", i_eos_baso_pre = "Indice eosinofilos/basofilos pretratamiento",
                 porcentaje_neutro_pre =  "% neutrofilos pretratamiento", porcentaje_linfo_pre = "% linfocitos pretratamiento", 
                 porcentaje_mono_pre = "% monocitos pretratamiento", porcentaje_eosino_pre = "% eosinofilos pretratamiento", 
                 porcentaje_baso_pre = "% basofilos pretratamiento", porcentaje_granulo_pre = "% granulocitos pretratamiento",
                 i_neu_linfo_pre = "Indice neutrofilos / linfocitos pretratamiento", i_eos_linfo_pre ="Indice eosinofilos / linfocitos pretratamiento", 
                 i_mono_linfo_pre = "Indice monocitos / linfocitos pretratamiento", i_baso_linfo_pre = "Indice basofilos / linfocitos pretratamiento", 
                 i_granu_linfo_pre = "Indice granulocitos exponenciados / linfocitos pretratamiento", i_neu_granu_pre = "Indice neutrofilos / granulocitos exponenciados pretratamiento",
                 i_eos_granu_pre = "Indice eosinofilos / granulocitos exponenciados pretratamiento", i_baso_granu_pre ="Indice basofilos / granulocitos exponenciados pretratamiento",
                 i_eos_mono_pre = "Indice eosinofilos / monocitos pretratamiento", i_eos_neu_pre = "Indice eosinofilos / neutrofilos pretratamiento",
                 i_neu_mono_pre = "Indice neutrofilos / monocitos pretratamiento", i_neu_baso_pre = "Indice neutrofilos / basofilos pretratamiento", 
                 i_baso_mono_pre = "Indice basofilos / monocitos pretratamiento",
                 i_monoeosneu_lin_pre = "Indice (monocitos + eosinofilos + neutrofilos) / linfocitos - Pretratamiento)", 
                 Leucocitos_post = "Leucocitos postratamiento", Neutrofilos_post = "Neutrofilos postratamiento", 
                 Linfocitos_post = "Linfocitos postratamiento", Monocitos_post = "Monocitos postratamiento", 
                 Eosinofilos_post = "Eosinofilos postratamiento", Basofilos_post = "Basofilos postratamiento", 
                 Granulocitos_post = "Granulocitos inmaduros postratamiento", Granulocitos_post_exp = "Granulocitos postratamiento exponenciados", Globulos_rojos_post = "Globulos rojos postratamiento", 
                 Hemoglobina_post = "Hemoglobina postratamiento", Hematocrito_post = "Hematocrito postratamiento", 
                 Plaquetas_post = "Plaquetas postratamiento", i_eos_baso_post = "Indice eosinofilos/basofilos postratamiento",
                 porcentaje_neutro_post =  "% neutrofilos postratamiento", porcentaje_linfo_post = "% linfocitos postratamiento", 
                 porcentaje_mono_post = "% monocitos postratamiento", porcentaje_eosino_post = "% eosinofilos postratamiento", 
                 porcentaje_baso_post = "% basofilos postratamiento", porcentaje_granulo_post = "% granulocitos postratamiento",
                 i_neu_linfo_post = "Indice neutrofilos / linfocitos postratamiento", i_eos_linfo_post ="Indice eosinofilos / linfocitos postratamiento", 
                 i_mono_linfo_post = "Indice monocitos / linfocitos postratamiento", i_baso_linfo_post = "Indice basofilos / linfocitos postratamiento", 
                 i_granu_linfo_post = "Indice granulocitos exponenciados / linfocitos postratamiento", i_neu_granu_post = "Indice neutrofilos / granulocitos exponenciados postratamiento",
                 i_eos_granu_post = "Indice eosinofilos / granulocitos exponenciados postratamiento", i_baso_granu_post ="Indice basofilos / granulocitos exponenciados postratamiento",
                 i_eos_mono_post = "Indice eosinofilos / monocitos postratamiento", i_eos_neu_post = "Indice eosinofilos / neutrofilos postratamiento",
                 i_neu_mono_post = "Indice neutrofilos / monocitos postratamiento", i_neu_baso_post = "Indice neutrofilos / basofilos postratamiento", 
                 i_baso_mono_post = "Indice basofilos / monocitos postratamiento",
                 i_monoeosneu_lin_post = "Indice (monocitos + eosinofilos + neutrofilos) / lindocitos - Postratamiento",
                 variacion_leucocitos = "Ratio leucocitos post / pre",
                 variacion_neutrofilos = "Ratio neutrofilos post / pre", 
                 variacion_linfocitos = "Ratio linfocitos post / pre",
                 variacion_monocitos = "Ratio monocitos post / pre",
                 variacion_eosinofilos = "Ratio eosinofilos post / pre",
                 variacion_basofilos = "Ratio basofilos post / pre",
                 variacion_granulocitos = "Ratio granulocitos post / pre",
                 variacion_globulos_rojos = "Ratio globulos rojos post / pre",
                 variacion_hemoglobina = "Ratio hemoglobina post / pre",
                 variacion_hematocrito = "Ratio hematocrito post / pre",
                 variacion_porcentaje_neutro = "Ratio porcentaje de neutrofilos post / pre",
                 variacion_porcentaje_linfo = "Ratio porcentaje de linfocitos post / pre",
                 variacion_porcentaje_mono = "Ratio porcentaje de monocitos post / pre",
                 variacion_porcentaje_eosino = "Ratio porcentaje de eosinofilos post / pre",
                 variacion_porcentaje_baso = "Ratio porcentaje de basofilos post / pre",
                 variacion_porcentaje_granulo = "Ratio porcentaje de granulocitos post / pre",
                 variacion_i_neu_linfo = "Variacion indice neutrofilos/linfocitos (post / pre)", 
                 variacion_i_eos_linfo = "Variacion indice eosinofilos/linfocitos (post / pre)",
                 variacion_i_mono_linfo = "Variacion indice monocitos/linfocitos (post / pre)",
                 variacion_i_baso_linfo = "Variacion indice basofilos/linfocitos (post / pre)",
                 variacion_i_granu_linfo = "Variacion indice granulocitos exponenciados/linfocitos (post / pre)",
                 variacion_i_neu_granu = "Variacion indice neutrofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_granu = "Variacion indice eosinofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_baso_granu = "Variacion indice basofilos/granulocitos exponenciados (post / pre)",
                 variacion_i_eos_mono = "Variacion indice eosinofilos/monocitos (post / pre)",
                 variacion_i_eos_neu = "Variacion indice eosinofilos/neutrofilos (post / pre)",
                 variacion_i_eos_baso = "Variacion indice eosinofilos/basofilos (post / pre)",
                 variacion_i_neu_mono = "Variacion indice neutrofilos/monocitos (post / pre)", 
                 variacion_i_neu_baso = "Variacion indice neutrofilos/basofilos (post / pre)", 
                 variacion_i_baso_mono = "Variacion indice basofilos/monocitos (post / pre)",
                 variacion_i_monoeosneu_lin = "Variacion indice compuesto ((monocitos + neutrofilos + eosinofilos) / linfos ) (post / pre)"),
  ) %>% 
  add_p() %>% 
  add_q() 
   

tble4.1

table4.2a <- (tbl_uvregression(base_regre_centrada[c("Leucocitos_pre", "Neutrofilos_pre", "Linfocitos_pre", "Monocitos_pre", "Eosinofilos_pre", 
                                                            "Basofilos_pre" , "Granulocitos_pre", "Granulocitos_pre_exp" ,  "Globulos_rojos_pre" , "Hemoglobina_pre", "Hematocrito_pre" , "Plaquetas_pre",
                                                            "porcentaje_neutro_pre" , "porcentaje_linfo_pre" , "porcentaje_mono_pre" , "porcentaje_eosino_pre" , "porcentaje_baso_pre" , 
                                                            "porcentaje_granulo_pre" , "i_neu_linfo_pre" , "i_eos_linfo_pre" , "i_mono_linfo_pre" , "i_baso_linfo_pre" , "i_granu_linfo_pre" , 
                                                            "i_neu_granu_pre" , "i_eos_granu_pre" , "i_baso_granu_pre" , "i_eos_mono_pre" , "i_eos_neu_pre" , "i_eos_baso_pre" , 
                                                            "i_neu_mono_pre" , "i_neu_baso_pre" , "i_baso_mono_pre" , "i_monoeosneu_lin_pre", "Estado_final")],
                                          method = glm,
                                          y = Estado_final,
                                          method.args = list(family = binomial),
                                          label = list(Leucocitos_pre = "Leucocitos pretratamiento", Neutrofilos_pre = "Neutrofilos pretratamiento", 
                                                       Linfocitos_pre = "Linfocitos pretratamiento", Monocitos_pre = "Monocitos pretratamiento", 
                                                       Eosinofilos_pre = "Eosinofilos pretratamiento", Basofilos_pre = "Basofilos pretratamiento", 
                                                       Granulocitos_pre = "Granulocitos inmaduros pretratamiento", Granulocitos_pre_exp = "Granulocitos pretratamiento exponenciados", Globulos_rojos_pre = "Globulos rojos pretratamiento", 
                                                       Hemoglobina_pre = "Hemoglobina pretratamiento", Hematocrito_pre = "Hematocrito pretratamiento", 
                                                       Plaquetas_pre = "Plaquetas pretratamiento", i_eos_baso_pre = "Indice eosinofilos/basofilos pretratamiento",
                                                       porcentaje_neutro_pre =  "% neutrofilos pretratamiento", porcentaje_linfo_pre = "% linfocitos pretratamiento", 
                                                       porcentaje_mono_pre = "% monocitos pretratamiento", porcentaje_eosino_pre = "% eosinofilos pretratamiento", 
                                                       porcentaje_baso_pre = "% basofilos pretratamiento", porcentaje_granulo_pre = "% granulocitos pretratamiento",
                                                       i_neu_linfo_pre = "Indice neutrofilos / linfocitos pretratamiento", i_eos_linfo_pre ="Indice eosinofilos / linfocitos pretratamiento", 
                                                       i_mono_linfo_pre = "Indice monocitos / linfocitos pretratamiento", i_baso_linfo_pre = "Indice basofilos / linfocitos pretratamiento", 
                                                       i_granu_linfo_pre = "Indice granulocitos exponenciados / linfocitos pretratamiento", i_neu_granu_pre = "Indice neutrofilos / granulocitos exponenciados pretratamiento",
                                                       i_eos_granu_pre = "Indice eosinofilos / granulocitos exponenciados pretratamiento", i_baso_granu_pre ="Indice basofilos / granulocitos exponenciados pretratamiento",
                                                       i_eos_mono_pre = "Indice eosinofilos / monocitos pretratamiento", i_eos_neu_pre = "Indice eosinofilos / neutrofilos pretratamiento",
                                                       i_neu_mono_pre = "Indice neutrofilos / monocitos pretratamiento", i_neu_baso_pre = "Indice neutrofilos / basofilos pretratamiento", 
                                                       i_baso_mono_pre = "Indice basofilos / monocitos pretratamiento",
                                                       i_monoeosneu_lin_pre = "Indice (monocitos + eosinofilos + neutrofilos) / linfocitos - Pretratamiento)" 
                                                       ),
                                          exponentiate = TRUE,
                                          pvalue_fun = ~style_pvalue(.x, digits = 2)))

table4.2a

## Bivariado con regresiones logísticas (Porque intenté con log binomial y me generó problemas de convergencia al trabajarla como numerica)

table4.2b <- (tbl_uvregression(base_regre_centrada[c("Leucocitos_post" , "Neutrofilos_post" , "Linfocitos_post" , "Monocitos_post" , "Eosinofilos_post" , 
                                                    "Basofilos_post" , "Granulocitos_post" , "Granulocitos_post_exp" , "Globulos_rojos_post" , "Hemoglobina_post" , "Hematocrito_post" , 
                                                    "porcentaje_neutro_post" , "porcentaje_linfo_post" , "porcentaje_mono_post" , "porcentaje_eosino_post" , "porcentaje_baso_post" , 
                                                    "porcentaje_granulo_post" , "i_neu_linfo_post" , "i_eos_linfo_post" , "i_mono_linfo_post" , "i_baso_linfo_post" , "i_granu_linfo_post" , 
                                                    "i_neu_granu_post" , "i_eos_granu_post" , "i_baso_granu_post" , "i_eos_mono_post" , "i_eos_neu_post" , "i_eos_baso_post" , 
                                                    "i_neu_mono_post" , "i_neu_baso_post" , "i_baso_mono_post" , "i_monoeosneu_lin_post" , 
                                                    "Estado_final")],
                              method = glm,
                              y = Estado_final,
                              method.args = list(family = binomial),
                              label = list(Leucocitos_post = "Leucocitos postratamiento", Neutrofilos_post = "Neutrofilos postratamiento", 
                                           Linfocitos_post = "Linfocitos postratamiento", Monocitos_post = "Monocitos postratamiento", 
                                           Eosinofilos_post = "Eosinofilos postratamiento", Basofilos_post = "Basofilos postratamiento", 
                                           Granulocitos_post = "Granulocitos inmaduros postratamiento", Granulocitos_post_exp = "Granulocitos postratamiento exponenciados", Globulos_rojos_post = "Globulos rojos postratamiento", 
                                           Hemoglobina_post = "Hemoglobina postratamiento", Hematocrito_post = "Hematocrito postratamiento", 
                                           Plaquetas_post = "Plaquetas postratamiento", i_eos_baso_post = "Indice eosinofilos/basofilos postratamiento",
                                           porcentaje_neutro_post =  "% neutrofilos postratamiento", porcentaje_linfo_post = "% linfocitos postratamiento", 
                                           porcentaje_mono_post = "% monocitos postratamiento", porcentaje_eosino_post = "% eosinofilos postratamiento", 
                                           porcentaje_baso_post = "% basofilos postratamiento", porcentaje_granulo_post = "% granulocitos postratamiento",
                                           i_neu_linfo_post = "Indice neutrofilos / linfocitos postratamiento", i_eos_linfo_post ="Indice eosinofilos / linfocitos postratamiento", 
                                           i_mono_linfo_post = "Indice monocitos / linfocitos postratamiento", i_baso_linfo_post = "Indice basofilos / linfocitos postratamiento", 
                                           i_granu_linfo_post = "Indice granulocitos exponenciados / linfocitos postratamiento", i_neu_granu_post = "Indice neutrofilos / granulocitos exponenciados postratamiento",
                                           i_eos_granu_post = "Indice eosinofilos / granulocitos exponenciados postratamiento", i_baso_granu_post ="Indice basofilos / granulocitos exponenciados postratamiento",
                                           i_eos_mono_post = "Indice eosinofilos / monocitos postratamiento", i_eos_neu_post = "Indice eosinofilos / neutrofilos postratamiento",
                                           i_neu_mono_post = "Indice neutrofilos / monocitos postratamiento", i_neu_baso_post = "Indice neutrofilos / basofilos postratamiento", 
                                           i_baso_mono_post = "Indice basofilos / monocitos postratamiento",
                                           i_monoeosneu_lin_post = "Indice (monocitos + eosinofilos + neutrofilos) / linfocitos - Postratamiento"
                                           ),
                              exponentiate = TRUE,
                              pvalue_fun = ~style_pvalue(.x, digits = 2)))

table4.2b 


table4.2c <- (tbl_uvregression(base_regre_centrada[c("variacion_leucocitos" , "variacion_neutrofilos" , "variacion_linfocitos" , "variacion_monocitos" , 
                                                    "variacion_eosinofilos" , "variacion_basofilos" , "variacion_granulocitos" , "variacion_globulos_rojos" , 
                                                    "variacion_hemoglobina" , "variacion_hematocrito" , "variacion_porcentaje_neutro" , "variacion_porcentaje_linfo" , 
                                                    "variacion_porcentaje_mono" , "variacion_porcentaje_eosino" , "variacion_porcentaje_baso" , "variacion_porcentaje_granulo" , 
                                                    "variacion_i_neu_linfo" , "variacion_i_eos_linfo" , "variacion_i_mono_linfo" , "variacion_i_baso_linfo" , 
                                                    "variacion_i_granu_linfo" , "variacion_i_neu_granu" , "variacion_i_eos_granu" , "variacion_i_baso_granu" , 
                                                    "variacion_i_eos_mono" , "variacion_i_eos_neu" , "variacion_i_eos_baso" , 
                                                    "variacion_i_neu_mono" , "variacion_i_neu_baso" , "variacion_i_baso_mono" , "variacion_i_monoeosneu_lin", 
                                                    "Estado_final")],
                              method = glm,
                              y = Estado_final,
                              method.args = list(family = binomial),
                              label = list(variacion_leucocitos = "Ratio leucocitos post / pre",
                                           variacion_neutrofilos = "Ratio neutrofilos post / pre", 
                                           variacion_linfocitos = "Ratio linfocitos post / pre",
                                           variacion_monocitos = "Ratio monocitos post / pre",
                                           variacion_eosinofilos = "Ratio eosinofilos post / pre",
                                           variacion_basofilos = "Ratio basofilos post / pre",
                                           variacion_granulocitos = "Ratio granulocitos post / pre",
                                           variacion_globulos_rojos = "Ratio globulos rojos post / pre",
                                           variacion_hemoglobina = "Ratio hemoglobina post / pre",
                                           variacion_hematocrito = "Ratio hematocrito post / pre",
                                           variacion_porcentaje_neutro = "Ratio porcentaje de neutrofilos post / pre",
                                           variacion_porcentaje_linfo = "Ratio porcentaje de linfocitos post / pre",
                                           variacion_porcentaje_mono = "Ratio porcentaje de monocitos post / pre",
                                           variacion_porcentaje_eosino = "Ratio porcentaje de eosinofilos post / pre",
                                           variacion_porcentaje_baso = "Ratio porcentaje de basofilos post / pre",
                                           variacion_porcentaje_granulo = "Ratio porcentaje de granulocitos post / pre",
                                           variacion_i_neu_linfo = "Variacion indice neutrofilos/linfocitos (post / pre)", 
                                           variacion_i_eos_linfo = "Variacion indice eosinofilos/linfocitos (post / pre)",
                                           variacion_i_mono_linfo = "Variacion indice monocitos/linfocitos (post / pre)",
                                           variacion_i_baso_linfo = "Variacion indice basofilos/linfocitos (post / pre)",
                                           variacion_i_granu_linfo = "Variacion indice granulocitos exponenciados/linfocitos (post / pre)",
                                           variacion_i_neu_granu = "Variacion indice neutrofilos/granulocitos exponenciados (post / pre)",
                                           variacion_i_eos_granu = "Variacion indice eosinofilos/granulocitos exponenciados (post / pre)",
                                           variacion_i_baso_granu = "Variacion indice basofilos/granulocitos exponenciados (post / pre)",
                                           variacion_i_eos_mono = "Variacion indice eosinofilos/monocitos (post / pre)",
                                           variacion_i_eos_neu = "Variacion indice eosinofilos/neutrofilos (post / pre)",
                                           variacion_i_eos_baso = "Variacion indice eosinofilos/basofilos (post / pre)",
                                           variacion_i_neu_mono = "Variacion indice neutrofilos/monocitos (post / pre)", 
                                           variacion_i_neu_baso = "Variacion indice neutrofilos/basofilos (post / pre)", 
                                           variacion_i_baso_mono = "Variacion indice basofilos/monocitos (post / pre)",
                                           variacion_i_monoeosneu_lin = "Variacion indice compuesto ((monocitos + neutrofilos + eosinofilos) / linfos ) (post / pre)"
                              ),
                              exponentiate = TRUE,
                              pvalue_fun = ~style_pvalue(.x, digits = 2)))

table4.2c 

## Tras completar el an?lisis bivariado de los par?metros del hemograma se seleccionaron 
## para la construcci?n de modelos multivariados, solo aquellos que fueron sugeridos por el PLS-DA
## y adem?s mostraron un grado importante de significancia estad?stica (valor p < 0.1 y/o valor q ??? 0,3)
## en las tablas comparativas.  

## Del hemograma pre-tratamiento se seleccionaron el conteo absoluto de granulocitos y el conteo de plaquetas. 
## Del hemograma post-tratamiento se seleccionaron el conteo absoluto y el porcentaje de eosin?filos, 
## el ?ndice eosin?filos/neutr?filos, el ?ndice eosin?filos/granulocitos inmaduros exponenciados 
## y la raz?n de cambio de los monocitos



######################################################
#### CONSTRUCCION DE MODELOS DE REGRESION MULTIVARIADA
######################################################

base_hemo_finales <- data.frame(
  granulocitos_pre_cen = base_regre_centrada$Granulocitos_pre,
  plaquetas_pre_cen = base_regre_centrada$Plaquetas_pre,
  eosinofilos_post_cen = base_regre_centrada$Eosinofilos_post,
  porc_eos_post_cen = base_regre_centrada$porcentaje_eosino_post,  
  i_eos_neu_post_cen = base_regre_centrada$i_eos_neu_post,
  i_eos_gran_exp_post_cen = base_regre_centrada$i_eos_granu_post,
  variacion_mono_cen = base_regre_centrada$variacion_monocitos
)
  
cor(base_hemo_finales, method = "pearson", use = "complete.obs")

# Traducir los niveles de la variable estado_final a inglés
levels(gran_base_resumen$estado_final) <- c("Cure", "Failure")


scatter_hemo_finales <- base_hemo_finales %>% mutate (estado_final = factor(gran_base_resumen$estado_final)) %>% 
  ggpairs(columns = c("granulocitos_pre_cen", "plaquetas_pre_cen", 
                      "eosinofilos_post_cen", "porc_eos_post_cen",
                      "i_eos_neu_post_cen", "i_eos_gran_exp_post_cen", "variacion_mono_cen"), 
          aes(color = estado_final),
          title = "Correlation between selected hemogram parameters for multivariate models",
          columnLabels = c("Granulocytes", "Platelets", 
                           "Eosinophils", "% Eosinophils", 
                           "Eos/Neu", "Eos/Gra", 
                           "Monocytes")
  ) 

for(i in 1:scatter_hemo_finales$nrow) {
  for(j in 1:scatter_hemo_finales$ncol){
    scatter_hemo_finales[i,j] <- scatter_hemo_finales[i,j] + 
      scale_fill_manual(values=c("#4DBBD5B2", "#E64B35B2")) +
      scale_color_manual(values=c("#4DBBD5B2", "#E64B35B2"))  
  }
}


scatter_hemo_finales


## Selecci?n de puntos de corte parametros del hemograma sin centrar

## Granulocitos pre
## Bootstrap
pc_granulocitos_pre <- cutpointr(data = Big_hemo, x = Granulocitos_pre , class = Estado_final,
                                 pos_class = "Falla terapeutica",
                                 direction = "<=",
                                 method = maximize_boot_metric,
                                 boot_runs = 10,
                                 na.rm = TRUE) 


summary(pc_granulocitos_pre)
plot(pc_granulocitos_pre)

## Youden
pc_granulocitos_pre2 <- cutpointr(data = Big_hemo, x = Granulocitos_pre , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = "<=",
                                  method = oc_youden_kernel,
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_granulocitos_pre2)
plot(pc_granulocitos_pre2)

## Manual
pc_granulocitos_pre3 <- cutpointr(data = Big_hemo, x = Granulocitos_pre , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = "<=",
                                  method = oc_manual, 
                                  cutpoint = 0.02,
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_granulocitos_pre3)
plot(pc_granulocitos_pre3)



## Plaquetas
## Bootstrap
pc_plaquetas_pre <- cutpointr(data = Big_hemo, x = Plaquetas_pre , class = Estado_final,
                                 pos_class = "Falla terapeutica",
                                 direction = "<=",
                                 method = maximize_boot_metric,
                                 boot_runs = 10,
                                 na.rm = TRUE)


summary(pc_plaquetas_pre)
plot(pc_plaquetas_pre)

## Youden
pc_plaquetas_pre2 <- cutpointr(data = Big_hemo, x = Plaquetas_pre , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = "<=",
                                  method = oc_youden_kernel,
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_plaquetas_pre2)
plot(pc_plaquetas_pre2)

## Manual
pc_plaquetas_pre3 <- cutpointr(data = Big_hemo, x = Plaquetas_pre , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = "<=",
                                  method = oc_manual, 
                                  cutpoint = 250,
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_plaquetas_pre3)
plot(pc_plaquetas_pre3)

## Conteo eosino post
## Bootstrap
pc_eosinofilos_post <- cutpointr(data = Big_hemo, x = Eosinofilos_post , class = Estado_final,
                              pos_class = "Falla terapeutica",
                              direction = ">=",
                              method = maximize_boot_metric, 
                              na.rm = TRUE,
                              boot_runs = 10)


summary(pc_eosinofilos_post)
plot(pc_eosinofilos_post)

## Youden
pc_eosinofilos_post2 <- cutpointr(data = Big_hemo, x = Eosinofilos_post , class = Estado_final,
                               pos_class = "Falla terapeutica",
                               direction = ">=",
                               method = oc_youden_kernel,
                               boot_runs = 10,
                               na.rm = TRUE)

summary(pc_eosinofilos_post2)
plot(pc_eosinofilos_post2)

## Manual
pc_eosinofilos_post3 <- cutpointr(data = Big_hemo, x = Eosinofilos_post , class = Estado_final,
                               pos_class = "Falla terapeutica",
                               direction = ">=",
                               method = oc_manual, 
                               boot_runs = 10,
                               cutpoint = 0.9,
                               na.rm = TRUE)

summary(pc_eosinofilos_post3)
plot(pc_eosinofilos_post3)


## Porcentaje eosinofilos postratamiento
## Bootstrap
pc_porc_eosinofilos_post <- cutpointr(data = Big_hemo, x = porcentaje_eosino_post , class = Estado_final,
                                 pos_class = "Falla terapeutica",
                                 direction = ">=",
                                 method = maximize_boot_metric,
                                 boot_runs = 10, 
                                 na.rm = TRUE)


summary(pc_porc_eosinofilos_post)
plot(pc_porc_eosinofilos_post)

## Youden
pc_porc_eosinofilos_post2 <- cutpointr(data = Big_hemo, x = porcentaje_eosino_post , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = ">=",
                                  method = oc_youden_kernel, 
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_porc_eosinofilos_post2)
plot(pc_porc_eosinofilos_post2)

## Manual
pc_porc_eosinofilos_post3 <- cutpointr(data = Big_hemo, x = porcentaje_eosino_post , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = ">=",
                                  method = oc_manual, 
                                  cutpoint = 14,
                                  boot_runs = 10,
                                  na.rm = TRUE)

summary(pc_porc_eosinofilos_post3)
plot(pc_porc_eosinofilos_post3)


## Indice eosin?filos neutr?filos
## Bootstrap
pc_i_eos_neu_post <- cutpointr(data = Big_hemo, x = i_eos_neu_post , class = Estado_final,
                                      pos_class = "Falla terapeutica",
                                      direction = ">=",
                                      method = maximize_boot_metric,
                                      boot_runs = 10, 
                                      na.rm = TRUE)


summary(pc_i_eos_neu_post)
plot(pc_i_eos_neu_post)

## Youden
pc_i_eos_neu_post2 <- cutpointr(data = Big_hemo, x = i_eos_neu_post , class = Estado_final,
                                       pos_class = "Falla terapeutica",
                                       direction = ">=",
                                       method = oc_youden_kernel,
                                       boot_runs = 10, 
                                       na.rm = TRUE)

summary(pc_i_eos_neu_post2)
plot(pc_i_eos_neu_post2)

## Manual
pc_i_eos_neu_post3 <- cutpointr(data = Big_hemo, x = i_eos_neu_post , class = Estado_final,
                                       pos_class = "Falla terapeutica",
                                       direction = ">=",
                                       method = oc_manual, 
                                       cutpoint = 0.3,
                                       boot_runs = 10,
                                       na.rm = TRUE)

summary(pc_i_eos_neu_post3)
plot(pc_i_eos_neu_post3)


## Indice eosin?filos granulos exp
## Bootstrap
pc_i_eos_granu_post <- cutpointr(data = Big_hemo, x = i_eos_granu_post , class = Estado_final,
                               pos_class = "Falla terapeutica",
                               direction = ">=",
                               method = maximize_boot_metric,
                               boot_runs = 10, 
                               na.rm = TRUE)


summary(pc_i_eos_granu_post)
plot(pc_i_eos_granu_post)

## Youden
pc_i_eos_granu_post2 <- cutpointr(data = Big_hemo, x = i_eos_granu_post , class = Estado_final,
                                pos_class = "Falla terapeutica",
                                direction = ">=",
                                method = oc_youden_kernel,
                                boot_runs = 10, 
                                na.rm = TRUE)

summary(pc_i_eos_granu_post2)
plot(pc_i_eos_granu_post2)

## Manual
pc_i_eos_granu_post3 <- cutpointr(data = Big_hemo, x = i_eos_granu_post , class = Estado_final,
                                pos_class = "Falla terapeutica",
                                direction = ">=",
                                method = oc_manual, 
                                cutpoint = 0.7,
                                boot_runs = 10,
                                na.rm = TRUE)

summary(pc_i_eos_granu_post3)
plot(pc_i_eos_granu_post3)

## Variacion de los monocitos

## Bootstrap
pc_variacion_monocitos <- cutpointr(data = Big_hemo, x = variacion_monocitos , class = Estado_final,
                                 pos_class = "Falla terapeutica",
                                 direction = ">=",
                                 method = maximize_boot_metric,
                                 boot_runs = 10, 
                                 na.rm = TRUE)


summary(pc_variacion_monocitos)
plot(pc_variacion_monocitos)

## Youden
pc_variacion_monocitos2 <- cutpointr(data = Big_hemo, x = variacion_monocitos , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = ">=",
                                  method = oc_youden_kernel,
                                  boot_runs = 10, 
                                  na.rm = TRUE)

summary(pc_variacion_monocitos2)
plot(pc_variacion_monocitos2)

## Manual
pc_variacion_monocitos3 <- cutpointr(data = Big_hemo, x = variacion_monocitos , class = Estado_final,
                                  pos_class = "Falla terapeutica",
                                  direction = ">=",
                                  method = oc_manual, 
                                  cutpoint = 1.1,
                                  boot_runs = 10,
                                  na.rm = TRUE
                                  )

summary(pc_variacion_monocitos3)
plot(pc_variacion_monocitos3)




## Construyo la base de la regresion final 

## Variables clinicas importantes:
  ## Sexo, Edad, Tiempo evolucion, Tipo lesion pre
  ## Linfadenopatia pre, Infxn concomitante,
  ## Especie parasitaria ## Tipo de tratamiento
  ## Cambio de lesion
  ## Infeccion fin tto

base_regresion_final <- data.frame(
  codigo_paciente = Big_hemo$codigo_paciente,
  sexo = gran_base_resumen$sexo,
  edad = gran_base_resumen$edad,
  edad_cat = gran_base_resumen$edad_categorica_corta,
  tiempo_evolucion = gran_base_resumen$tiempo_evolucion_dicotomica,
  tipo_lesion = gran_base_resumen$tipo_lesion_eval_base_corta,
  adenopatia_pre = gran_base_resumen$adenopatia_evalbas,
  infeccion_pre = gran_base_resumen$infeccion_concom_evalbas,
  especie = gran_base_resumen$especie_corta,
  tratamiento = gran_base_resumen$tratamiento,
  variacion_lesion = gran_base_resumen$variacion_lesion_post2,
  infeccion_post = gran_base_resumen$infeccion_concom_fintto,
  granulocitos_pre_cen = base_regre_centrada$Granulocitos_pre,
  plaquetas_pre_cen = base_regre_centrada$Plaquetas_pre,
  eosinofilos_post_cen = base_regre_centrada$Eosinofilos_post,
  porc_eos_post_cen = base_regre_centrada$porcentaje_eosino_post,  
  i_eos_neu_post_cen = base_regre_centrada$i_eos_neu_post,
  i_eos_gran_exp_post_cen = base_regre_centrada$i_eos_granu_post,
  variacion_mono_cen = base_regre_centrada$variacion_monocitos, 
  Estado_final = Big_hemo$Estado_final
  )

base_regresion_final <- base_regresion_final %>% 
  mutate(granulocitos_pre_dicotomica = case_when(
    Big_hemo$Granulocitos_pre > 0.01 ~ "0",
    Big_hemo$Granulocitos_pre <= 0.01 ~ "1"))

base_regresion_final$granulocitos_pre_dicotomica <- factor(base_regresion_final$granulocitos_pre_dicotomica,
                                         levels = c("0", "1"),
                                         labels = c("Mayor a 0.01 x 10^3 cel/uL",
                                                    "Menor o igual a 0.01 x 10^3 cel/uL"))

base_regresion_final <- base_regresion_final %>% 
  mutate(plaquetas_pre_dicotomica = case_when(
    Big_hemo$Plaquetas_pre > 250 ~ "0",
    Big_hemo$Plaquetas_pre <= 250 ~ "1"))

base_regresion_final$plaquetas_pre_dicotomica <- factor(base_regresion_final$plaquetas_pre_dicotomica,
                                                           levels = c("0", "1"),
                                                           labels = c("Mayor a 250 x 10^3 cel/uL",
                                                                      "Menor o igual a 250 x 10^3 cel/uL"))

base_regresion_final <- base_regresion_final %>% 
  mutate(eosinofilos_post_dicotomica = case_when(
    Big_hemo$Eosinofilos_post < 0.92 ~ "0",
    Big_hemo$Eosinofilos_post >= 0.92 ~ "1"))

base_regresion_final$eosinofilos_post_dicotomica <- factor(base_regresion_final$eosinofilos_post_dicotomica,
                                                        levels = c("0", "1"),
                                                        labels = c("Menor a 0.92 x 10^3 cel/uL",
                                                                   "Mayor o igual a 0.92 x 10^3 cel/uL"))


base_regresion_final <- base_regresion_final %>% 
  mutate(porc_eosinofilos_post_dicotomica = case_when(
    Big_hemo$porcentaje_eosino_post < 14 ~ "0",
    Big_hemo$porcentaje_eosino_post >= 14 ~ "1"))

base_regresion_final$porc_eosinofilos_post_dicotomica <- factor(base_regresion_final$porc_eosinofilos_post_dicotomica,
                                                        levels = c("0", "1"),
                                                        labels = c("Menor a 14%",
                                                                   "Mayor o igual a 14%"))


base_regresion_final <- base_regresion_final %>% 
  mutate(i_eos_neu_post_dicotomica = case_when(
    Big_hemo$i_eos_neu_post < 0.3 ~ "0",
    Big_hemo$i_eos_neu_post >= 0.3 ~ "1"))

base_regresion_final$i_eos_neu_post_dicotomica <- factor(base_regresion_final$i_eos_neu_post_dicotomica,
                                                           levels = c("0", "1"),
                                                           labels = c("Menor a 0.3",
                                                                      "Mayor o igual a 0.3"))

base_regresion_final <- base_regresion_final %>% 
  mutate(i_eos_gran_exp_post_dicotomica = case_when(
    Big_hemo$i_eos_granu_post < 0.93 ~ "0",
    Big_hemo$i_eos_granu_post >= 0.93 ~ "1"))

base_regresion_final$i_eos_gran_exp_post_dicotomica <- factor(base_regresion_final$i_eos_gran_exp_post_dicotomica,
                                                         levels = c("0", "1"),
                                                         labels = c("Menor a 0.93",
                                                                    "Mayor o igual a 0.93"))


base_regresion_final <- base_regresion_final %>% 
  mutate(razon_cambio_monocitos_dicotomica = case_when(
    Big_hemo$variacion_monocitos < 1.07 ~ "0",
    Big_hemo$variacion_monocitos >= 1.07 ~ "1"))

base_regresion_final$razon_cambio_monocitos_dicotomica <- factor(base_regresion_final$razon_cambio_monocitos_dicotomica,
                                                              levels = c("0", "1"),
                                                              labels = c("Menor a 1.07",
                                                                         "Mayor o igual a 1.07"))

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

base_regresion_final$edad_centrada = center_scale(base_regresion_final$edad)

base_regresion_final = subset(base_regresion_final, select = -c(Estado_final) )

base_regresion_final$Estado_final = Big_hemo$Estado_final

save(base_regresion_final, file ="data/Clean_data_RData/base_regresion_final.RData")
write.dta(base_regresion_final, file ="data/Clean_data_STATA_dta/base_regresion_final.dta")


## Construccion de las regresiones univariadas - Calculo de RR univariados hemograma,
## Se abrio la base de regresion_final en STATA y se calcularon los RR univariados

##########################################################################################
## CONSTRUCCION DE MODELOS UNIVARIADOS PRE-TRATAMIENTO
##########################################################################################

base_regresion_final <- base_regresion_final %>% 
  mutate(
    sexo = ifelse(sexo=="Femenino", 0,1),
    tiempo_evolucion = ifelse(tiempo_evolucion=="Mas de 4 semanas", 0, 1),
    tipo_lesion = ifelse(tipo_lesion=="Ulcera", 0,1),
    adenopatia_pre = ifelse(adenopatia_pre=="No",0,1),
    infeccion_pre = ifelse(infeccion_pre=="No", 0,1),
    especie = ifelse(especie=="L. panamensis y otras L. viannia spp",0,1),
    granulocitos_pre_dicotomica = ifelse(granulocitos_pre_dicotomica == "Mayor a 0.01 x 10^3 cel/uL",0,1),
    plaquetas_pre_dicotomica = ifelse(plaquetas_pre_dicotomica == "Mayor a 250 x 10^3 cel/uL",0,1),
    Estado_final = ifelse(Estado_final=="Cura definitiva", 0,1))


base_regresion_final <- base_regresion_final %>% 
  mutate(edad_cat2 = case_when(
    edad_cat == "Entre 17 y 44 años" ~ 0,
    edad_cat == "Entre 1 y 16 años" ~ 1,
    edad_cat == "Mayor o igual a 45 años" ~ 0
  ))

library(foreign)
write.dta(base_regresion_final, file = "data/Clean_data_STATA_dta/base_regresion_final_nolabel.dta")

## RR por Log binomial
table9 <- (tbl_uvregression(base_regresion_final[c("Estado_final", "sexo", "edad_centrada","edad_cat" , "tiempo_evolucion" , "tipo_lesion" , "adenopatia_pre",
"infeccion_pre" , "especie" , "tratamiento" , "granulocitos_pre_dicotomica" , "plaquetas_pre_dicotomica")],
                               method = glm,
                               y = Estado_final,
                               method.args = list(family = binomial(link="log")),
                               label = list(sexo = "Sexo",
                               edad_cat = "Edad categórica",
                               tiempo_evolucion = "Tiempo de evolución",
                               tipo_lesion = "Tipo de lesión pre-tratamiento",
                               adenopatia_pre = "Presencia de adenopatia pre-tratamiento",
                               infeccion_pre = "Infección concomitante pre-tratamiento",
                               especie = "Especie infectante",
                               tratamiento = "Tratamiento recibido",
                               granulocitos_pre_dicotomica = "Granulocitos pre-tratamiento",
                               plaquetas_pre_dicotomica = "Plaquetas pre-tratamiento"),
                               exponentiate = TRUE,
                               pvalue_fun = ~style_pvalue(.x, digits = 2)))

table9


## RR por Poisson
table10 <- (tbl_uvregression(base_regresion_final[c("Estado_final", "sexo", "edad_cat" , "tiempo_evolucion" , "tipo_lesion" , "adenopatia_pre",
                                                   "infeccion_pre" , "especie" , "tratamiento" , "granulocitos_pre_dicotomica" , "plaquetas_pre_dicotomica")],
                            method = glm,
                            y = Estado_final,
                            method.args = list(family = poisson(link="log")),
                            label = list(sexo = "Sexo",
                                         edad_cat = "Edad categórica",
                                         tiempo_evolucion = "Tiempo de evolución",
                                         tipo_lesion = "Tipo de lesión pre-tratamiento",
                                         adenopatia_pre = "Presencia de adenopatia pre-tratamiento",
                                         infeccion_pre = "Infección concomitante pre-tratamiento",
                                         especie = "Especie infectante",
                                         tratamiento = "Tratamiento recibido",
                                         granulocitos_pre_dicotomica = "Granulocitos pre-tratamiento",
                                         plaquetas_pre_dicotomica = "Plaquetas pre-tratamiento"),
                            exponentiate = TRUE,
                            pvalue_fun = ~style_pvalue(.x, digits = 2)))

table10


##########################################################################################
## CONSTRUCCION DE MODELOS MULTIVARIADOS PRE-TRATAMIENTO
##########################################################################################

#### PRIMER MODELO - FULL
## Modelo para hemogramas pre-tratamiento incluyendo todas las variables

model1 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + 
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
              family = binomial(link = "log"),
              data = base_regresion_final)
### Este modelo log binomial no corre por problemas de convergencia, si retiro la edad, ahi si corre. 


## Modelo full base
model1 <- glm(Estado_final ~ sexo + edad_centrada + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + 
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
              family = poisson(link = "log"),
              data = base_regresion_final)
BIC(model1)
summary(model1)
tab_reg <- tbl_regression(model1, exponentiate = TRUE)
tab_reg


#plot(model1)
modeldata <- model.frame(model1) ## obtengo un df de los pacientes que aportaron al modelo 1 porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model1))

## Para obtener el AUC
set.seed(200)
data <- base_regresion_final
#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ] 
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model1_traintest <- glm(Estado_final ~ sexo + edad_centrada + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + 
                        granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
              family = poisson(link = "log"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model1_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)


#### SEGUNDO MODELO - RETIRANDO TIEMPO DE EVOLUCION MENOR A 4 SEMANAS
#################################################

## 
model2 <- glm(Estado_final ~ sexo + edad_centrada + tipo_lesion + adenopatia_pre + infeccion_pre + 
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
              family = poisson(link = "log"),
              data = base_regresion_final)
BIC(model2)
summary(model2)
tab_reg2 <- tbl_regression(model2, exponentiate = TRUE)
tab_reg2
#plot(model2)
modeldata <- model.frame(model2) ## obtengo un df de los pacientes que aportaron al modelo 1 porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model2))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model2_traintest <- glm(Estado_final ~ sexo + edad_centrada + tipo_lesion + adenopatia_pre + infeccion_pre + 
                          granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
              family = poisson(link = "log"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model2_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model1, model2)

## Transformo los NA pa poder comparar los modelos
data.no.na <- na.omit(base_regresion_final)
model1.no.na <- glm(Estado_final ~ sexo + edad_centrada + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + 
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
                    family = poisson(link = "log"),
                    data = data.no.na)
summary(model1.no.na)

model2.no.na <- glm(Estado_final ~ sexo + edad_centrada + tipo_lesion + adenopatia_pre + infeccion_pre + 
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
                    family = poisson(link = "log"),
                    data = data.no.na)

lrtest(model1.no.na, model2.no.na) 
## LR test con p > a 0.26, lo que indica que los dos modelos explican igualmente los datos, por parsimonia me quedo con el modelo 2

?add_q

#### TERCER MODELO - SIN TIPO DE LESION
###############################################################################

## Para ver que los parametros excluidos no fueran significativos o no alteraran de manera importante las estimaciones contruyo nuevos
## modelos incluyendo cada parametro eliminado

### ## Se excluye la infección concomitante
model3 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + granulocitos_pre_dicotomica, plaquetas_pre_dicotomica,
              family = poisson,
              data = base_regresion_final)
BIC(model3)
summary(model3)
round(exp(model3$coefficients),3)
confint(model3)
tab_reg3 <- tbl_regression(model3, exponentiate = TRUE)
tab_reg3
#plot(model3)
modeldata <- model.frame(model3) ## obtengo un df de los pacientes que aportaron al modelo 1 porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model3))

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model3_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento + especie +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model3_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted) ## la primera vez que lo corri me dio 0.65

# Transformo los NA pa poder comparar los modelos
model3.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento + especie +
                      granulocitos_pre_dicotomica,  
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model2.no.na, model3.no.na) 



#### CUARTO MODELO - INCLUYENDO INFECCION CONCOMITANTE
#################################################

## Se excluyeron todas las que tuvieran p > 0.2
model4 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + tratamiento +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model4)
summary(model4)
tab_reg4 <- tbl_regression(model4, exponentiate = TRUE)
tab_reg4
#plot(model4)
modeldata <- model.frame(model4) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model4))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model4_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + tratamiento +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model4_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model4, model2)


### MODELO 5 --- INCLUYENDO PLAQUETAS
#####################################

model5 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model5)
summary(model5)
tab_reg5 <- tbl_regression(model5, exponentiate = TRUE)
tab_reg5
#plot(model5)
modeldata <- model.frame(model5) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model5))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model5_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model5_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
#lrtest(model5, model2)

## Transformo los NA pa poder comparar los modelos
model5.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model5.no.na, model2.no.na)


## Modelo 6  ---  Quitando tipo de lesion al modelo 2
##########################################################

model6 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + 
      + granulocitos_pre_dicotomica,
    family = binomial(link = "logit"),
    data = base_regresion_final)
BIC(model6)
summary(model6)
tab_reg6 <- tbl_regression(model6, exponentiate = TRUE)
tab_reg6
#plot(model5)
modeldata <- model.frame(model6) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model6))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model6_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + 
                          + granulocitos_pre_dicotomica,
              family = binomial(link = "logit"),
              data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model6_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model6, model2)


## Modelo 7  ---  Añadiendo especie al modelo 6
##########################################################

model7 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + especie 
                + granulocitos_pre_dicotomica,
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model7)
summary(model7)
tab_reg7 <- tbl_regression(model7, exponentiate = TRUE)
tab_reg7
#plot(model5)
modeldata <- model.frame(model7) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model7))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model7_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + especie
                          + granulocitos_pre_dicotomica,
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model7_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model7, model6)

## Transformo los NA pa poder comparar los modelos
model7.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model7.no.na, model2.no.na)



#### INTERACCIONES DEL MODELO 6 --- DEFINIDO COMO EL MEJOR

## Interaccion edad medicamento

model6_inter_tto_edad <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + granulocitos_pre_dicotomica +
                             edad_cat:tratamiento,
    family = binomial(link = "logit"),
    data = base_regresion_final)

tbl_interaccion_edad_droga <- tbl_regression(model6_inter_tto_edad, exponentiate = T)
tbl_interaccion_edad_droga

base_regresion_final %>% 
  tabyl (edad_cat, tratamiento, Estado_final)

## Interaccion edad granulocitos

model6_inter_tto_edad <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + granulocitos_pre_dicotomica +
                               edad_cat:tratamiento,
                             family = binomial(link = "logit"),
                             data = base_regresion_final)

tbl_interaccion_edad_droga <- tbl_regression(model6_inter_tto_edad, exponentiate = T)
tbl_interaccion_edad_droga

base_regresion_final %>% 
  tabyl (edad_cat, tratamiento, Estado_final)







### Regresiones logisticas

#### SEGUNDO MODELO - RETIRANDO LAS NO ASOCIADAS
#################################################

## Se excluyeron todas las que tuvieran p > 0.2
model2 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model2)
summary(model2)
tab_reg2 <- tbl_regression(model2, exponentiate = TRUE)
tab_reg2
#plot(model2)
modeldata <- model.frame(model2) ## obtengo un df de los pacientes que aportaron al modelo 1 porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model2))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model2_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                          granulocitos_pre_dicotomica, 
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model2_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model1, model2)

## Transformo los NA pa poder comparar los modelos
data.no.na <- na.omit(base_regresion_final)
model1.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + especie + tratamiento +
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica,
                    family = binomial(link = "logit"),
                    data = data.no.na)
summary(model1.no.na)

model2.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                      granulocitos_pre_dicotomica, 
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model1.no.na, model2.no.na) 
## LR test con p > a 0.26, lo que indica que los dos modelos explican igualmente los datos, por parsimonia me quedo con el modelo 2



#### TERCER MODELO - INCLUYENDO LA ESPECIE QUE FUE RETIRADA EN EL PASO ANTERIOR
###############################################################################

## Para ver que los parametros excluidos no fueran significativos o no alteraran de manera importante las estimaciones contruyo nuevos
## modelos incluyendo cada parametro eliminado

### ## Se incluye la especie
model3 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento + especie +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model3)
summary(model3)
tab_reg3 <- tbl_regression(model3, exponentiate = TRUE)
tab_reg3
#plot(model3)
modeldata <- model.frame(model3) ## obtengo un df de los pacientes que aportaron al modelo 1 porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model3))

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model3_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento + especie +
                          granulocitos_pre_dicotomica, 
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model3_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted) ## la primera vez que lo corri me dio 0.65

# Transformo los NA pa poder comparar los modelos
model3.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento + especie +
                      granulocitos_pre_dicotomica,  
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model2.no.na, model3.no.na) 



#### CUARTO MODELO - INCLUYENDO INFECCION CONCOMITANTE
#################################################

## Se excluyeron todas las que tuvieran p > 0.2
model4 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + tratamiento +
                granulocitos_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model4)
summary(model4)
tab_reg4 <- tbl_regression(model4, exponentiate = TRUE)
tab_reg4
#plot(model4)
modeldata <- model.frame(model4) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model4))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model4_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + infeccion_pre + tratamiento +
                          granulocitos_pre_dicotomica, 
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model4_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model4, model2)


### MODELO 5 --- INCLUYENDO PLAQUETAS
#####################################

model5 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model5)
summary(model5)
tab_reg5 <- tbl_regression(model5, exponentiate = TRUE)
tab_reg5
#plot(model5)
modeldata <- model.frame(model5) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model5))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------

# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model5_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                          granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model5_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
#lrtest(model5, model2)

## Transformo los NA pa poder comparar los modelos
model5.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model5.no.na, model2.no.na)


## Modelo 6  ---  Quitando tipo de lesion al modelo 2
##########################################################

model6 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + 
                + granulocitos_pre_dicotomica,
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model6)
summary(model6)
tab_reg6 <- tbl_regression(model6, exponentiate = TRUE)
tab_reg6
#plot(model5)
modeldata <- model.frame(model6) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model6))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model6_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + 
                          + granulocitos_pre_dicotomica,
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model6_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model6, model2)


## Modelo 7  ---  Añadiendo especie al modelo 6
##########################################################

model7 <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + especie 
              + granulocitos_pre_dicotomica,
              family = binomial(link = "logit"),
              data = base_regresion_final)
BIC(model7)
summary(model7)
tab_reg7 <- tbl_regression(model7, exponentiate = TRUE)
tab_reg7
#plot(model5)
modeldata <- model.frame(model7) ## obtengo un df de los pacientes que aportaron al modelo porque no me corria el hosmer
logitgof(modeldata$Estado_final, fitted(model7))

## Para obtener el AUC se corre lo que esta entre lineas 
## -------------------------------------------------------------------------------------------------------------------------------
# Analisis multivariado pre-tratamiento full - Entreno el modelo con el subset de entrenamiento
model7_traintest <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + especie
                        + granulocitos_pre_dicotomica,
                        family = binomial(link = "logit"),
                        data = train)
#calculate probability of default for each individual in test dataset. Con el modelo uno, asigno una probabilidad predicha a mi set de testeo
predicted <- predict(model7_traintest, test, type="response")
# calculo el AUC con una funcion de la libreria pROC
auc(test$Estado_final, predicted)
## -------------------------------------------------------------------------------------------------------------------------------

## Intento comparar los modelos con LR-test pero no son comparables por los NA
lrtest(model7, model6)

## Transformo los NA pa poder comparar los modelos
model7.no.na <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + tipo_lesion + adenopatia_pre + tratamiento +
                      granulocitos_pre_dicotomica + plaquetas_pre_dicotomica, 
                    family = binomial(link = "logit"),
                    data = data.no.na)

lrtest(model7.no.na, model2.no.na)



#### INTERACCIONES DEL MODELO 6 --- DEFINIDO COMO EL MEJOR

## Interaccion edad medicamento

model6_inter_tto_edad <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + granulocitos_pre_dicotomica +
                               edad_cat:tratamiento,
                             family = binomial(link = "logit"),
                             data = base_regresion_final)

tbl_interaccion_edad_droga <- tbl_regression(model6_inter_tto_edad, exponentiate = T)
tbl_interaccion_edad_droga

base_regresion_final %>% 
  tabyl (edad_cat, tratamiento, Estado_final)

## Interaccion edad granulocitos

model6_inter_tto_edad <- glm(Estado_final ~ sexo + edad_cat + tiempo_evolucion + adenopatia_pre + tratamiento + granulocitos_pre_dicotomica +
                               edad_cat:tratamiento,
                             family = binomial(link = "logit"),
                             data = base_regresion_final)

tbl_interaccion_edad_droga <- tbl_regression(model6_inter_tto_edad, exponentiate = T)
tbl_interaccion_edad_droga

base_regresion_final %>% 
  tabyl (edad_cat, tratamiento, Estado_final)



### Gráficas WorldLeish granulocitos
granulo_graph <- Big_hemo %>% 
  ggplot(aes(x=Estado_final, y=Granulocitos_pre, fill=Estado_final)) +
  geom_violin(show.legend = F, adjust = 0.85, alpha = 0.5) +
  labs(title="Pre-treatment immature granulocytes") +
  stat_summary(fun = median, fun.args = 0.50, show.legend = F,
               geom = "crossbar", alpha = 0.25, width = 0.60) +
  xlab("Final outcome") + ylab("Number of cells x 10^3/μL") +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2"))

granulo_graph


## Eosinofilos post

eosino_graph <- Big_hemo %>% 
  ggplot(aes(x=Estado_final, y=porcentaje_eosino_post, fill=Estado_final)) +
  geom_violin(show.legend = F, adjust = 0.6, alpha = 0.9) +
  labs(title="Post-treatment eosinophils") +
  stat_summary(fun = median, fun.args = 0.50, show.legend = F,
               geom = "crossbar", alpha = 0.25, width = 0.60) +
  xlab("Final outcome") + ylab("Percentage (%)") +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2"))

eosino_graph

eosino2_graph <- Big_hemo %>% 
  ggplot(aes(x=Estado_final, y=Eosinofilos_post, fill=Estado_final)) +
  geom_violin(show.legend = F, adjust = 0.6, alpha = 0.9) +
  labs(title="Post-treatment eosinophils") +
  stat_summary(fun = median, fun.args = 0.50, show.legend = F,
               geom = "crossbar", alpha = 0.25, width = 0.80) +
  xlab("Final outcome") + ylab("Number of cells x 10^3/μL") +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2"))

eosino2_graph



## Ratio monocytes

mono_graph <- Big_hemo %>% 
  ggplot(aes(x=Estado_final, y=variacion_monocitos, fill=Estado_final)) +
  geom_violin(show.legend = F, adjust = 0.7, alpha = 0.9) +
  labs(title="Variation in monocytes") +
  stat_summary(fun = median, fun.args = 0.50, show.legend = F,
               geom = "crossbar", alpha = 0.25, width = 0.70) +
  xlab("Final outcome") + ylab("Ratio post / pre-treatment monocytes") +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2")) + theme(axis.title = element_text(size = 16))   

mono_graph


## Neutro

neu_graph <- Big_hemo %>% 
  ggplot(aes(x=Estado_final, y=Neutrofilos_post, fill=Estado_final)) +
  geom_violin(show.legend = F, adjust = 0.25, alpha = 0.5) +
  labs(title="Post-treatmen neutrophils") +
  stat_summary(fun = median, fun.args = 0.50, show.legend = F,
               geom = "crossbar", alpha = 0.25, width = 0.60) +
  xlab("Final outcome") + ylab("Number of cells x 10^3/μL") +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5B2", "#E64B35B2"))

neu_graph



levels(Big_hemo$Estado_final) <- c("Cure", "Failure")

## Pre- Platelets
platebox <- boxplot(Plaquetas_pre ~ Estado_final, data = Big_hemo, col = "white",
                      xlab = "Final outcome",
                      ylab = "Number of cells x 10^3/μL")

# Points
platebox <- stripchart(Plaquetas_pre ~ Estado_final,
                         data =  Big_hemo,
                         method = "jitter",
                         pch = 19,
                         col = 2:4,
                         vertical = TRUE,
                         title ("Pre-treatment platelets"),
                         add = T) 


### Pre-Granulo

granulobox <- boxplot(Granulocitos_pre ~ Estado_final, data = Big_hemo, col = "white",
             xlab = "Final outcome",
             ylab = "Number of cells x 10^3/μL")

# Points
granulobox <- stripchart(Granulocitos_pre ~ Estado_final,
           data =  Big_hemo,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           title ("Pre-treatment immature granulocytes"),
           add = T) 



# Monocitos

monobox <- boxplot(variacion_monocitos ~ Estado_final, data = Big_hemo, col = "white",
             xlab = "Final outcome",
             ylab = "Ratio post / pre-treatment monocytes")

# Points
monobox <- stripchart(variacion_monocitos ~ Estado_final,
           data = Big_hemo,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           title ("Variation in monocytes"),
           add = T) 

#Eosinophils

eosinobox <- boxplot(porcentaje_eosino_post ~ Estado_final, data = Big_hemo, col = "white",
                   xlab = "Final outcome",
                   ylab = "Percentage (%)")

# Points
eosinobox <- stripchart(porcentaje_eosino_post ~ Estado_final,
                      data = Big_hemo,
                      method = "jitter",
                      pch = 19,
                      col = 2:4,
                      vertical = TRUE,
                      title ("Post-treatment eosinophils"),
                      add = T)



figure_box <- gridExtra::grid.arrange(granulobox, monobox, eosinobox, top="Main Title")



#Big_hemo$Estado_final

####################################
######### graficas paper

# Loading necessary libraries
library(ggplot2)
library(pROC)
library(gridExtra)
library(dplyr)

create_and_combine_plots <- function(variable_name, cutoff, data, y_label) {
  # Make sure there are no missing values in the variable of interest
  data <- na.omit(data)
  
  # Translate 'Estado_final' levels into English
  data$Estado_final <- factor(data$Estado_final, levels = c("Cura definitiva", "Falla terapeutica"),
                              labels = c("Cure", "Failure"))
  
  # Define colors for the English levels
  colors <- c("Cure" = "#4DBBD5B2", "Failure" = "#E64B35B2")
  
  # 1. Boxplot with jittered points
  boxplot <- ggplot(data, aes(x = Estado_final, y = !!sym(variable_name), fill = Estado_final)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, color = "black") +
    scale_fill_manual(values = colors) +
    labs(title = paste("Boxplot of", y_label), x = "Final Status", y = y_label) +
    theme_minimal(base_size = 18) +
    guides(fill=guide_legend(title="Final Status")) # Set legend title
  
  # 2. Density plot with cutoff
  density_plot <- ggplot(data, aes(x = !!sym(variable_name), fill = Estado_final)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = cutoff, linetype = "dashed", color = "black") +
    scale_fill_manual(values = colors) +
    labs(title = paste("Density and Cutoff for", y_label), x = y_label, y = "Density") +
    theme_minimal(base_size = 18) +
    guides(fill=guide_legend(title="Final Status")) # Set legend title
  
  # 3. ROC Curve with AUC annotation, corrected
  roc_data <- roc(response = data$Estado_final, predictor = data[[variable_name]], levels = rev(levels(data$Estado_final)))
  auc_value <- auc(roc_data)
  roc_plot <- ggroc(roc_data) +
    labs(title = paste("ROC Curve for", y_label), x = "1 - Specificity", y = "Sensitivity") +
    annotate("text", x = 0.75, y = 0.25, label = paste("AUC =", round(auc_value, 2)), size = 5) +
    theme_minimal(base_size = 18)
  
  # Combine the plots side by side
  combined_plot <- grid.arrange(boxplot, density_plot, roc_plot, ncol = 3)
  
  return(combined_plot)
}



# Calls for each hemogram parameter with their specific cutoffs and labels
create_and_combine_plots("Plaquetas_pre", 250, Big_hemo, "Platelets - PreTx")
create_and_combine_plots("Granulocitos_pre", 0.01, Big_hemo, "Immature granulocyte count - PreTx")
create_and_combine_plots("porcentaje_eosino_post", 14, Big_hemo, "Percentage of Eosinophils - EoTx")
create_and_combine_plots("variacion_monocitos", 1.07, Big_hemo, "Variation in Monocytes - EoTx")
