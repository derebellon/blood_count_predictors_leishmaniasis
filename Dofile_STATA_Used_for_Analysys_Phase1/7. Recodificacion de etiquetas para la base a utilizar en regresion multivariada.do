** ASIGNACION ETIQUETAS

use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\base_regresion_final_nolabel.dta" 
tab sexo
label define sex 0"Femenino"1"Masculino"
label values sexo sex
replace edad_cat = 0 if edad_cat==1
replace edad_cat = 1 if edad_cat==2
replace edad_cat = 2 if edad_cat==3
label define edad 0"Entre 17 y 44 años"1"Entre 1 y 16 años"2"Mayor o igual a 45 años"
label values edad_cat edad
tab edad_cat
tab tiempo_evolucion
label define tiempo 0"Mas de 4 semanas con sintomas"1"Menor o igual a 4 semanas con sintomas"
label values tiempo_evolucion tiempo
tab tipo_lesion
label define tipo_lesion 0"Ulcera"1"No ulcera"
label values tipo_lesion tipo_lesion
tab tipo_lesion
tab adenopatia_pre
tab infeccion_pre
label define sino 0"No"1"Si"
label values adenopatia_pre infeccion_pre sino
tab especie
label define especie 0"Otras especies de Leishmania Viannia spp" 1"Leishmania Viannia braziliensis"
label values especie especie
tab tratamiento
tab tratamiento, nolabel
replace tratamiento = 0 if tratamiento ==2
replace tratamiento = 2 if tratamiento ==3
label define tto 0"Miltefosine"1"Glucantime"2"Glucantime + Pentoxifilina"
label values tratamiento tto
tab tratamiento
tab variacion_lesion
tab variacion_lesion, nolabel
replace variacion_lesion = 0 if variacion_lesion==1
replace variacion_lesion = 1 if variacion_lesion==2
replace variacion_lesion = 2 if variacion_lesion==3
label define lesioncambio 0"Ulcera - Placa"1"Ulcera - Ulcera"2"Otros cambios"
label values variacion_lesion lesioncambio
tab variacion_lesion
tab variacion_lesion, nolabel
tab infeccion_post
tab infeccion_post, nolabel
replace infeccion_post = 0 if infeccion_post==1
replace infeccion_post = 1 if infeccion_post==2
label values infeccion_post sino
tab infeccion_post
tab infeccion_post, nolabel
tab granulocitos_pre_dicotomica
label define granu 0"> 0,01 x 10^3 cél/uL "1"≤ 0,01 x 10^3 cél/uL"
label values granulocitos_pre_dicotomica granu
label define pla 0"> 250 x 10^3 cél/uL"1"≤ 250 x 10^3 cél/uL"
label values plaquetas_pre_dicotomica pla
tab eosinofilos_post_dicotomica
tab eosinofilos_post_dicotomica, nolabel
replace eosinofilos_post_dicotomica = 0 if eosinofilos_post_dicotomica==1
replace eosinofilos_post_dicotomica = 1 if eosinofilos_post_dicotomica==2
label define eosdic 0"< 0,92 x 10^3 cél/uL "1"≥ 0,92 x 10^3 cél/uL "
label values eosinofilos_post_dicotomica eosdic
tab eosinofilos_post_dicotomica
tab porc_eosinofilos_post_dicotomic
tab porc_eosinofilos_post_dicotomic, nolabel
replace porc_eosinofilos_post_dicotomic=0 if porc_eosinofilos_post_dicotomic==1
replace porc_eosinofilos_post_dicotomic=1 if porc_eosinofilos_post_dicotomic==2
label define porceos 0"Menor a 14%"1"Mayor o igual a 14%"
label values porc_eosinofilos_post_dicotomic porceos
tab porc_eosinofilos_post_dicotomic
tab i_eos_neu_post_dicotomica
tab i_eos_neu_post_dicotomica, nolabel
replace i_eos_neu_post_dicotomica = 0 if i_eos_neu_post_dicotomica==1
replace i_eos_neu_post_dicotomica = 1 if i_eos_neu_post_dicotomica==2
label define ieosneu 0"Menor a 0.3"1"Mayor o igual a 0.3"
label values i_eos_neu_post_dicotomica ieosneu
tab i_eos_neu_post_dicotomica
tab i_eos_neu_post_dicotomica,nolabel
tab i_eos_gran_exp_post_dicotomica
tab i_eos_gran_exp_post_dicotomica, nolabel
replace i_eos_gran_exp_post_dicotomica = 0 if i_eos_gran_exp_post_dicotomica==1
replace i_eos_gran_exp_post_dicotomica = 1 if i_eos_gran_exp_post_dicotomica==2
label define eosgran 0"Menor a 0.93"1"Mayor o igual a 0.93"
label values i_eos_gran_exp_post_dicotomica eosgran
tab i_eos_gran_exp_post_dicotomica
tab i_eos_gran_exp_post_dicotomica,nolabel
tab razon_cambio_monocitos_dicotomc
tab razon_cambio_monocitos_dicotomc,nolabel
replace razon_cambio_monocitos_dicotomc=0 if razon_cambio_monocitos_dicotomc==1
replace razon_cambio_monocitos_dicotomc=1 if razon_cambio_monocitos_dicotomc==2
label define cambiomonos 0"Menor a 1.07"1"Mayor o igual a 1.07"
label values razon_cambio_monocitos_dicotomc cambiomonos
tab edad_cat2 edad_cat
label define edad2 0"De 1 a 16 años de edad"1"De 17 años o mas deedad"
label values edad_cat2 edad2
label define estadof 0"Cura"1"Falla"
label values Estado_final estadof
save "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Base de regresión final - Poisson robusta.dta", replace