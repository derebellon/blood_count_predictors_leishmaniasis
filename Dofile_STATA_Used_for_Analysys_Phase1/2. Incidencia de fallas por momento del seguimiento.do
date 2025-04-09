use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\gran_base_resumen.dta" 
tab estado_fin_tto
tab estado_fin_tto, nolabel
tab estado_sem8
tab estado_sem8, nolabel
tab estado_sem13
tab estado_sem13, nolabel
tab estado_sem26
tab estado_sem26, nolabel
tab estado_va
tab estado_va, nolabel

** 
gen estado_fin_tto_no_na = estado_fin_tto
replace estado_fin_tto_no_na= 0 if estado_fin_tto==2 | estado_fin_tto ==3
replace estado_fin_tto_no_na=0 if estado_fin_tto_no_na==. & acude_fin_tto==2
label define mejoriafalla 0"Mejoria" 1"Falla terapéutica"
label values estado_fin_tto_no_na mejoriafalla

gen estado_sem8_no_na = estado_sem8
replace estado_sem8_no_na =. if estado_fin_tto==1
replace estado_sem8_no_na =0 if estado_sem8_no_na==2 | estado_sem8_no_na ==3 | estado_sem8_no_na==4
label values estado_sem8_no_na mejoriafalla

gen estado_sem13_no_na = estado_sem13
replace estado_sem13_no_na =. if estado_fin_tto==1
replace estado_sem13_no_na =. if estado_sem8_no_na==1
replace estado_sem13_no_na =. if estado_va==2 & dias_va < 90
replace estado_sem13_no_na = 0 if estado_sem13_no_na==1 | estado_sem13_no_na==3 | estado_sem13_no_na==4 | estado_sem13_no_na==5
replace estado_sem13_no_na = 1 if estado_sem13_no_na==2
label define ef 0"Cura" 1"Falla"
label values estado_sem13_no_na ef

gen estado_sem26_no_na = estado_sem26
replace estado_sem26_no_na =. if estado_fin_tto==1
replace estado_sem26_no_na =. if estado_sem8_no_na==1
replace estado_sem26_no_na =. if estado_sem13_no_na==1
replace estado_sem26_no_na =. if estado_va==2
replace estado_sem26_no_na = 0 if estado_sem26_no_na==1 | estado_sem26_no_na==3 | estado_sem26_no_na==4
replace estado_sem26_no_na = 1 if estado_sem26_no_na==2
label values estado_sem26_no_na ef

gen estado_va_no_na = estado_va
replace estado_va_no_na =. if estado_fin_tto==1
replace estado_va_no_na =. if estado_sem8_no_na==1
replace estado_va_no_na = 0 if estado_va_no_na==1 | estado_va_no_na==3 | estado_va_no_na==4
replace estado_va_no_na = 1 if estado_va_no_na==2
label values estado_va_no_na ef

gen estado_va_ftt_s13_no_na = estado_va_no_na if dias_va >20 & dias_va <90
replace estado_va_ftt_s13_no_na=. if estado_fin_tto_no_na==1 | estado_sem8_no_na ==1
label values estado_va_ftt_s13_no_na mejoriafalla

gen estado_va_s13_s26_no_na = estado_va_no_na if dias_va >90 & dias_va <194
replace estado_va_s13_s26_no_na=. if estado_fin_tto_no_na==1 | estado_sem8_no_na ==1 | estado_sem13_no_na ==1 | estado_va_ftt_s13_no_na==1
label values estado_va_s13_s26_no_na ef

** INCIDENCIAS/PREVALENCIAS
tab estado_fin_tto_no_na
proportion estado_fin_tto_no_na

tab estado_sem8_no_na
proportion estado_sem8_no_na

tab estado_va_ftt_s13_no_na
proportion estado_va_ftt_s13_no_na

tab estado_sem13_no_na
proportion estado_sem13_no_na

tab estado_va_s13_s26_no_na
proportion estado_va_s13_s26_no_na

tab estado_sem26_no_na
proportion estado_sem26_no_na

tab estado_va_no_na
proportion estado_va_no_na


*** INCIDENCIAS ACUMULADAS

gen estado_sem8_2 = estado_sem8
replace estado_sem8_2 =0 if estado_sem8_2==2 | estado_sem8_2 ==3 | estado_sem8_2==4
label values estado_sem8_2 mejoriafalla

gen estado_va_2 = estado_va
replace estado_va_2 = 0 if estado_va_2==1 | estado_va_2==3 | estado_va_2==4
replace estado_va_2 = 1 if estado_va_2==2
label values estado_va_2 ef

gen estado_sem13_2 = estado_sem13
replace estado_sem13_2 = 0 if estado_sem13_2==1 | estado_sem13_2==3 | estado_sem13_2==4 | estado_sem13_2==5
replace estado_sem13_2 = 1 if estado_sem13_2==2 | estado_va_2==1
label values estado_sem13_2 ef

gen estado_sem26_2 = estado_sem26
replace estado_sem26_2 = 0 if estado_sem26_2==1 | estado_sem26_2==3 | estado_sem26_2==4
replace estado_sem26_2 = 1 if estado_sem26_2==2 | estado_va_2==1
label values estado_sem26_2 ef

tab estado_fin_tto_no_na
proportion estado_fin_tto_no_na
tab estado_sem8_2
proportion estado_sem8_2
tab estado_sem13_2
proportion estado_sem13_2
tab estado_sem26_2
proportion estado_sem26_2

