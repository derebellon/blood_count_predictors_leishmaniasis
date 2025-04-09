// RR UNIVARIADOS

csi 29 2 129 40
csi 3 22 4 96
csi 1 22 13 96
csi 4 22 42 96
csi 1 22 6 96
csi 22 7 100 56
csi 2 7 12 56
csi 4 18 22 74
csi 9 18 47 74
csi 9 15 40 85
csi 7 15 43 85
csi 2 25 1 146
csi 1 25 1 146
csi 2 25 19 146
csi 9 22 29 139
csi 3 28 4 165
csi 2 29 8 161
csi 6 20 12 110
csi 8 15 28 80
csi 8 15 61 80
csi 5 26 19 148
csi 12 11 26 91
csi 6 11 25 91
csi 2 29 3 165




use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\gran_base_resumen.dta" 
tab edad_categorica
tab edad_categorica, nolabel
gen edad_categorica2 = 0 if edad_categorica == 1
replace edad_categorica2 = 1 if edad_categorica == 2
replace edad_categorica2 = 1 if edad_categorica == 3
gen ef = 0 if estado_final_1 ==1
replace ef =1 if estado_final_1==2
cs ef edad_categorica2
cs ef edad_categorica2
gen edad_mayor_45 = 1 if edad_categorica==4
replace edad_mayor_45 = 1 if edad_categorica==5
replace edad_mayor_45 = 0 if edad_categorica == 1
tab ef edad_mayor_45, row
tab edad_mayor_45 ef, row
cs ef edad_mayor_45
tab imc_categorico
tab imc_categorico, nolabel
gen imc_mayor_25 = 1 if imc_categorico == 3
replace imc_mayor_25 = 1 if imc_categorico == 4
replace imc_mayor_25 = 1 if imc_categorico == 5
replace imc_mayor_25 = 0 if imc_categorico == 1
tab imc_mayor_25 ef, row
cs ef imc_mayor_25
tab tipo_lesion_evalbas
tab tipo_lesion_evalbas, nolabel
gen otra_lesion = 1 if tipo_lesion_evalbas==2
replace otra_lesion = 1 if tipo_lesion_evalbas==3
replace otra_lesion = 1 if tipo_lesion_evalbas==5
replace otra_lesion = 0 if tipo_lesion_evalbas==1
tab otra_lesion ef, row
cs ef otra_lesion
cs ef otra_lesion
cs ef sexo
tab sexo
tab sexo, nolabel
gen sex2=1 if sexo ==2
replace sex2=0 if sexo==1
cs ef sex2