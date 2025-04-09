use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\base_regresion_final.dta" 
gen ef = 0 if Estado_final==1
replace ef = 1 if Estado_final ==2
cs ef granulocitos_pre_dicotomica
tab granulocitos_pre_dicotomica, nolabel
gen granulo = 0 if granulocitos_pre_dicotomica==1
replace granulo = 1 if granulocitos_pre_dicotomica==2
cs ef granulo
help cs
tab plaquetas_pre_dicotomica
tab plaquetas_pre_dicotomica, nolabel
gen plaq = 0 if plaquetas_pre_dicotomica==1
replace plaq = 1 if plaquetas_pre_dicotomica==2
cs ef plaq
tab eosinofilos_post_dicotomica
tab eosinofilos_post_dicotomica, nolabel
gen eos_cont = 0 if eosinofilos_post_dicotomica ==1
replace eos_cont = 1 if eosinofilos_post_dicotomica ==2
cs ef eos_cont
tab porc_eosinofilos_post_dicotomic
tab porc_eosinofilos_post_dicotomic, nolabel
gen porc_eos = 0 if porc_eosinofilos_post_dicotomic==1
replace porc_eos = 1 if porc_eosinofilos_post_dicotomic==2
cs ef porc_eos
tab i_eos_neu_post_dicotomica
tab i_eos_neu_post_dicotomica, nolabel
gen ieosneu = 0 if i_eos_neu_post_dicotomica==1
replace ieosneu = 1 if i_eos_neu_post_dicotomica==2
cs ef ieosneu
tab i_eos_gran_exp_post_dicotomica
tab i_eos_gran_exp_post_dicotomica, nolabel
gen ieosgran = 0 if i_eos_gran_exp_post_dicotomica==1
replace ieosgran = 1 if i_eos_gran_exp_post_dicotomica==2
cs ef ieosgran
tab razon_cambio_monocitos_dicotomc
tab razon_cambio_monocitos_dicotomc, nolabel
gen razon_mono = 0 if razon_cambio_monocitos_dicotomc==1
replace razon_mono = 1 if razon_cambio_monocitos_dicotomc==2
cs ef razon_mono