** Proceso de ajuste en las unidades de medicion de basofilos y granulocitos para estimacion de los odds


use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\hemogramas_centrados.dta" 
gen ef = 1 if Estado_final ==2
replace ef = 0 if Estado_final ==1
gen basofilos_pre_100 = Basofilos_pre*10
logistic ef basofilos_pre_100
gen granulocitos_pre_100 = Granulocitos_pre * 10
logistic ef granulocitos_pre_100
gen i_baso_linfo_pre2 = basofilos_pre_100 / Linfocitos_pre
logistic ef i_baso_linfo_pre2
gen i_baso_mono_pre2 = basofilos_pre_100 / Monocitos_pre
logistic ef i_baso_mono_pre2
gen i_bas_gra_pre_100 = basofilos_pre_100 / granulocitos_pre_100
logistic ef i_bas_gra_pre_100
gen basofilos_post_100 = Basofilos_post * 10
logistic ef basofilos_post_100
gen granulocitos_post_100 = Granulocitos_post * 10
logistic ef granulocitos_post_100
gen i_baso_linfo_post2 = basofilos_post_100 / Linfocitos_post
logistic ef i_baso_linfo_post2
gen i_baso_mono_post2 = basofilos_post_100 / Monocitos_post
logistic ef i_baso_mono_post2
gen i_bas_gra_post_100 = basofilos_post_100 / granulocitos_post_100
logistic ef i_bas_gra_post_100
gen variacion_granulocitos2 = granulocitos_post_100 / granulocitos_pre_100
logistic ef variacion_granulocitos2