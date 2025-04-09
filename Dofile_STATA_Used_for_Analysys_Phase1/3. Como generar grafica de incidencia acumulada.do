use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Base_grande_limpia.dta" 
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
gen dias_falla = dias_fin_tto if estado_fin_tto==1
replace dias_falla = dias_sem8 if dias_falla==. & estado_sem8==1
replace dias_falla = dias_sem13 if dias_falla==. & estado_sem13==2
replace dias_falla = dias_sem26 if dias_falla==. & estado_sem26==2
replace dias_falla = dias_va if estado_va==2
replace dias_falla = dias_va if dias_falla==. & dias_sem13 ==. & dias_sem26 ==.
replace dias_falla = dias_sem13 if dias_falla==. & dias_sem26 ==.
replace dias_falla = dias_sem26 if dias_falla==.
sum dias_falla, d
rename dias_falla dias_observacion
gen ef = 1 if estado_fin_tto ==1 | estado_sem8 ==1 | estado_sem13 ==2 | estado_sem26 ==2 | estado_va ==2
replace ef = 0 if ef ==.
stset dias_observacion ef
ltable dias_observacion ef
ltable dias_observacion ef, graph failure notable ci xlab(0(2)10)