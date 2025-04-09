use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Big_hemo.dta" 
sum Leucocitos_pre, d
sum Leucocitos_pre, d
sum Neutrofilos_pre, d
sum Eosinofilos_pre, d
list codigo_paciente if Eosinofilos_pre >=1.5
help operators
list codigo_paciente if Eosinofilos_pre >=1.5 & Eosinofilos_pre != .
list codigo_paciente if Eosinofilos_pre >0.5 & Eosinofilos_pre <=1.5
sum porcentaje_eosino_pre, d
tab codigo_paciente if porcentaje_eosino_pre >0.5 & porcentaje_eosino_pre != .
tab codigo_paciente if porcentaje_eosino_pre >4 & porcentaje_eosino_pre != .
tab codigo_paciente if porcentaje_eosino_pre >4 & porcentaje_eosino_pre != . & Eosinofilos_pre <= 0.5
sum Monocitos_pre, d
sum porcentaje_mono_pre, d
tab codigo_paciente if porcentaje_mono_pre > 8 & porcentaje_mono_pre != . & Monocitos_pre <= 0.7
sum porcentaje_neutro_pre, d
tab codigo_paciente if porcentaje_neutro_pre < 55 & porcentaje_neutro_pre != . & Neutrofilos_pre < 2.5
sum eos_neu2
sum i_eos_neu_post
sum Leucocitos_post, d
sum Neutrofilos_post, d
sum Neutrofilos_pre, d
sum Eosinofilos_pre, d
sum Eosinofilos_post, d
ttest Eosinofilos_pre == Eosinofilos_post
help mann withney
help wilcoxon
help ranksum
 signrank Eosinofilos_pre = Eosinofilos_post
ttest Leucocitos_pre == Leucocitos_post
ttest Neutrofilos_pre == Neutrofilos_post
tab codigo_paciente if Eosinofilos_post > 0.5 & Eosinofilos_post != .
tab codigo_paciente if Eosinofilos_post >=1.5 & Eosinofilos_post != .
tab codigo_paciente if Eosinofilos_post >0.5 & Eosinofilos_post < 1.5