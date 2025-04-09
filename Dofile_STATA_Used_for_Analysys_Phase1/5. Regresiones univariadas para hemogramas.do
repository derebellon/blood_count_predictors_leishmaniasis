use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Base_grande_limpia.dta" 
glm ef Granulocitos_pre, family(binomial 1) link(log) eform
glm ef Granulocitos_pre, family(binomial 1) link(log) eform
glm ef Granulocitos_pre, family(poisson) link(log) vce(robust) eform
glm ef Granulocitos_pre_exp, family(poisson) link(log) vce(robust) eform
gen Granulocitos_pre100 = Granulocitos_pre*10
glm ef Granulocitos_pre100, family(binomial 1) link(log) eform
glm ef Plaquetas_pre, family(binomial 1) link(log) eform
glm ef Eosinofilos_pre , family(binomial 1) link(log) eform
glm ef i_eos_granu_pre , family(binomial 1) link(log) eform
glm ef Eosinofilos_post , family(binomial 1) link(log) eform
glm ef porcentaje_eosino_post , family(binomial 1) link(log) eform
glm ef i_eos_neu_post, family(binomial 1) link(log) eform
glm ef i_eos_granu_post , family(binomial 1) link(log) eform
glm ef i_eos_granu_post , family(binomial 1) link(log) eform
glm ef variacion_leucocitos , family(binomial 1) link(log) eform
glm ef variacion_monocitos , family(binomial 1) link(log) eform
glm ef variacion_granulocitos , family(binomial 1) link(log) eform
gen granulocitos_post100 = Granulocitos_post*10
gen variacion_granulocitos100 = granulocitos_post100/ Granulocitos_pre100
glm ef variacion_granulocitos100 , family(binomial 1) link(log) eform
glm ef variacion_i_granu_linfo , family(binomial 1) link(log) eform
glm ef variacion_linfocitos , family(binomial 1) link(log) eform
glm ef variacion_porcentaje_granulo , family(binomial 1) link(log) eform