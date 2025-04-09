use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Base de regresión final - Poisson robusta.dta"

** Verificacion rapida 

tab sexo, m
tab edad_cat, m
tab edad, m
tab tiempo_evolucion, m
tab tipo_lesion
tab adenopatia_pre, m
tab infeccion_pre, m
tab especie
tab tratamiento
tab variacion_lesion
tab infeccion_post

** LOG BINOMIAL // Sin convergencia
**glm Estado_final sexo edad_centrada tipo_lesion adenopatia_pre infeccion_pre tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(binomial 1) link(log)


*** Modelo base - pre tratamiento 
glm Estado_final sexo edad_centrada tipo_lesion adenopatia_pre infeccion_pre i.tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust)
glm Estado_final sexo edad_centrada tipo_lesion adenopatia_pre infeccion_pre i.tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform
est store A
glm Estado_final sexo edad_centrada tipo_lesion adenopatia_pre infeccion_pre i.tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform

** Modelo 2
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust)
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform
est store B

lrtest A B

** Modelo 3
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre tipo_lesion granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform
est store C
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre tipo_lesion granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform
lrtest B C

** Modelo 4
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre i.tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform
est store D
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre i.tratamiento granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform
lrtest B D

** Modelo 5
glm Estado_final edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform
est store E
glm Estado_final edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform
lrtest B E

** Modelo 6
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica tiempo_evolucion , family(poisson) link(log) vce(robust) eform

** Modelo 7
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica especie, family(poisson) link(log) vce(robust) eform



*** Modelos post-tratamiento 

** Modelo 1
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log)
est store A
collin Estado_final sexo edad_centrada tratamiento infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre

** Modelo 2
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log)
est store B
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log) vce(robust) eform
lrtest A B

** Modelo 3
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre tipo_lesion, family(poisson) link(log)
est store C
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre tipo_lesion, family(poisson) link(log) vce(robust) eform
lrtest B C

** Modeleo 4
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre i.tratamiento, family(poisson) link(log)
est store D
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre i.tratamiento, family(poisson) link(log) vce(robust) eform
lrtest B D

** Modelo 5
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre i.variacion_lesion, family(poisson) link(log) vce(robust) eform

** Modelo 6
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre tiempo_evolucion , family(poisson) link(log) vce(robust) eform

** Modelo 7
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre especie, family(poisson) link(log) vce(robust) eform

** Modelo 8
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post eosinofilos_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log)
est store X
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post eosinofilos_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform
lrtest B X

* Modelo 9
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post i_eos_gran_exp_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform

** Modelo 10
glm Estado_final sexo edad_centrada i.tratamiento infeccion_post i_eos_neu_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform



*** MODELOS FINALES ELEGIDOS
* Pre tratamiento 
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform

** Post tratamiento 
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log) vce(robust) eform

glm Estado_final sexo edad_centrada i.tratamiento infeccion_post eosinofilos_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform

glm Estado_final sexo edad_centrada i.tratamiento infeccion_post i_eos_gran_exp_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform

glm Estado_final sexo edad_centrada i.tratamiento infeccion_post i_eos_neu_post_dicotomica razon_cambio_monocitos_dicotomc tipo_lesion adenopatia_pre, family(poisson) link(log) vce(robust) eform



*** ANALISIS POST REGRESION MODELO PRE-TRATAMIENTO
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) eform   // mirar deviance y pearson, ambos tienen que ser no significativos,  If the tests are significant, the Poisson regression model is inappropriate.  Then you could try a negative binomial model
predict pearson // aquie lo que hago es predecir los residuales de pearson para graficarlos mas adelante. Un residual es la distancia entre mi observación y el valor predicho por la regresion, en este caso grafico los residuales porque me interesa identificar esos valores atípicos (entendidos como los residuales muy grandes), es decir las observaciones muy alejadas del valor predicho
gen id1=_n // para generar una variable con un numero consecutivo de identificacion
scatter pearson id1, mlab(id) yline(0) //me muestra una nube, la cual yo espero que todos estén al rededor de cero, me sirve para ver valores extremos y al lado del punto veo el identificador
predict dv, dev // para estimar los residuales de devianza, ya que estos si o si los tengo que mostrar siempre en una regresion de poisson
scatter dv id1, mlab(id) yline(0) // aqui miro la desviacion de los residuales respecto al likelihood ratio, lo hago para lo siguiente: aquellos valores atipicos corresponden a pacientes que me pueden estar afectando mi modelo de regresion, el ideal sería que yo quite esas observaciones y volviera a correr mi modelo de regresion para ver que tanto me varían mis coeficientes beta (que son indicadores indirectos de mis odds ratio), si la variacion no es muy grande quiere decir que mi regresion estaba bien calculada con o sin la observacion anomala y los hallazgos son consistentes, si la variacion es muy grande quiere decir que mis hallazgos podrían estar sesgados por la observacion, asi que me quedaría la duda de si es mejor quitar ese dato del modelo o no (dificil decidirlo porque es una observacion que paso un control de calidad, asi que tendría que justificar muy bien por que la quité, o mejor no quitarla)  
predict hat, hat // esto es muy similar a lo anterior de mirar la desviación de los residuales, me sirve para ver apalancamiento. La variable hat lo que hace es calcularme la diferencia entre mi valor predicho y mi residual (a diferencia de lo anterior que comparaba mi residual contra la verosimilitud usando la siguiente formula -2*ln(versosimilutd) devianza=-2*ln(versosimilutd)). Este hat tambien me sirve para identificar observaciones raras que pueden estar apalancando mis resultados. Ya que Hat es la probabilidad predicha, me sirve para aplicar el estadistico linktest
predict p // para encontrar valores de probabilidad del desenlace (prediccion)
scatter hat p, mlab(id) //veo los valores atipicos graficamente
linktest // me muestra dos variables, la hat y la variable hat al cuadrado, en general la variable hat siempre dará significativa asi que esta no me interesa tanto mirarla, la que miro es la hat al cuadrado, si me da un hat al cuadrado con un valor de p significativo, entonces significa que tengo una mala especificacion del modelo, las causas de mala especificacion del modelo incluyen: omisión de variables, la relación de las covariables con el log(risk) no es lineal, hay omisión de un término de interacción

// si quiero entender mas del analisis post- regresion 
*help poisgof
*help predict
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform
drop if id1==48
glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log) vce(robust) eform


drop pearson id1 dv hat p



*** ANALISIS POST REGRESION MODELO POST-TRATAMIENTO
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log) eform  // mirar deviance y pearson, ambos tienen que ser no significativos,  If the tests are significant, the Poisson regression model is inappropriate.  Then you could try a negative binomial model
predict pearson // aquie lo que hago es predecir los residuales de pearson para graficarlos mas adelante. Un residual es la distancia entre mi observación y el valor predicho por la regresion, en este caso grafico los residuales porque me interesa identificar esos valores atípicos (entendidos como los residuales muy grandes), es decir las observaciones muy alejadas del valor predicho
gen id1=_n // para generar una variable con un numero consecutivo de identificacion
scatter pearson id1, mlab(id) yline(0) //me muestra una nube, la cual yo espero que todos estén al rededor de cero, me sirve para ver valores extremos y al lado del punto veo el identificador
predict dv, dev // para estimar los residuales de devianza, ya que estos si o si los tengo que mostrar siempre en una regresion de poisson
scatter dv id1, mlab(id) yline(0) // aqui miro la desviacion de los residuales respecto al likelihood ratio, lo hago para lo siguiente: aquellos valores atipicos corresponden a pacientes que me pueden estar afectando mi modelo de regresion, el ideal sería que yo quite esas observaciones y volviera a correr mi modelo de regresion para ver que tanto me varían mis coeficientes beta (que son indicadores indirectos de mis odds ratio), si la variacion no es muy grande quiere decir que mi regresion estaba bien calculada con o sin la observacion anomala y los hallazgos son consistentes, si la variacion es muy grande quiere decir que mis hallazgos podrían estar sesgados por la observacion, asi que me quedaría la duda de si es mejor quitar ese dato del modelo o no (dificil decidirlo porque es una observacion que paso un control de calidad, asi que tendría que justificar muy bien por que la quité, o mejor no quitarla)  
predict hat, hat // esto es muy similar a lo anterior de mirar la desviación de los residuales, me sirve para ver apalancamiento. La variable hat lo que hace es calcularme la diferencia entre mi valor predicho y mi residual (a diferencia de lo anterior que comparaba mi residual contra la verosimilitud usando la siguiente formula -2*ln(versosimilutd) devianza=-2*ln(versosimilutd)). Este hat tambien me sirve para identificar observaciones raras que pueden estar apalancando mis resultados. Ya que Hat es la probabilidad predicha, me servirá para aplicar el estadistico linktest
predict p // para encontrar valores de probabilidad del desenlace (prediccion)
scatter hat p, mlab(id) //veo los valores atipicos graficamente

list if id1== 43
list if id1== 57
list if id1== 132
list if id1== 152
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log) eform
drop if id1== 43 | id1== 57 | id1== 132 | id1== 152
glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dicotomic razon_cambio_monocitos_dicotomc adenopatia_pre, family(poisson) link(log) eform


linktest // me muestra dos variables, la hat y la variable hat al cuadrado, en general la variable hat siempre dará significativa asi que esta no me interesa tanto mirarla, la que miro es la hat al cuadrado, si me da un hat al cuadrado con un valor de p significativo, entonces significa que tengo una mala especificacion del modelo, las causas de mala especificacion del modelo incluyen: omisión de variables, la relación de las covariables con el log(risk) no es lineal, hay omisión de un término de interacción



*** Imputacion multiple
clear
use "D:\OneDrive - CIDEIM\DAVID\Proyecto de tesis - fellowship\3. Manejo de datos\3. Analisis tesis en R\Analisis tesis de maestria\data\Clean_data_STATA_dta\Base de regresión final - Poisson robusta.dta"
mi set mlong
rename porc_eosinofilos_post_dicotomic porc_eosinofilos_post_dic
rename razon_cambio_monocitos_dicotomc razon_cambio_monocitos_dic
mi register imputed sexo adenopatia_pre infeccion_pre infeccion_post granulocitos_pre_dicotomica plaquetas_pre_dicotomica porc_eosinofilos_post_dic razon_cambio_monocitos_dic edad_centrada
mi misstable summarize, all
mi impute mvn adenopatia_pre infeccion_pre infeccion_post granulocitos_pre_dicotomica plaquetas_pre_dicotomica porc_eosinofilos_post_dic razon_cambio_monocitos_dic edad_centrada = sexo  tratamiento  Estado_final, add(20) rseed(1234)
replace adenopatia_pre=0 if adenopatia_pre <=0.5
replace adenopatia_pre=1 if adenopatia_pre >0.5 & adenopatia_pre <1
replace granulocitos_pre_dicotomica=0 if granulocitos_pre_dicotomica <=0.5
replace granulocitos_pre_dicotomica=1 if granulocitos_pre_dicotomica >0.5 & granulocitos_pre_dicotomica <1
replace infeccion_post =0 if infeccion_post <=0.5
replace infeccion_post =1 if infeccion_post >0.5 & infeccion_post <1
replace infeccion_pre =0 if infeccion_pre <=0.5
replace infeccion_pre =1 if infeccion_pre >0.5 & infeccion_pre <1
replace plaquetas_pre_dicotomica =1 if plaquetas_pre_dicotomica >0.5 & plaquetas_pre_dicotomica <1
replace plaquetas_pre_dicotomica =0 if plaquetas_pre_dicotomica <=0.5
replace porc_eosinofilos_post_dic =0 if porc_eosinofilos_post_dic <=0.5
replace porc_eosinofilos_post_dic=1 if porc_eosinofilos_post_dic >0.5 & porc_eosinofilos_post_dic <1
replace razon_cambio_monocitos_dic=0 if razon_cambio_monocitos_dic <=0.5
replace razon_cambio_monocitos_dic=1 if razon_cambio_monocitos_dic >0.5 & razon_cambio_monocitos_dic <1

mi estimate: glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log)
mi estimate: glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dic razon_cambio_monocitos_dic adenopatia_pre, family(poisson) link(log)
mi estimate, eform: glm Estado_final sexo edad_centrada adenopatia_pre infeccion_pre granulocitos_pre_dicotomica plaquetas_pre_dicotomica, family(poisson) link(log)
mi estimate, eform: glm Estado_final sexo edad_centrada infeccion_post porc_eosinofilos_post_dic razon_cambio_monocitos_dic adenopatia_pre, family(poisson) link(log)
