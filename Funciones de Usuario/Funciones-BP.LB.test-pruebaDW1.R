#Funciones disponibles en este archivo son las siguientes.
#Función de usuario BP.LB.test() para realizar las pruebas Ljung-Box o Box-Pierce
#para m=6, 12, 18,..., hasta m=[maxlag/6] donde maxlag es el máximo orden de rezagos
#hasta el que se desea aplicar el test. Esta función hace uso de la función R Box.test() 
#Argumentos
# serie:  vector de valores de una realización muestral del proceso sobre el que se quiere evaluar
#         el supuesto de ruido blanco a traves de los tests de incorrelacion Ljung-Box o Box-Pierce
# maxlag: Entero para especificar el máximo orden de rezago temporal hasta el que se quiere probar la significacia de la función de aoticorrelación
# type:   cadena de caracteres para indicar el tipo de estadísticos de prueba deseado, "Ljung" (valor establecido por defecto) para las pruebas Ljung-Box
#         "Box" para solicitar las pruebas con estadístico tipo Box-Pierce. 
#Resultados
# La función crea un objeto "data.frame" con tres variables
# X.squared: los valores de los estadísticos de prueba para m=6, 12,...,[maxlag/6]
# df: los grados de libertad del estadístico de prueba, corresponden a los valores de m=6, 12,...,[maxlag/6]
# p.value: los valores P para las pruebas con m=6, 12,...,[maxlag/6]

BP.LB.test=function(serie,maxlag,type="Ljung"){
aux=floor(maxlag/6);
X.squared=c(rep(NA,aux))
df=c(rep(NA,aux))
p.value=c(rep(NA,aux))
for(i in 1:aux){
test=Box.test(serie,lag=(6*i),type=type)
X.squared[i]=test[[1]]
df[i]=test[[2]]
p.value[i]=test[[3]]
}
lag=6*c(1:aux)
teste=as.data.frame(cbind(X.squared,df,p.value))
rownames(teste)=lag
teste
}

#Función de usuario pruebaDW1() para evaluar el test Durbin-Watson para autocorrelación de orden 1 en MRLM
#Usa la función R durbinWatsonTest() de la librería car, y proporciona simultáneamente los valores P correspondientes a los tests
#con hipótesis alternativa de autocorrelación de orden 1 positiva y negativa.
#Argumentos:
#modelo: Un objeto lm en el que se guardó el ajuste de un MRLM.
#Resultados:
# La función crea un objeto tipo "data.frame" que contiene las siguientes variables
#rho(1) estimado: el valor estimado para la autocorrelación de orden 1 usando los residuales del modelo
#Estadistico D-W: Valor del estadístico de la prueba, que en el curso denotamos por d1
#VP H1: rho(1)>0: el valor P para el test H0: rho(1)=0, vs. H1:rho(1)>0
#VP H1: rho(1)<0: el valor P para el test H0: rho(1)=0, vs. H1:rho(1)<0

pruebaDW1=function(modelo){
dwneg=durbinWatsonTest(modelo,max.lag=1,method="normal",alternative="negative")
dwpos=durbinWatsonTest(modelo,max.lag=1,method="normal",alternative="positive")
res=data.frame(dwneg$r,dwneg$dw,dwpos$p,dwneg$p,row.names="Resultados")
names(res)=c("rho(1) estimado","Estadistico D-W","VP H1: rho(1)>0","VP H1: rho(1)<0")
res
}


