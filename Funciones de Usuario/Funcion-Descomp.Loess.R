#Función de usuario Descomp.Loess() para ajuste y pronósticos  de validación cruzada, por la combinación del filtro de descomposición con loess
#Requiere la librería fANCOVA
#La función combina el filtro de la descomposición clásica disponible en la función R decompose(), para la estimación y 
#pronóstico de la componente estacional, con la  regresión loess disponible en la función R loess.as() de la librería fANCOVA, 
#para la estimación y pronóstico de la tendencia. Combinando estos resultados mediante un modelo de descomposición aditiva o multiplicativa,
#obtiene la estimación y el pronóstico total de una serie de tiempo, en la estrategia de validación cruzada en la cual se ajusta con n obs.
#y se pronóstican los últimos m datos observados

##Sus argumentos son
 #1. serie.ajuste: Corresponde al conjunto de datos de la muestra de ajuste (serie recortada a n datos)
 #2. h: Longitud de los pronósticos ex-post 
 #3. tipo.descomp: Para indicar el tipo de descomposición sus valores pueden ser "additive" en el caso aditivo ó "multiplicative"
 #   en el caso multiplicativo
 #4. grado: Para especificar el grado del polinomio local, siendo 1 en el loess lineal ó 2 en el loess cuadrático.
 #5. criterio: Para especificar el criterio de información a usar en la selección del parámetro del suavizamiento loess, 
 #   puede ser "aicc" si el criterio es el AICC (Criterio de información de Akaike corregido), ó "gcv"
 #   si el criterio es el GCV (criterio de validación cruzada generalizada)

##Sus resultados: La función produce un objeto tipo lista con los siguientes componentes
 #deltasi: Vector con valores estimados de los efectos estacionales en el orden de los periodos de un año calendario
 #alfa.optim: Escalar, correspondiendo al valor del parámetro de suavizamiento loess óptimo según criterio escogido
 #nep: Escalar, es el número equivalente de parámetros de la regresión loess realizada sobre la serie desestacionalizada
 #de acuerdo al tipo de descomposición
 #p: Escalar, es el número aproximado de párametros del ajuste combinando el fitro de la descomposición
 #en la estimación de la estacionalidad, con la regresión loess en la estimación de la tendencia
 #St: Objeto tipo ts (serie de tiempo) que corresponde a la estimación de la componente estacional en los periodos de ajuste,
 #de acuerdo al tipo de descomposición realizada
 #Tt: Objeto tipo ts (serie de tiempo) que corresponde a la estimación de la componente de tendencia en los periodos de ajuste
 #ytd: Objeto tipo ts (serie de tiempo) que corresponde a la versión desestacionalizada de la serie en lo periodos de ajuste,
 #de acuerdo al tipo de descomposición
 #fitted: Objeto tipo ts (serie de tiempo) que corresponde a la estimación de la serie (juntando tendencia y estacionalidad aditiva o
 #multiplicativamente) en lo periodos de ajuste.
 #tablapron: Objetivo tipo mts (serie de tiempo múltiple) con tres columnas: Pron_Tt (pronóstico de la tendencia), 
 #Pron_St (pronóstico de la estacionalidad) y Pron_serie (pronóstico de la serie al combinar aditiva o multiplicativamente los pronósticos
 #de las componentes).
 #ytpron: Objeto tipo ts (serie de tiempo) que corresponde a los pronósticos de la serie en lo periodos ex-post.
 #residuals: Objeto tipo ts (serie de tiempo) que corresponde a los residuos del ajuste calculados como la diferencia entre valores observados
 #menos valores estimados, en los periodos de ajuste.
 #MSE: Escalar, es una aproximación al MSE del ajuste total de la serie en los periodos de ajuste.

Descomp.Loess=function(serie.ajuste,h,tipo.descomp,grado,criterio){
library(fANCOVA)
library(forecast)
s=tsp(serie.ajuste)[3]
tajuste=1:length(serie.ajuste)
tpron=(length(serie.ajuste)+1):(length(serie.ajuste)+h)
if(end(serie.ajuste)[2]<s){
start.pron=c(end(serie.ajuste)[1],end(serie.ajuste)[2]+1)
}
if(end(serie.ajuste)[2]==s){
start.pron=c(end(serie.ajuste)[1]+1,1)
}


descom=decompose(serie.ajuste,type=tipo.descomp)
St=descom$seasonal
estacionini=cycle(serie.ajuste)[1]
if(estacionini==1){
deltas_i=descom$figure
}
if(estacionini!=1){
j=estacionini;deltas_i=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
}
efectos=deltas_i
if(s==12){
names(efectos)=c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12")
}
if(s==4){
names(efectos)=c("s1","s2","s3","s4")
}
efectos=data.frame(efectos)
names(efectos)=""
cat("Efectos estacionales estimados")
print(efectos)

indi=seasonaldummy(serie.ajuste,h)
indi2=cbind(indi,1-apply(indi,1,sum))
Stnuevo=ts(indi2%*%deltas_i,frequency=s,start=start.pron)

if(tipo.descomp=="additive"){
ytd=serie.ajuste-St
}
if(tipo.descomp=="multiplicative"){
ytd=serie.ajuste/St
}
ajusteLoess1=loess.as(tajuste,ytd,degree=grado,criterion=criterio,family="gaussian",plot=F)
cat("\n")
cat("Resumen Loess sobre serie desestacionalizada:")
cat("\n")
print(summary(ajusteLoess1))
alfa.optim1=ajusteLoess1$pars$span 
nep1=round(ajusteLoess1$enp)
p1=nep1+s-1 #número aproximado de parámetros en ajuste Modelo
Tt1=ts(fitted(ajusteLoess1),frequency=s,start=start(serie.ajuste))
Ttnuevo1=predict(loess(ytd~tajuste,span=alfa.optim1,degree=grado,control=loess.control(surface="direct")),data.frame(tajuste=tpron),se=FALSE)
Ttnuevo1=ts(Ttnuevo1,freq=s,start=start.pron)
if(tipo.descomp=="additive"){
ythat1=St+Tt1
ytpron1=Ttnuevo1+Stnuevo
}
if(tipo.descomp=="multiplicative"){
ythat1=St*Tt1
ytpron1=Ttnuevo1*Stnuevo
}
tablapron1=cbind(Pron_Tt=Ttnuevo1,Pron_St=Stnuevo,Pron_serie=ytpron1)
et1=serie.ajuste-ythat1
df1=length(serie.ajuste)-nep1+s-1 
MSE1=sum(et1^2)/df1 
result=list(deltasi=deltas_i,alfa.optim=alfa.optim1,nep=nep1,p=p1,St=St,Tt=Tt1,ytd=ytd,fitted=ythat1,tablapron=tablapron1,ytpron=ytpron1,residuals=et1,MSE=MSE1)
result
}

