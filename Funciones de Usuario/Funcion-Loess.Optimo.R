#Función de usuario Loess.Optimo(): Es una función diseñada para ajustes y pronósticos de una serie no estacional, mediante loess óptimo
#escogiendo con criterio AICC o GCV el valor del parámetro de suavizamiento loess. Esta función opera con la librería fANCOVA 
#de la cual hace uso de la función loess.as(), como tambiés de la función loess() y predict(). Sus argumentos son
#serie: Objeto ts() (serie de tiempo) sobre el cual se hará el ajuste loess
#grado: El grado del polinomio local, 1 en el caso lineal y 2 en el cuadrático
#criterio: Criterio a usar para la escogencia del parámetro de suavizamiento loess, puede ser "aicc" o "gcv"
#h: Número de periodos requeridos de pronósticos después del último tiempo de ajuste
#level: Nivel de confianza para los I.P, por defecto es 0.95

#Sus resultados: La función crea un objeto lista con los siguientes componentes
#alfa.optim: el valor del parámetro de suavizamiento óptimo encontrado según el criterio usado
#nep: el número de parámetros equivalentes loess
#MSE: una aproximación del MSE del ajuste
#fitted: objeto ts() con los valores ajustados de la serie
#residuals: objeto ts(9 con los valores de los residuos del ajuste.
#forecast: objeto mts() con los pronósticos puntuales y los límites de predicción inferior (lwr) y superior (upr)

Loess.Optimo=function(serie,grado,criterio,h,level=0.95){
library(fANCOVA)
ta=1:length(serie)
s=frequency(serie)
n=length(serie)
ajuste=loess.as(ta,serie,degree=grado,criterion=criterio,family="gaussian",plot=F)
print(summary(ajuste))
alpha=ajuste$pars$span
ythat=ts(fitted(ajuste),freq=s,start=start(serie))
MSE=sum(residuals(ajuste)^2)/(n-round(ajuste$enp))
residuos=ts(residuals(ajuste),freq=s,start=start(serie))
tpron=(length(serie)+1):(length(serie)+h)
if(end(serie)[2]<s){
start.pron=c(end(serie)[1],end(serie)[2]+1)
}
if(end(serie)[2]==s){
start.pron=c(end(serie)[1]+1,1)
}


aux1p=loess(serie~ta,degree=grado,span=alpha,family="gaussian",control=loess.control(surface="direct"))
aux2p=predict(aux1p,data.frame(ta=tpron),se=T)
ytpron=aux2p$fit
LI=ytpron-qt((1-level)/2,df=aux2p$df,lower.tail=F)*aux2p$se.fit
LP=ytpron+qt((1-level)/2,df=aux2p$df,lower.tail=F)*aux2p$se.fit
tablapron=ts(data.frame("Point forecast"=ytpron,lwr=LI,upr=LP),freq=s,start=start.pron)
resul=list(alfa.optim=alpha,nep=ajuste$enp,MSE=MSE,fitted=ythat,residuals=residuos,forecast=tablapron)
resul
}

