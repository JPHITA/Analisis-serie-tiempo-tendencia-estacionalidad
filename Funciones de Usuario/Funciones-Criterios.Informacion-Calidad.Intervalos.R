#--------------------------------------------------------------------------------------------------
#Función usuario exp.crit.inf.resid() para calcular AIC y BIC versión exp(C_n^*(p))
##Argumentos:
 #residuales: un vector numérico que debe corresponder a la diferencia Yt-Ythat, donde Yt es
 #el valor observado de la serie en su escala original y Ythat su valor estimado en la misma escala
 #NOTA: en modelos con transformación de la serie, Yt-Ythat son los seudoresiduos y en modelos con 
 #ajuste sin transformar a la serie esta diferencia corresponde a residuos de ajuste.
 #n.par: Es el número de parámetros atribuido al modelo ajustado

##Resultados arrojados por esta función: un vector con los valores del número de parámetros (p), el AIC y el BIC.

exp.crit.inf.resid=function(residuales,n.par){
#Calcula AIC
AIC=exp(log(mean(residuales^2))+2*n.par/length(residuales))
#Calcula BIC
BIC=exp(log(mean(residuales^2))+n.par*log(length(residuales))/length(residuales))
crit=list(p=n.par,AIC=AIC,BIC=BIC)
unlist(crit)
}

#--------------------------------------------------------------------------------------------------
#Función para calcular la amplitud media y la cobertura (%) de "m">1 Intervalos de Predicción (I.P)
##Argumentos:
  #real: serie de tiempo o vector numérico con los valores observados en los periodos de pronóstico
  #LIP, LSP: Vectores numéricos con los valores de los límites inferior y superior, respectivamente,   #de los intervalos de predicción

##Resultados arrojados por esta función: un vector con los valores de la amplitud promedio y la cobertura (en porcentaje) de los "m" I.P  

amplitud.cobertura=function(real,LIP,LSP){
a=LSP-LIP
am=mean(a)
I=ifelse(real>=LIP & real<=LSP,1,0)
p=mean(I)*100
res=list(Amplitud=am,"Cobertura(%)"=p)
unlist(res)
}


