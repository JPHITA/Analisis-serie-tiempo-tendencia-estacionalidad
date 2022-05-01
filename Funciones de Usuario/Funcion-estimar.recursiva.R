#Función de usuario estimar.recursiva() realiza la estimación recursiva de un modelo de regresión lineal
#Invoca además los residuos recursivos, la gráfica CUSUMt-Recursivo y el test CUSUMt-Recursivo
#a través de las funciones recresid(), efp() y sctest(), respectivamente, de la librería strucchange.
#Sus argumentos son los siguientes
#respuesta: Un vector numérico o serie de tiempo con los valores de la respuesta del MRLM.
#data: Un 'data.frame' cuyas columnas son los predictores del MRLM, el número de filas debe ser igual a la longitud de la variable 'respuesta'.
#min.n: Un escalar para indicar el mínimo tamaño de muestra con el que debe iniciar la estimación recursiva
#       no puede ser inferior ncol(data)+2. 
#names.param: Un vector de caracteres con los nombres de los parámetros del MRLM iniciando con el intercepto
#y los demás nombres deben indicar, en el orden de los predictores, el correspondiente parámetro.
#plot.recursive: Argumento lógico, sus valores posibles son TRUE (por defecto) y FALSE, para indicar si se desean o no las gráficas de las estimaciones recursivas de los parámetros del MRLM.

#Resultados: La función muestra en la consola R la estimación del modelo global
#sobre el total de observaciones leidas en el vector 'respuesta', y los resultados del test CUSUM t recursivo, y genera por defecto las gráficas de las estimaciones recursivas para cada parámetro, el gráfico de los residuos recursivos y el gráfico del estadístico CUSUMt. Además guarda una lista con los siguientes componentes:
#n: matriz de una sola columna con sus filas siendo los valores de tamaño de muestra desde 'min.n' hasta 'length(respuesta)'.
#estimacion_recursiva: Un arreglo de matrices, donde cada matriz tiene tres columnas: la estimación de los parámetros y los límites inferior y superior de confianza del 95% de cada parámetro. El número de matrices es igual al total de tamaños de muestras entre  "min.n" y "length(respuesta)".
#ajusteglobal: El objeto tipo lm() del ajuste de la 'respuesta' vs. los predictores en 'data'.
#resid_recursivos: el vector de residuos recursivos.
#test_CUSUM: Los resultados (estadístico de prueba y valor P) del test CUSUMt recursivo

estimar.recursiva=function(respuesta,data,min.n,names.param,plot.recursive=TRUE){
library(strucchange)
names.vars=names(data)
p=ncol(data)+1
m=matrix(min.n:length(respuesta),ncol=1)
ajusteglobal=lm(respuesta~.,data=data)
cat("Ajuste Global")
cat("\n")
print(summary(ajusteglobal))
estim=function(n){
datan=data.frame(data[1:n,])
names(datan)=names.vars
modelo=lm(respuesta[1:n]~.,data=datan)
resul=cbind(coef(modelo),confint(modelo))
resul
}
rr=recresid(ajusteglobal)
test=sctest(respuesta~.,data=data)
cat("\n")
cat("Resultados Test Cusum Recursivo")
cat("\n")
print(test)
b=array(apply(m,1,estim),dim=c(p,3,nrow(m)),dimnames=list(names.param,c("Estimate","LCL","UCL"),paste0("n=",m)))
if(plot.recursive==TRUE){
for(i in 1:p){
pari=t(b[i,,])
win.graph()
matplot(m,pari,type='l',lty=c(1,3,3),col=c(1,4,4),lwd=2,ylab=names.param[i],xlab="n")
abline(h=coef(ajusteglobal)[i],col=2,lwd=2)
legend("topright",legend=c("Estimación recursiva","I.C del 95%","Estimación global"),col=c(1,4,2),lty=c(1,3,1),lwd=2,cex=0.8)
}
win.graph()
plot(rr, type = "l",ylab="Residuales recursivos",xlab="t")
abline(h=0,col=2,lwd=2)
win.graph()
plot(efp(respuesta~.,data=data, type = "Rec-CUSUM"),lwd=2)
}
if(plot.recursive==FALSE){
win.graph()
plot(rr, type = "l",ylab="Residuales recursivos",xlab="t")
abline(h=0,col=2,lwd=2)
win.graph()
plot(efp(respuesta~.,data=data, type = "Rec-CUSUM"),lwd=2)
}
result=list(n=m,estimacion_recursiva=b,ajusteglobal=ajusteglobal,resid_recursivos=rr,test_CUSUM=test)
result
}


