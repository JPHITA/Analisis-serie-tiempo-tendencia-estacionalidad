#Funci�n de usuario estimar.recursiva() realiza la estimaci�n recursiva de un modelo de regresi�n lineal
#Invoca adem�s los residuos recursivos, la gr�fica CUSUMt-Recursivo y el test CUSUMt-Recursivo
#a trav�s de las funciones recresid(), efp() y sctest(), respectivamente, de la librer�a strucchange.
#Sus argumentos son los siguientes
#respuesta: Un vector num�rico o serie de tiempo con los valores de la respuesta del MRLM.
#data: Un 'data.frame' cuyas columnas son los predictores del MRLM, el n�mero de filas debe ser igual a la longitud de la variable 'respuesta'.
#min.n: Un escalar para indicar el m�nimo tama�o de muestra con el que debe iniciar la estimaci�n recursiva
#       no puede ser inferior ncol(data)+2. 
#names.param: Un vector de caracteres con los nombres de los par�metros del MRLM iniciando con el intercepto
#y los dem�s nombres deben indicar, en el orden de los predictores, el correspondiente par�metro.
#plot.recursive: Argumento l�gico, sus valores posibles son TRUE (por defecto) y FALSE, para indicar si se desean o no las gr�ficas de las estimaciones recursivas de los par�metros del MRLM.

#Resultados: La funci�n muestra en la consola R la estimaci�n del modelo global
#sobre el total de observaciones leidas en el vector 'respuesta', y los resultados del test CUSUM t recursivo, y genera por defecto las gr�ficas de las estimaciones recursivas para cada par�metro, el gr�fico de los residuos recursivos y el gr�fico del estad�stico CUSUMt. Adem�s guarda una lista con los siguientes componentes:
#n: matriz de una sola columna con sus filas siendo los valores de tama�o de muestra desde 'min.n' hasta 'length(respuesta)'.
#estimacion_recursiva: Un arreglo de matrices, donde cada matriz tiene tres columnas: la estimaci�n de los par�metros y los l�mites inferior y superior de confianza del 95% de cada par�metro. El n�mero de matrices es igual al total de tama�os de muestras entre  "min.n" y "length(respuesta)".
#ajusteglobal: El objeto tipo lm() del ajuste de la 'respuesta' vs. los predictores en 'data'.
#resid_recursivos: el vector de residuos recursivos.
#test_CUSUM: Los resultados (estad�stico de prueba y valor P) del test CUSUMt recursivo

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
legend("topright",legend=c("Estimaci�n recursiva","I.C del 95%","Estimaci�n global"),col=c(1,4,2),lty=c(1,3,1),lwd=2,cex=0.8)
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


