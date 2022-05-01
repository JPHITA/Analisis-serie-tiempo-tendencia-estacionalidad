# librerias y funciones ####################################################################################
rm(list=ls(all=TRUE))

library(forecast)
library(TSA)
library(fANCOVA)

source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-SuavizamientoEstacional.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-Descomp.Loess.R")
############################################################################################################

# lectura de datos y grafica de la serie ###################################################################
#Para leer "anexos-emmet-noviembre-2021-1-Elaboracion de productos de panaderia macarrones fideos y sus productos-modif.csv", col 6: indice de produccion real
Datos10=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos10=ts(Datos10,freq=12,start=c(2001,1))

plot(Datos10, xlab="Tiempo", ylab="Indice de produccion real")            # grafica en escala original
plot(log(Datos10), xlab="Tiempo", ylab="log(Indice de produccion real)")  # grafica en escala log
############################################################################################################

# graficos de residules de descomposicion aditiva y multiplicativa #########################################
plot(decompose(Datos10, type="additive")$random, ylab="Errores desc. aditiva")
abline(h=0, col="red")

plot(decompose(Datos10, type="multiplicative")$random, ylab="Errores desc. multiplciativa")
abline(h=1, col="red")
##########################################################################################################

# analisis descriptivo de la serie ####################################################################
plot(decompose(log(Datos10), type = "additive")$trend, ylim=range(log(Datos10)))  # tendencia

boxplot(log(Datos10)~cycle(log(Datos10)),names=month.abb) # boxplot para evaluar la  estacionalidad

periodogram(diff(log(Datos10)))                           # periodograma
abline(v=c((1:6)/12, 0.35),col=2,lty=2)
#########################################################################################################

# variables para le ajuste de modelos ###################################################################
s <- 12               # periodos en el año
N <- length(Datos10)  # tamaño de todos los datos
m <- 12               # tamaño de los datos para pronostico
n <- N - m            # tamaño de los datos para ajuste

t.ajuste <- 1:n       # indice de tiempo para el ajuste
t.pronost <- (n+1):N  # indice de tiempo para los pronosticos

Yt <- ts(Datos10[t.ajuste], freq=12, start=c(2001,1))           # valores reales del ajuste
Yt.pr <- ts(Datos10[t.pronost], freq=12, start=c(2020, 12))   # valores reales de los pronosticos
#########################################################################################################

# ajuste modelo 1 #######################################################################################
# ajuste modelo 1 (log-polinomial)

# sin1 <- sin(pi*(1/6)*t.ajuste)
# sin2 <- sin(pi*(2/6)*t.ajuste)
# sin3 <- sin(pi*(3/6)*t.ajuste)
# sin4 <- sin(pi*(4/6)*t.ajuste)
# sin5 <- sin(pi*(5/6)*t.ajuste)
# 
# cos1 <- cos(pi*(1/6)*t.ajuste)
# cos2 <- cos(pi*(2/6)*t.ajuste)
# cos3 <- cos(pi*(3/6)*t.ajuste)
# cos4 <- cos(pi*(4/6)*t.ajuste)
# cos5 <- cos(pi*(5/6)*t.ajuste)
# cos6 <- cos(pi*(6/6)*t.ajuste)
# 
# sin7 <- sin(2*pi*(0.35)*t.ajuste)
# cos7 <- cos(2*pi*(0.35)*t.ajuste)

t <- t.ajuste
t2 <- t.ajuste^2
t3 <- t.ajuste^3

sin1 <- sin(pi*t.ajuste/6)
sin2 <- sin(pi*t.ajuste/3)
sin3 <- sin(pi*t.ajuste/2)
sin4 <- sin(2*pi*t.ajuste/3)
sin5 <- sin(5*pi*t.ajuste/6)

cos1 <- cos(pi*t.ajuste/6)
cos2 <- cos(pi*t.ajuste/3)
cos3 <- cos(pi*t.ajuste/2)
cos4 <- cos(2*pi*t.ajuste/3)
cos5 <- cos(5*pi*t.ajuste/6)
cos6 <- cos(pi*t.ajuste)

sin7 <- sin(pi*0.7*t.ajuste)
cos7 <- cos(pi*0.7*t.ajuste)

X <- data.frame(t, t2, t3, sin1, cos1, sin2, cos2, sin3, cos3, sin4, cos4, sin5, cos5, cos6, sin7, cos7)

modelo1 <- lm(log(Yt) ~ . , data = X)

summary(modelo1)

# grafica de ajuste
png()
Yt.ajustados1 <- ts(exp(fitted(modelo1)) * exp(summary(modelo1)$sigma^2 / 2 ), freq=12, start=start(Yt))
plot(Datos10, ylab="Produccion real", xlab="Tiempo")
lines(Yt.ajustados1, col="red")
legend("topleft",legend=c("Original","Ajuste Modelo 1"),col=c(1,2),lty=1)
dev.off()

# CI
seudoresid1 <- Yt - Yt.ajustados1
nparams1 <- length(coef(modelo1)[coef(modelo1)!=0])
exp.crit.inf.resid(seudoresid1, nparams1)

# analisis de residuales
png()
plot.ts(residuals(modelo1), ylab="residuales modelo 1", xlab="tiempo")
abline(h=0, col="red")
abline(h = c( 2*summary(modelo1)$sigma, -2*summary(modelo1)$sigma ), col="red")
dev.off()

png()
plot(fitted(modelo1), residuals(modelo1), ylab="residuales modelo 1", xlab="valores ajustados")
abline(h=0, col="red")
abline(h = c( 2*summary(modelo1)$sigma, -2*summary(modelo1)$sigma ), col="red")
dev.off()

# pronosticos

# sin1.pr <- sin(pi*(1/6)*t.pronost)
# sin2.pr <- sin(pi*(2/6)*t.pronost)
# sin3.pr <- sin(pi*(3/6)*t.pronost)
# sin4.pr <- sin(pi*(4/6)*t.pronost)
# sin5.pr <- sin(pi*(5/6)*t.pronost)
# 
# cos1.pr <- cos(pi*(1/6)*t.pronost)
# cos2.pr <- cos(pi*(2/6)*t.pronost)
# cos3.pr <- cos(pi*(3/6)*t.pronost)
# cos4.pr <- cos(pi*(4/6)*t.pronost)
# cos5.pr <- cos(pi*(5/6)*t.pronost)
# cos6.pr <- cos(pi*(6/6)*t.pronost)
# 
# sin7.pr <- sin(2*pi*(0.35)*t.pronost)
# cos7.pr <- cos(2*pi*(0.35)*t.pronost)

t <- t.pronost
t2 <- t.pronost^2
t3 <- t.pronost^3

sin1 <- sin(pi*t.pronost/6)
sin2 <- sin(pi*t.pronost/3)
sin3 <- sin(pi*t.pronost/2)
sin4 <- sin(2*pi*t.pronost/3)
sin5 <- sin(5*pi*t.pronost/6)

cos1 <- cos(pi*t.pronost/6)
cos2 <- cos(pi*t.pronost/3)
cos3 <- cos(pi*t.pronost/2)
cos4 <- cos(2*pi*t.pronost/3)
cos5 <- cos(5*pi*t.pronost/6)
cos6 <- cos(pi*t.pronost)

sin7 <- sin(pi*0.7*t.pronost)
cos7 <- cos(pi*0.7*t.pronost)

X.pr <- data.frame(t, t2, t3, sin1, cos1, sin2, cos2, sin3, cos3, sin4, cos4, sin5, cos5, cos6, sin7, cos7)

# pronosticos
predict1 <- exp(predict(modelo1, newdata = X.pr, interval = "prediction")) * exp(summary(modelo1)$sigma^2 / 2)
predict1 = ts(predict1, freq=12, start=start(Yt.pr))

# precision de pronosticos y calidad de intervalos
accuracy(predict1[,1], Yt.pr)
amplitud.cobertura(Yt.pr, predict1[, 2], predict1[, 3])
#########################################################################################################

# ajuste modelo 2 #######################################################################################
# ajuste modelo 2 (exponencial polinomial)
noms <- c("beta0", "beta1", "beta2", "beta3", "alpha1","gamma1", "alpha2","gamma2", "alpha3","gamma3", "alpha4","gamma4", "alpha5","gamma5", "gamma6", "alpha7","gamma7")

modelo2 <- regexponencial(Yt, X, noms)

summary(modelo2)

# grafica de ajuste
png()
Yt.ajustados2 <- ts(fitted(modelo2), freq=12, start=c(2001,1)) # valores ajustados
plot(Datos10, ylab="Produccion real", xlab="Tiempo")
lines(Yt.ajustados2, col="red")
legend("topleft",legend=c("Original","Ajuste Modelo 2"),col=c(1,2),lty=1)
dev.off()

# CI
exp.crit.inf.resid(residuals(modelo2), length(coef(modelo2)[coef(modelo2)!=0]))

# analisis de residuales
png()
plot.ts(residuals(modelo2), ylab="residuales modelo 2", xlab="tiempo")
abline(h=0, col="red")
abline(h = c( 2*summary(modelo2)$sigma, -2*summary(modelo2)$sigma ), col="red")
dev.off()

png()
plot(fitted(modelo2), residuals(modelo2), ylab="residuales modelo 2", xlab="valores ajustados")
abline(h=0, col="red")
abline(h = c( 2*summary(modelo2)$sigma, -2*summary(modelo2)$sigma ), col="red")
dev.off()

#pronosticos
predict2 <- ts(predict(modelo2, newdata = X.pr, interval = "prediction"), freq=12, start=start(Yt.pr))
predict2

# precision de pronosticos y calidad de intervalos
accuracy(predict2, Yt.pr)
#########################################################################################################

# ajuste modelo 3 #######################################################################################
# ajuste modelo 3 (SEHW)
modelo3 <- SuavizamientoEstacional(Yt, seasonal = "multiplicative", h = m)

# grafica de ajuste
png()
plot(Datos10, ylab="Produccion real", xlab="Tiempo")
lines(fitted(modelo3), col="red")
legend("topleft",legend=c("Original","Ajuste Modelo 3"),col=c(1,2),lty=1)
dev.off()

# CI
exp.crit.inf.resid(residuals(modelo3), (s-1)+2)

# analisis de residuales
png()
plot.ts(residuals(modelo3), ylab="residuales modelo 3", xlab="tiempo")
abline(h=0, col="red")
abline(h = c( 2*sqrt(modelo3$MSE), -2*sqrt(modelo3$MSE) ), col="red")
dev.off()

png()
plot(fitted(modelo3), residuals(modelo3), ylab="residuales modelo 3", xlab="valores ajustados")
abline(h=0, col="red")
abline(h = c( 2*sqrt(modelo3$MSE), -2*sqrt(modelo3$MSE) ), col="red")
dev.off()

#pronosticos
modelo3$forecast

# precision de pronosticos y calidad de intervalos
accuracy(modelo3$forecast[,1], Yt.pr)
amplitud.cobertura(Yt.pr,modelo3$forecast[,2], modelo3$forecast[,3])
#########################################################################################################

# ajuste modelo 4 #######################################################################################
# ajuste modelo 4 (DLL(AICC))
modelo4 <-Descomp.Loess(Yt, h=m, tipo.descomp = "multiplicative", grado=1, criterio = "aicc")

# grafica de ajuste
png()
plot(Datos10, ylab="Produccion real", xlab="Tiempo")
lines(modelo4$fitted, col="red")
legend("topleft",legend=c("Original","Ajuste Modelo 4"),col=c(1,2),lty=1)
dev.off()

# grafica de la serie desestacionalizada y la tendencia estimada con loess
png()
plot(modelo4$ytd, xlab="Tiempo")
lines(modelo4$Tt, col="red", lwd=2)
legend("topleft",legend=c("Serie ajustada estacionalmente","Tendencia LOESS lineal (AICC)"),col=c(1,2),lty=1)
dev.off()

# grafica de la estacionalidad
png()
plot(modelo4$St, xlab="Tiempo", ylab="St")
dev.off()

# grafica efectos estacionales H-W y filtro de la descomposicion
png()
plot(1:12, modelo4$deltasi, type="l", xlab="Mes del años", ylab="", ylim=c(min(modelo4$deltasi),max(coef(modelo3)[3:14,])+0.05))
lines(coef(modelo3)[-c(1,2),], lty=2,lwd=2,col=2)
legend("topleft",legend=c("Efectos estacionales Filtro de descomposicion", "Efectos estacionales H-W en t=239"),col=1:2,lty=1:2,lwd=3)
dev.off()


# CI
exp.crit.inf.resid(residuals(modelo4), modelo4$p)

# analisis de residuales
png()
plot.ts(residuals(modelo4), ylab="residuales modelo 4", xlab="tiempo")
abline(h=0, col="red")
abline(h = c( 2*sqrt(modelo4$MSE), -2*sqrt(modelo4$MSE) ), col="red")
dev.off()

png()
plot(fitted(modelo4), residuals(modelo4), ylab="residuales modelo 4", xlab="valores ajustados")
abline(h=0, col="red")
abline(h = c( 2*sqrt(modelo4$MSE), -2*sqrt(modelo4$MSE) ), col="red")
dev.off()

# pronosticos
modelo4$ytpron

# precision de pronosticos y calidad de intervalos
accuracy(modelo4$ytpron, Yt.pr)
#########################################################################################################

# grafica de pronosticos y valores reales ###############################################################
win.graph()
plot(Yt.pr, lty=1,col="black",type="b",pch=19,xaxt="n",lwd=4, ylim=c(99, 125), xlab="Tiempo", ylab="Indice de produccion real")
axis(1,at=time(Yt.pr),labels=c("2020 12", paste0("2021 ", 1:11)))

lines(predict1[,1],lty=2,col="red",type="b",pch=2,lwd=2)
lines(predict2,lty=3,col="blue",type="b",pch=3,lwd=2)
lines(modelo3$forecast[,1],col="green",pch=4,lty=4,type="b",lwd=2)
lines(modelo4$ytpron,col="orange",pch=5,lty=5,type="b",lwd=2)

legend("topleft",
       lwd=2,
       legend=c("Real","Modelo 1","Modelo 2","Modelo 3","Modelo 4"),
       lty=c(1:5),
       pch=c(19,2:5),
       col=c("black","red","blue","green","orange"))
#########################################################################################################

