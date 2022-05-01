#Función de usuario regexpo.ErrorARMA() para el ajuste y pronóstico de modelos de regresión exponenciales con error ARMA, hace un ajuste aproximado en el cual estima primero los parámetros de las componentes estructurales
#(tendencia y estacionalidad) por mínimos cuadrados no lineales, luego, ajusta a los residuos estructurales de este modelo, un modelo ARMA estacionario de media cero, mediante la función Arima() de la librería forecast. 
#Los valores estimados de la serie son aproximados sumando las estimaciones del ajuste de las componentes estructurales y las estimaciones obtenidas del modelo ARMA sobre los 
# residuos estructurales del modelo preliminar. El anterior procedimiento también es aplicado para la construcción de pronósticos puntuales (no hay posibilidad de realizar pronósticos por intervalo). 

#Los argumentos son:

# respuesta:     Un objeto ts con los datos a ser ajustados.
# names.param:   Un vector de caracteres con los nombres para los parámetros en la función de regresión de las componentes estructurales.
#data, newdata:  Objetos tipo data.frame, cuyas columnas corresponden a los valores de las variables predictoras del modelo en el ajuste y en el pronóstico, respectivamente.
# control:       una lista opcional de configuraciones de control para la función nls(). Consulte nls.control() para conocer los nombres de los valores de control
#                configurables y su efecto.
# order:         Una especificación de la parte no estacional del modelo ARIMA: los tres componentes (p, d, q) son el orden AR, el grado de diferenciación y el orden MA, respectivamente.
# seasonal:      Una especificación de la parte estacional del modelo ARIMA, más el período (que por defecto es frequency(respuesta)). Este argumente debe ser dado como una lista 
#                con el orden de los componentes y el período, pero una especificación de solo un vector numérico de longitud 3 se convertirá en una lista adecuada con las especificaciones como el orden
# fixed:         Vector numérico opcional de la misma longitud que la suma de los órdenes de la ecuación del ARMA estacionario de media cero a ajustar sobre residuos estructurales del ajuste exponencial. 
#                Ver ayuda de función arima() sobre detalles de este argumento.
# method:        Método de ajuste: máxima verosimilitud ("ML"), Máxima verosilmilitud combinada con mínimos cuadrados condicionales ("CSS-ML") o mínimos de cuadrados condicionales ("CSS"). 
#                El valor predeterminado (a menos que existan valores faltantes) es "CSS-ML", en el cual se usan mínimos cuadrados condicionales para encontrar los valores iniciales y 
#                luego aplica máxima verosimilitud. 
# optim.method:  El valor pasado al argumento  ‘method’ para ‘optim’ en la función Arima(). Por defecto es "BFGS".
# optim.control: Una lista de parámetros de control para ‘optim’, usandos en la función Arima().

# Resultados:    La función produce una lista con las siguientes componentes
# tabla:         Matriz con la tabla de parámetros estimados, sus errores estándar, estadístico T0 y valor P asociado. Tenga en cuenta que no hay una estimación conjunta de los parámetros
#                de regresión de la función exponencial y de los parámetros del modelo ARMA del error estructural, de modo que los errores estándar provienen del ajuste separado de las dos 
#                estructuras, pero los valores p son calculados bajo una distribución t-student cuyos grados de libertad son df=n - total de parámetros. el objeto tabla puede ser obtenido
#                con la función coef() aplicada al objeto R donde se guarde el resultado de la función regexpo.ErrorARMA().
# fitted:        Objeto tipo ts con los valores ajustados de la respuesta. Estos valores pueden ser accesados mediante la función fitted() 
#                sobre el objeto R donde se guarde el resultado de la función regexpo.ErrorARMA().
# residuals:     Objeto tipo ts con los residuos del ajuste de la respuesta. Estos valores pueden ser accesados mediante la función residuals() 
#                sobre el objeto R donde se guarde el resultado de la función regexpo.ErrorARMA(). 
# sigma2:        numérico con la estimación de la varianza de las innovaciones del modelo ARMA definido para el error estructural del modelo de regresión. Puede ser accesado de la siguiente manera
#                nombre_objeto$sigma2, donde 'nombre_objeto' es el nombre del objeto R donde se guarda el resultado de la función regexpo.ErrorARMA().
# forecast:      Objeto tipo ts con los pronósticos puntuales de la respuesta para h=ncol(newdata) periodos después del ajuste.


regexpo.ErrorARMA=function(respuesta,names.param,data,newdata,control=nls.control(),order= c(0L, 0L, 0L),seasonal=list(order = c(0L, 0L, 0L),period=NA),fixed=NULL,method=c("CSS-ML", "ML", "CSS"),optim.method="BFGS",optim.control = list()){
yt=respuesta
model.aux=lm(log(yt)~.,data)
names.vars=c(1,names(data))
miformula=as.formula(paste("yt~",paste(paste("exp(",paste(names.param,names.vars,sep="*",collapse="+"),sep=""),")",sep="")))
coef0=as.list(coef(model.aux));names(coef0)=as.list(names.param)
modelo=nls(miformula,start=coef0,control=control,data=data)
serie.et=ts(residuals(modelo),freq=frequency(yt),start=start(yt))
ciclos=Arima(serie.et,order=order,seasonal=seasonal,include.mean=F,fixed=fixed,method=method,optim.method=optim.method,optim.control=optim.control)
p1=length(coef(modelo)[coef(modelo)!=0])
p2=p1+length(coef(ciclos)[coef(ciclos)!=0])
df=length(yt)-p2
tabla=rbind(coeftest(ciclos,df=df),coeftest(modelo,df=df))[,1:4]
ythat=ts(fitted(modelo),freq=frequency(yt),start=start(yt))+fitted(ciclos)
sigma2=ciclos$sigma2
s=tsp(yt)[3]
tajuste=1:length(yt)
h=ncol(newdata)
tpron=(length(yt)+1):(length(yt)+h)
if(end(yt)[2]<s){
start.pron=c(end(yt)[1],end(yt)[2]+1)
}
if(end(yt)[2]==s){
start.pron=c(end(yt)[1]+1,1)
}
resid=residuals(ciclos)
ytpron=ts(predict(modelo,newdata=newdata,interval="prediction",level=0.95),freq=frequency(yt),start=start.pron)+ts(forecast(ciclos,h=h)$mean,freq=frequency(yt),start=start.pron)
result=list(coefficients=tabla,fitted=ythat,residuals=resid,sigma2=sigma2,p=p2,forecast=ytpron)
result
}
