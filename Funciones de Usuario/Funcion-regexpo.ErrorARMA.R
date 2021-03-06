#Funci�n de usuario regexpo.ErrorARMA() para el ajuste y pron�stico de modelos de regresi�n exponenciales con error ARMA, hace un ajuste aproximado en el cual estima primero los par�metros de las componentes estructurales
#(tendencia y estacionalidad) por m�nimos cuadrados no lineales, luego, ajusta a los residuos estructurales de este modelo, un modelo ARMA estacionario de media cero, mediante la funci�n Arima() de la librer�a forecast. 
#Los valores estimados de la serie son aproximados sumando las estimaciones del ajuste de las componentes estructurales y las estimaciones obtenidas del modelo ARMA sobre los 
# residuos estructurales del modelo preliminar. El anterior procedimiento tambi�n es aplicado para la construcci�n de pron�sticos puntuales (no hay posibilidad de realizar pron�sticos por intervalo). 

#Los argumentos son:

# respuesta:     Un objeto ts con los datos a ser ajustados.
# names.param:   Un vector de caracteres con los nombres para los par�metros en la funci�n de regresi�n de las componentes estructurales.
#data, newdata:  Objetos tipo data.frame, cuyas columnas corresponden a los valores de las variables predictoras del modelo en el ajuste y en el pron�stico, respectivamente.
# control:       una lista opcional de configuraciones de control para la funci�n nls(). Consulte nls.control() para conocer los nombres de los valores de control
#                configurables y su efecto.
# order:         Una especificaci�n de la parte no estacional del modelo ARIMA: los tres componentes (p, d, q) son el orden AR, el grado de diferenciaci�n y el orden MA, respectivamente.
# seasonal:      Una especificaci�n de la parte estacional del modelo ARIMA, m�s el per�odo (que por defecto es frequency(respuesta)). Este argumente debe ser dado como una lista 
#                con el orden de los componentes y el per�odo, pero una especificaci�n de solo un vector num�rico de longitud 3 se convertir� en una lista adecuada con las especificaciones como el orden
# fixed:         Vector num�rico opcional de la misma longitud que la suma de los �rdenes de la ecuaci�n del ARMA estacionario de media cero a ajustar sobre residuos estructurales del ajuste exponencial. 
#                Ver ayuda de funci�n arima() sobre detalles de este argumento.
# method:        M�todo de ajuste: m�xima verosimilitud ("ML"), M�xima verosilmilitud combinada con m�nimos cuadrados condicionales ("CSS-ML") o m�nimos de cuadrados condicionales ("CSS"). 
#                El valor predeterminado (a menos que existan valores faltantes) es "CSS-ML", en el cual se usan m�nimos cuadrados condicionales para encontrar los valores iniciales y 
#                luego aplica m�xima verosimilitud. 
# optim.method:  El valor pasado al argumento  �method� para �optim� en la funci�n Arima(). Por defecto es "BFGS".
# optim.control: Una lista de par�metros de control para �optim�, usandos en la funci�n Arima().

# Resultados:    La funci�n produce una lista con las siguientes componentes
# tabla:         Matriz con la tabla de par�metros estimados, sus errores est�ndar, estad�stico T0 y valor P asociado. Tenga en cuenta que no hay una estimaci�n conjunta de los par�metros
#                de regresi�n de la funci�n exponencial y de los par�metros del modelo ARMA del error estructural, de modo que los errores est�ndar provienen del ajuste separado de las dos 
#                estructuras, pero los valores p son calculados bajo una distribuci�n t-student cuyos grados de libertad son df=n - total de par�metros. el objeto tabla puede ser obtenido
#                con la funci�n coef() aplicada al objeto R donde se guarde el resultado de la funci�n regexpo.ErrorARMA().
# fitted:        Objeto tipo ts con los valores ajustados de la respuesta. Estos valores pueden ser accesados mediante la funci�n fitted() 
#                sobre el objeto R donde se guarde el resultado de la funci�n regexpo.ErrorARMA().
# residuals:     Objeto tipo ts con los residuos del ajuste de la respuesta. Estos valores pueden ser accesados mediante la funci�n residuals() 
#                sobre el objeto R donde se guarde el resultado de la funci�n regexpo.ErrorARMA(). 
# sigma2:        num�rico con la estimaci�n de la varianza de las innovaciones del modelo ARMA definido para el error estructural del modelo de regresi�n. Puede ser accesado de la siguiente manera
#                nombre_objeto$sigma2, donde 'nombre_objeto' es el nombre del objeto R donde se guarda el resultado de la funci�n regexpo.ErrorARMA().
# forecast:      Objeto tipo ts con los pron�sticos puntuales de la respuesta para h=ncol(newdata) periodos despu�s del ajuste.


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
