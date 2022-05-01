rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-total industria-modif.csv", col 5: Índice de producción nominal
Datos1=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos1=ts(Datos1,freq=12,start=c(2001,1))
plot(Datos1)
#-----------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-total industria-modif.csv", col 6: Índice de producción real
Datos2=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos2=ts(Datos2,freq=12,start=c(2001,1))
plot(Datos2)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-total industria-modif.csv", col 7: Índice de ventas nominales
Datos3=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos3=ts(Datos3,freq=12,start=c(2001,1))
plot(Datos3)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-total industria-modif.csv", col 8: Índice de ventas reales
Datos4=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos4=ts(Datos4,freq=12,start=c(2001,1))
plot(Datos4)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de bebidas-modif.csv", col 5: Índice de producción nominal
Datos5=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos5=ts(Datos5,freq=12,start=c(2001,1))
plot(Datos5)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de bebidas-modif.csv", col 5: Índice de producción real
Datos6=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos6=ts(Datos6,freq=12,start=c(2001,1))
plot(Datos6)


#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de bebidas-modif.csv", col 7: Índice de ventas nominales
Datos7=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos7=ts(Datos7,freq=12,start=c(2001,1))
plot(Datos7)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de bebidas-modif.csv", col 8: Índice de ventas reales
Datos8=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos8=ts(Datos8,freq=12,start=c(2001,1))
plot(Datos8)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Elaboracion de productos de panaderia macarrones fideos y sus productos-modif.csv", col 5: Índice de producción nominal
Datos9=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos9=ts(Datos9,freq=12,start=c(2001,1))
plot(Datos9)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Elaboracion de productos de panaderia macarrones fideos y sus productos-modif.csv", col 6: Índice de producción real
Datos10=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos10=ts(Datos10,freq=12,start=c(2001,1))
plot(Datos10)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Elaboracion de productos de panaderia macarrones fideos y sus productos-modif.csv", col 7: Índice de ventas nominales
Datos11=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos11=ts(Datos11,freq=12,start=c(2001,1))
plot(Datos11)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Elaboracion de productos de panaderia macarrones fideos y sus productos-modif.csv", col 8: Índice de ventas reales
Datos12=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos12=ts(Datos12,freq=12,start=c(2001,1))
plot(Datos12)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Fabricacion de otros productos quimicos-modif.csv", col 5: Índice de producción nominal
Datos13=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos13=ts(Datos13,freq=12,start=c(2001,1))
plot(Datos13)


#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Fabricacion de otros productos quimicos-modif.csv", col 6: Índice de producción real
Datos14=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos14=ts(Datos14,freq=12,start=c(2001,1))
plot(Datos14)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Fabricacion de otros productos quimicos-modif.csv", col 7: Índice de ventas nominales
Datos15=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",6),"numeric",rep("NULL",4)))
Datos15=ts(Datos15,freq=12,start=c(2001,1))
plot(Datos15)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-Fabricacion de otros productos quimicos-modif.csv", col 8: Índice de ventas reales
Datos16=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",7),"numeric",rep("NULL",3)))
Datos16=ts(Datos16,freq=12,start=c(2001,1))
plot(Datos16)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de lacteos-modif.csv", col 5: Índice de producción nominal
Datos17=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",4),"numeric",rep("NULL",6)))
Datos17=ts(Datos17,freq=12,start=c(2001,1))
plot(Datos17)

#------------------------------------------------------------
rm(list=ls(all=TRUE))
#Para leer "anexos-emmet-noviembre-2021-1-elaboracion de lacteos-modif.csv", col 6: Índice de producción real
Datos18=read.table(file.choose(),header=T,sep=";",skip=15,dec=",",colClasses=c(rep("NULL",5),"numeric",rep("NULL",5)))
Datos18=ts(Datos18,freq=12,start=c(2001,1))
plot(Datos18)

