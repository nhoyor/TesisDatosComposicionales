#Este script es para trabajar con los archivos de los contaminantes en .xls

##0.-LECTURA DE ARCHIVOS NECESARIOS PARA ARMAR LA BASE: dfDatos
  #0.0 Descargar archivos
  #0.1 Directorio,variables y viernes Seca Cálida
  #0.2 Loop i... 2002+i (Del año 2003-2016)
    #0.2.1 Loop j... para juntar columans ("CO","NO2","O3","SO2","PM10")
      #0.2.1.1 Loop k length(viernes del año i+2002)
  #0.3 Reemplazamos -99 en 2016 PED PM10, por (AJM+CUA)/2
  #0.4 Reemplazamos 0's
  #0.5 Reemplazamos valores Xt= -99 por Promedio [(Xt-1)+(Xt+1)]/2
  #0.6 Eliminamos datos de HORA=7 y HORA=16
  #0.7 Quitamos -99
  #0.8 Guardamos dfDatos
  #0.9 Borramos todas variables excepto dfDatos

#0.0 Descargar archivos
  #Lo primero que se necesita hacer es descargar las bases de la pagina de Monitoreo Atmosferico de la CDMX:
  #http://www.aire.cdmx.gob.mx/default.php?opc=%27aKBh%27
  #Seleccionen el año deseado y se descargará un archivo comprimido 
  #(para este trabajo requeiren bajar los archivos del 2003-2016)
  #Copien al directorio los archivos .xls de los contaminantes "CO","NO2","O3","SO2"y "PM10"
  
#0.1 Directorio,variables y viernes Seca Cálida
library(gdata) #Para leer archivos de excel
directorio="//w2k3pc01/OP_Portafolios/NHR/yo/personales/tesis/aire_CDMX/RAMA/"
#nom_fechas="viernes_Seca_Calida.csv" #archivo con las fechas de los viernes de la tempirada Seca Cálida de 2003-2016


#Creamos vector de fechas de los viernes de la temporada Seca Cálida de 2003-2016
viernes=read.csv(paste(directorio,nom_fechas,sep=""),sep=",", header=FALSE)
viernes=as.Date(viernes[,1],"%d/%m/%Y")
year=as.numeric(format(viernes,'%Y')) #Año de las fechas de viernes

dfDatos=data.frame(matrix(0,0,1))

contaminantes=c("CO","NO2","O3","SO2","PM10")
columnas=c("TMP",contaminantes)#Temperatura y contaminantes

#0.2 Abrimos archivos con contaminantes del 2003-2016
for (i in 1:14){
  dfDatos1=data.frame(matrix(0,0,1))
  posYear=year==(i+2002)
  viernes2=viernes[posYear]#los viernes del año correspondiente al loop i
  
  #0.2.1 Hacemos loop j para tener a cada variable en una columna diferente  
  for (j in 1:length(columnas)){
    nombre=paste(i+2002,columnas[j],".xls",sep="") #archivo de temperatura y contaminates del año i+2002
    datos=read.xls(paste(directorio,nombre,sep="")) #base de datos del año i+2002
    
    #Nos quedamos sólo con los viernes de las bases de datos y 8<=HORA<=15 (de 8-15 Hrs)
    a=as.Date(datos$FECHA,"%Y-%m-%d") #Columna de Fecha
    if (j==1){ #En el primer loop de j guardamos columna de FECHA, HORA y la de datos
      #0.2.1.1
      for (k in 1:length(viernes2)){
        b=(a==viernes2[k] & datos$HORA>=7 & datos$HORA<=16)
        datos2=datos[b,c("FECHA","HORA","PED")]
        names(datos2)[names(datos2)=="PED"]=columnas[j]
        dfDatos1=rbind(dfDatos1,datos2)
      }
    } else { #Para los demás loops de j guardamos solo columna de datos
        datos2=data.frame(matrix(0,0,1))
        for (k in 1:length(viernes2)){
          b=(a==viernes2[k] & datos$HORA>=7 & datos$HORA<=16)
          datos3=as.data.frame(datos[b,c("PED")])
          names(datos3)=columnas[j]
          datos2=rbind(datos2,datos3)
        }
        dfDatos1=cbind(dfDatos1,datos2)
    }
  }
  dfDatos=rbind(dfDatos,dfDatos1)
}

# 0.3 Reemplazamos -99 en 2016 PED PM10, por (AJM+CUA)/2
  # Las variable "datos","a","posYear" y "viernes2" ya traen base del 2016 porque fue el ultimo loop que se hizo
  #Si se necesitan cargar de nuevo, las siguientes 4 lineas son los comandos
    #nombre="2016PM10.xls"
    #datos=read.xls(paste(directorio,nombre,sep=""))
    #a=as.Date(datos$FECHA,"%Y-%m-%d") #Columna de Fecha
    #posYear=year==2016
    #viernes2=viernes[posYear]#los viernes del año 2016
  #Nos quedamos sólo con los viernes de las bases de datos y 8<=HORA<=15 (de 8-15 Hrs)
  AJM_CUA=matrix(0,0,1)
  for (k in 1:length(viernes2)){
    b=(a==viernes2[k] & datos$HORA>=7 & datos$HORA<=16)
    datos3=datos[b,c("AJM","CUA")]
    AJM_CUA=rbind(AJM_CUA,datos3)
  }
  AJM_CUA=as.data.frame(AJM_CUA)
  AJM_CUA.2neg=(AJM_CUA$AJM<0 & AJM_CUA$CUA<0) #los dos <0
  table(AJM_CUA.2neg)
  AJM_CUA.Aneg=(AJM_CUA$AJM<0 & AJM_CUA$CUA>=0) #AJM<0 y CUA>=0
  table(AJM_CUA.Aneg)
  AJM_CUA.Cneg=(AJM_CUA$AJM>=0 & AJM_CUA$CUA<0) #AJM>=0 y CUA<0
  table(AJM_CUA.Cneg)
  AJM_CUA.2pos=(AJM_CUA$AJM>=0 & AJM_CUA$CUA>=0) #los dos >=0
  table(AJM_CUA.2pos)
  
  PED=matrix(0,nrow(AJM_CUA),1)
  PED[AJM_CUA.2neg]=-99
  PED[AJM_CUA.Aneg]=AJM_CUA$CUA[AJM_CUA.Aneg]
  PED[AJM_CUA.Cneg]=AJM_CUA$AJM[AJM_CUA.Cneg]
  PED[AJM_CUA.2pos]=round((AJM_CUA$AJM[AJM_CUA.2pos] + AJM_CUA$AJM[AJM_CUA.2pos])/2,0)
  
  y2016=as.Date(dfDatos$FECHA,"%Y-%m-%d")
  y2016=as.numeric(format(y2016,'%Y'))
  y2016=(y2016==2016)
  dfDatos$PM10[y2016]=PED

  #Reemplazamos -99 en 2006,2007,2010 PED TMP, por estación PLA
  temp.years=c(2006,2007,2010)
  for(i in 1:3){
    nombre=paste(temp.years[i],"TMP.xls",sep="")
    datos=read.xls(paste(directorio,nombre,sep=""))
    a=as.Date(datos$FECHA,"%Y-%m-%d") #Columna de Fecha
    posYear=year==temp.years[i]
    viernes2=viernes[posYear]#los viernes del año temp.years[i]
      #Nos quedamos sólo con los viernes de las bases de datos y 8<=HORA<=15 (de 8-15 Hrs)
    PLA=data.frame(matrix(0,0,1))
    for (k in 1:length(viernes2)){
      b=(a==viernes2[k] & datos$HORA>=7 & datos$HORA<=16)
      datos3=as.data.frame(datos[b,c("PLA")])
      names(datos3)="TMP"
      PLA=rbind(PLA,datos3)
    }
    year.compare=as.Date(dfDatos$FECHA,"%Y-%m-%d")
    year.compare=as.numeric(format(year.compare,'%Y'))
    year.compare=(year.compare==temp.years[i])
    Ped.temp=dfDatos$TMP[year.compare]
    Ped.temp.pos=Ped.temp==-99
    Ped.temp[Ped.temp.pos]=PLA$TMP[Ped.temp.pos]
    dfDatos$TMP[year.compare]=Ped.temp
    #year.compare=(year.compare & dfDatos$TMP==-99)
    #Ped.temp=dfDatos$TMP[year.compare]
    #dfDatos$TMP[year.compare]=t(PLA)
    #dfDatos$TMP=as.numeric(dfDatos$TMP)
  }

  
#0.4 Reemplazamos 0's
#Al momento de realizar este trabajo solo CO, O3 y SO2 presentaban 0's
#Los 0's en CO <- .1, O3 <- 1 y en SO2 <- 1 
CO_0=which(dfDatos$CO==0, arr.ind=TRUE)
n.CO_0=length(CO_0)
n.CO_0
O3_0=which(dfDatos$O3==0, arr.ind=TRUE)
n.O3_0=length(O3_0)
n.O3_0
SO2_0=which(dfDatos$SO2==0, arr.ind=TRUE)
n.SO2_0=length(SO2_0)
n.SO2_0
dfDatos$CO[CO_0] <- .1
dfDatos$O3[O3_0] <- 1
dfDatos$SO2[SO2_0] <- 1
#ya no debe haber 0's
checa0=which(dfDatos==0, arr.ind=TRUE)
checa0

#0.5 Reemplazamos valores Xt= -99 por Promedio [(Xt-1)+(Xt+1)]/2
#Esta operaci'on se hace para los 5 contaminantes y temperatura
#OJO: unicamente se reemplaza Xi si (Xt-1) y (Xt+1) son distintos de -99 y
      # HORA distinta de 7 y de 16(pues son el 1er y ultimo dato del día)
n.obs=nrow(dfDatos)
obs99=data.frame(matrix(0,1,length(columnas))) #Para saber cuantas observaciones se estan reemplazando en cada contaminante
colnames(obs99)=columnas
for (j2 in 1:length(columnas)){
  auxh=(dfDatos$HORA>7 & dfDatos$HORA<16)
  auxCont=dfDatos[,columnas[j2]]==-99
  auxCont.t=auxCont[-c(1,n.obs)]
  auxCont.t_1= !auxCont[-c(n.obs-1,n.obs)]
  auxCont.t1= !auxCont[-c(1,2)]
  auxh=auxh[-c(1,n.obs)]
  aux=(auxh & auxCont.t & auxCont.t_1 & auxCont.t1)
  ind=c(FALSE,aux,FALSE)
  ind=which(ind)
  obs99[j2]=length(ind)#Observaciones que se estan reemplazando (para c/contaminante)
  #dfDatos$CO[ind] <- sapply(ind, function(i) with(dfDatos, round(mean(c(CO[i-1], CO[i+1])),1)))
  if (j2<=2 ){#si es TMP o CO
    dfDatos[ind,columnas[j2]] <- sapply(ind, function(i) round(mean(c(dfDatos[i-1,columnas[j2]], dfDatos[i+1,columnas[j2]])),1))
  }else{
    dfDatos[ind,columnas[j2]] <- sapply(ind, function(i) round(mean(c(dfDatos[i-1,columnas[j2]], dfDatos[i+1,columnas[j2]])),0))
  }
}
obs99

#Hay dos días con temperatura=-99 que se reemplazan 
#por el valor del día anterior(para no perder más datos)
dfDatos$TMP[133:135]=dfDatos$TMP[125:127]
dfDatos$TMP[1094:1096]=dfDatos$TMP[1086:1088]


#0.6 Eliminamos datos de HORA=7 y HORA=16
Hora.7_16=(dfDatos$HORA==7 | dfDatos$HORA==16)
Hora.7_16=which(Hora.7_16)
dfDatos=dfDatos[-Hora.7_16,]

#0.7 Quitamos -99
#Cuantos dias tenemos?
fechas.unicas=unique(as.Date(dfDatos$FECHA,"%Y-%m-%d"))
fechas.unicas=length(fechas.unicas)
fechas.unicas

#Quitamos renglones con valores -99 en alguna columna 
#(OJO: quitamos renglones de todo el día)
cuales.99=which(dfDatos==-99, arr.ind=TRUE)
cuales.99=unique(cuales.99[,1])
fechas.99=as.Date(dfDatos$FECHA[cuales.99],"%Y-%m-%d")
fechas.99=unique(fechas.99)
for (i2 in 1:length(fechas.99)){
 dias.99=which(as.Date(dfDatos$FECHA,"%Y-%m-%d")==fechas.99[i2])
 dfDatos=dfDatos[-dias.99,]
}

#0.8 Guardamos dfDatos 
#write.table(dfDatos, paste(directorio,"DataFrame_Tesis_HORAS.txt",sep=""), sep="\t",row.names=FALSE)  
write.csv(dfDatos, file = paste(directorio,"DataFrame_Tesis_viernes.csv",sep=""),row.names=FALSE)

#0.9 Borramos todas variables excepto dfDatos
rm(list=setdiff(ls(), c("directorio","dfDatos")))

#############De aqui podemos empezar a correr para no bajar todos los datos#######################################################################
##1.-HACEMOS CAMBIO DE UNIDADES, SEPARAMOS EN TRAIN Y TEST
  #1.1(Opcional)Descargamos base
  #1.2 Hacemos cambio de unidades (todo a microgramos por m3)
  #1.3 Límites Permisibles por contaminante
  #1.4 Separamos en Entrenamiento y Prueba

library(ggplot2)
library(compositions)
library(scales)#Para transparencia alpha en scatterplots
library(gtools)#Para hacer isodensidades de dirichlet
library(lmtest)

#1.1 (Opcional)Descargamos base

#OJO:SOLO ES NECESARIO CORRER ESTA SECCIÓN(1.1) SI NO CORRISTE EL SCRIPT DESDE LA SECCIÓN 0

directorio="C:/Users/Nico/Documents/ITAM/tesis/aire_CDMX/RAMA/RAMA/"
  #En ingles (separado con ;)
  #dfDatos=read.csv(paste(directorio,"DataFrame_Tesis_LIMPIA.csv",sep=""),sep=";", header=TRUE)
#directorio="//w2k3pc01/OP_Portafolios/NHR/yo/personales/tesis/aire_CDMX/RAMA/"
  #En español (separado con ,)
  dfDatos=read.csv(paste(directorio,"DataFrame_Tesis_viernes.csv",sep=""),sep=",", header=TRUE)
  
  
#Separamos Temperatura de la base de contaminantes
Temperatura=dfDatos$TMP
TemperaturaK=Temperatura+273
dfDatos=dfDatos[,-3]
dfDatos_orig=dfDatos#Orig es en las unidades originales

#1.2 Hacemos cambio de unidades (todo a microgramos por metro cúbico)
#Usando los supuestos:
  #Temperatura = 17° o 290K (temperatura promedio en la CDMX 2014)
  #R(Constante idel del gas)=62.4
  #Presión = 585 torr (CDMX)

#Se llega a Volumen = (R*TemperaturaK/Presion)
R=62.4
Presion=585
T_11=mean(Temperatura[dfDatos$HORA==11])+273
T_15=mean(Temperatura[dfDatos$HORA==15])+273
Volumen_15=R*T_15/Presion
#Masa Molar o Peso Molecular de los contaminantes
#en este orden ("CO","NO2","O3","SO2","PM10")
Masa.Molar=c(28.01,46.0055,48,64.066)
#Factor de cambio de unidades a microg/m3 
#(Recordar que CO está en PPM mientras los demás están en PPB)
#(PM10 ya está en microg/m3)
unidades=c(1000,1,1,1)
#coef.Cambio=unidades*Masa.Molar/Volumen
coef.Cambio=unidades*Masa.Molar*Presion/R
Mat.Cambio=matrix(coef.Cambio,nrow=nrow(dfDatos),ncol=length(coef.Cambio),byrow=TRUE)
#dfDatos[,3:7]=Mat.Cambio*dfDatos[,3:7]
dfDatos[,3:6]=Mat.Cambio*dfDatos[,3:6]/TemperaturaK



#1.3 Límites Permisibles por contaminante
#En unidades originales
limites=c(11,210,110,110,120)
#En microgramos por m3
limites_mg=c(limites[1:4]*unidades*Masa.Molar/Volumen_15,limites[5])

#suma de las columnas
suma=rowSums(dfDatos[,3:7])

#1.4 Separamos en Entrenamiento y Prueba
year.final=as.numeric(format(as.Date(dfDatos$FECHA,"%Y-%m-%d"),'%Y')) #Año de las fechas de viernes
year.2016=year.final==2016
dfDatos.train=dfDatos[!year.2016,]#contiene todas las horas de 8-15
dfDatos.train.orig=dfDatos_orig[!year.2016,]#contiene todas las horas de 8-15

dfDatos.test=dfDatos[year.2016,]#contiene todas las horas de 8-15
dfDatos.test.orig=dfDatos_orig[year.2016,]#contiene todas las horas de 8-15

dfDatos.train.11=dfDatos.train[dfDatos.train$HORA==11,]#contiene solo datos con HORA==11
dfDatos.train.15=dfDatos.train[dfDatos.train$HORA==15,]#contiene solo datos con HORA==15
dfDatos.test.15=dfDatos.test[dfDatos.test$HORA==15,]#contiene solo datos con HORA==15

dfDatos.train.15.orig=dfDatos.train.orig[dfDatos.train.orig$HORA==15,]#contiene solo datos con HORA==15
dfDatos.test.15.orig=dfDatos.test.orig[dfDatos.test.orig$HORA==15,]#contiene solo datos con HORA==15


##2.-ANÁLISIS EXPLORATORIO
  #2.1 Gráficos de dispersión
  #2.2 Rangos por años
  #2.3 Localizamos días malos y días buenos de ozono
  #2.4 Creamos Composiciones y estadísticos descriptivos (para HORA=15)
  #2.5 Hacemos subcomposición x1=[CO,NO2,O3]
    #2.5.1 Gráficos Ternarios a cada hora, señalando días buenos y días malos
    #2.5.2 Gráficos Ternarios a las 11 y 15 hrs, señalando días buenos y días malos
    #2.5.3 Gráficos Ternarios a las 11 y 15 hrs, coloreados por rango de años
  #2.6 Gráficos de dispersión con Log-cocientes log(CO/O3) y log(NO2/O3)

#2.1 Gráficos de dispersión
attach(dfDatos.train.15.orig)
par(mfrow=c(2,2))
plot(CO,ylim=c(0,limites[1]*1.1),xlab="Observaciones")
abline(limites[1],0,col="red")
plot(NO2,ylim=c(0,limites[2]*1.1),xlab="Observaciones")
abline(limites[2],0,col="red")
plot(O3,xlab="Observaciones")
abline(limites[3],0,col="red")
plot(PM10,xlab="Observaciones")
abline(limites[5],0,col="red")

par(mfrow=c(1,1))
plot(SO2,ylim=c(0,limites[4]*1.1),xlab="Observaciones")
abline(limites[4],0,col="red")
detach(dfDatos.train.15.orig)


#2.2 Rangos por años
#Hacemos 4 grupos por rango de años
year.train=year.final[year.final<2016]
year1=(year.train<=2006)
year2=(year.train>2006 & year.train<=2009)
year3=(year.train>2009 & year.train<=2012)
year4=(year.train>2012 & year.train<=2015)
dfDatos1=dfDatos.train[year1,]
dfDatos2=dfDatos.train[year2,]
dfDatos3=dfDatos.train[year3,]
dfDatos4=dfDatos.train[year4,]
#Para tener la leyenda de los Rangos por año
par(mfrow=c(1,1))
plot(dfDatos.train$CO,col="white",axes = FALSE, ann = FALSE)
legend("center",title="Rango de años",c("2003-2006","2007-2009","2010-2012","2013-2015"),fill=c("darkgreen","chocolate4","darkcyan","darkmagenta"))

#2.3 Localizamos días malos y días buenos de ozono
O3.MayorA=dfDatos.train$O3>=limites_mg[3]*1.625 #con1.65 son los peores 2 días, con 1.37 son 10 días, con 1.25 son 25 días,con 1 son 58 días
table(O3.MayorA)
O3.MayorA.fechas=as.Date(dfDatos.train$FECHA[O3.MayorA],"%Y-%m-%d")
O3.MayorA.fechas=unique(O3.MayorA.fechas)
O3.MayorA.indlogic=(is.element(as.Date(dfDatos.train$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train$HORA==8)
O3.MayorA.indlogic11=(is.element(as.Date(dfDatos.train$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train$HORA==11)
O3.MayorA.indlogic15=(is.element(as.Date(dfDatos.train$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train$HORA==15)
O3.MayorA.ind=which(O3.MayorA.indlogic)

#en la base original
O3.MayorA=dfDatos.train.orig$O3>=limites[3]*1.25 #con1.65 son los peores 2 días, con 1.37 son 10 días, con 1.25 son 25 días,con 1 son 58 días
table(O3.MayorA)
O3.MayorA.fechas=as.Date(dfDatos.train.orig$FECHA[O3.MayorA],"%Y-%m-%d")
O3.MayorA.fechas=unique(O3.MayorA.fechas)
O3.MayorA.indlogic=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train.orig$HORA==8)
O3.MayorA.indlogic11=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train.orig$HORA==11)
O3.MayorA.indlogic15=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.MayorA.fechas) & dfDatos.train.orig$HORA==15)
O3.MayorA.ind=which(O3.MayorA.indlogic)

O3.MayorA.test=dfDatos.test.15.orig$O3>=limites[3]

#Encontramos los máximos de Ozono
n=length(dfDatos.train$O3)
maxO3=c(0,0,0,0,0)
maxO3[1]=sort(dfDatos.train$O3,partial=n)[n]
maxO3[2]=sort(dfDatos.train$O3,partial=n-1)[n-1]
maxO3[3]=sort(dfDatos.train$O3,partial=n-2)[n-2]
maxO3[4]=sort(dfDatos.train$O3,partial=n-3)[n-3]
maxO3[5]=sort(dfDatos.train$O3,partial=n-4)[n-4]

maxO3.ind=c(0,0,0,0,0)
for (i in 1:5){
  maxO3.ind[i]=which(dfDatos.train$O3==maxO3[i])
}
maxO3.ind
maxO3.fechas=as.Date(dfDatos.train$FECHA[maxO3.ind],"%Y-%m-%d")
maxO3.fechas
maxO3.hora=dfDatos.train$HORA[maxO3.ind]
maxO3.hora
maxO3
#El máximo fue el 09-may-2003,renglones=33:40
#3er máximo (diferente día) fue el 05-may-2006,renglones=241:248
#4to máximo (diferente día) fue el 08-abr-2005,renglones=153:160

#Localizamos días "buenos"
#(Días donde no se pasa el umbral de 50ppm ~ 77.58 microg/m3 de O3 a las 15hrs)
O3.menor=dfDatos.train.orig$O3<=(limites[3]*.60) & dfDatos.train.orig$HORA==15 #con .23 son los 2 mejores dias, con .45 10 dias, con .60 22 días
table(O3.menor)
O3.menor.fechas=as.Date(dfDatos.train.orig$FECHA[O3.menor],"%Y-%m-%d")
O3.menor.fechas=unique(O3.menor.fechas)
O3.menor.indlogic=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.menor.fechas) & dfDatos.train.orig$HORA==8)
O3.menor.indlogic11=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.menor.fechas) & dfDatos.train.orig$HORA==11)
O3.menor.indlogic15=(is.element(as.Date(dfDatos.train.orig$FECHA,"%Y-%m-%d"),O3.menor.fechas) & dfDatos.train.orig$HORA==15)
O3.menor.ind=which(O3.menor.indlogic)

#Encontramos los máximos de PM10
# maxPM10=c(0,0,0,0,0)
# maxPM10[1]=sort(dfDatos.train$PM10,partial=n)[n]
# maxPM10[2]=sort(dfDatos.train$PM10,partial=n-1)[n-1]
# maxPM10[3]=sort(dfDatos.train$PM10,partial=n-2)[n-2]
# maxPM10[4]=sort(dfDatos.train$PM10,partial=n-3)[n-3]
# maxPM10[5]=sort(dfDatos.train$PM10,partial=n-4)[n-4]
# 
# maxPM10.ind=c(0,0,0,0,0)
# for (i in 1:5){
#   maxPM10.ind[i]=which(dfDatos.train$PM10==maxPM10[i])
# }
# maxPM10.ind
# maxPM10.fechas=as.Date(dfDatos.train$FECHA[maxPM10.ind],"%Y-%m-%d")
# maxPM10.fechas
# maxPM10.hora=dfDatos.train$HORA[maxPM10.ind]
# maxPM10.hora
# maxPM10
#El máximo fue el 05-abr-2013,renglones=745:752
#3er máximo (diferente día) fue el 29-abr-2005,renglones=177:184
#4to máximo (diferente día) fue el 26-abr-2013,renglones=769:776


#2.4 Creamos Composiciones y estadísticos descriptivos (para HORA=15)
dCDatos=acomp(dfDatos.train[,3:7])
dCDatos.15=acomp(dfDatos.train.15[,3:7])
dCDatos.11=acomp(dfDatos.train.11[,3:7])
summary(dCDatos.15)
boxplot(dCDatos.15)

#2.5 Hacemos subcomposición x1=[CO,NO2,O3]
idx1=c(3,4,5)
x1=dfDatos.train[,idx1]
x1=acomp(x1) #x1p=acomp(clo(dCDatos,parts=(idx1-2)))
x1.test=dfDatos.test[,idx1]
x1.test=acomp(x1.test)
x1.comp=dfDatos[,idx1]
x1.comp=acomp(x1.comp)

#2.5.1 Gráficos Ternarios a cada hora, señalando días buenos y días malos
  #Sin centrar
par(mfrow=c(3,3))
for(i in 0:7){
  plot(acomp(x1[dfDatos.train$HORA==i+8,]))
  #plot(acomp(x1[i+33,]),col=alpha("red",.3),pch=20,add=TRUE) #máximo
  #plot(acomp(x1[i+241,]),col=alpha("red",.3),pch=20,add=TRUE) #2do máximo
  #plot(acomp(x1[O3.MayorA.ind+i,]),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
  #plot(acomp(x1[O3.menor.ind+i,]),col=alpha("blue",.3),pch=20,add=TRUE) #3 mejores días
  plot(acomp(x1.test[dfDatos.test$HORA==i+8,]),pch=8,cex=0.8,col="blue",add=TRUE)
}
  #Datos centrados
par(mfrow=c(3,3))
for(i in 0:7){
  plot(acomp(x1[dfDatos.train$HORA==i+8,]),center=TRUE)
  #plot(acomp(x1[i+33,]),col=alpha("red",.3),pch=20,add=TRUE,center=TRUE) #máximo
  #plot(acomp(x1[i+241,]),col=alpha("red",.3),pch=20,add=TRUE,center=TRUE) #2do máximo
  #plot(acomp(x1[O3.MayorA.ind+i,]),col=alpha("red",.3),pch=20,add=TRUE,center=TRUE) #dias malos
  #plot(acomp(x1[O3.menor.ind+i,]),col=alpha("blue",.3),pch=20,add=TRUE,center=TRUE) #3 mejores días
  plot(acomp(x1.test[dfDatos.test$HORA==i+8,]),pch=8,cex=0.8,col="blue",add=TRUE)
}

#2.5.2 Gráficos Ternarios a las 11 y 15 hrs, señalando días buenos y días malos
#Centrados manualmente
par(mfrow=c(1,2))
cenx1.11=mean(acomp(x1[dfDatos.train$HORA==11,]))
cenx1.15=mean(acomp(x1[dfDatos.train$HORA==15,]))

cenx1.11=mean(acomp(x1.comp[dfDatos$HORA==11,]))
cenx1.15=mean(acomp(x1.comp[dfDatos$HORA==15,]))

  #11 hrs
plot(acomp(x1[dfDatos.train$HORA==11,]-cenx1.11),main="11 hrs")
plot(acomp(x1[O3.MayorA.indlogic11,]-cenx1.11),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic11,]-cenx1.11),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos
  #15 hrs
plot(acomp(x1[dfDatos.train$HORA==15,]-cenx1.15))
plot(acomp(x1[O3.MayorA.indlogic15,]-cenx1.15),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic15,]-cenx1.15),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos


#2.5.3 Gráficos Ternarios a las 11 y 15 hrs, coloreados por rango de años
  #11 hrs
plot(acomp(x1[dfDatos.train$HORA==11 & year1,]-cenx1.11),col="darkgreen")
plot(acomp(x1[dfDatos.train$HORA==11 & year2,]-cenx1.11),col="chocolate4",add=TRUE)
plot(acomp(x1[dfDatos.train$HORA==11 & year3,]-cenx1.11),col="darkcyan",add=TRUE)
plot(acomp(x1[dfDatos.train$HORA==11 & year4,]-cenx1.11),col="darkmagenta",add=TRUE)
plot(acomp(x1.test[dfDatos.test$HORA==11,]-cenx1.11),pch=8,cex=0.8,add=TRUE)
  #15 hrs
plot(acomp(x1[dfDatos.train$HORA==15 & year1,]-cenx1.15),col="darkgreen")
plot(acomp(x1[dfDatos.train$HORA==15 & year2,]-cenx1.15),col="chocolate4",add=TRUE)
plot(acomp(x1[dfDatos.train$HORA==15 & year3,]-cenx1.15),col="darkcyan",add=TRUE)
plot(acomp(x1[dfDatos.train$HORA==15 & year4,]-cenx1.15),col="darkmagenta",add=TRUE)
plot(acomp(x1.test[dfDatos.test$HORA==15,]-cenx1.15),pch=8,cex=0.8,add=TRUE)
#Gr'aficas para cada rango de año, señalando días buenos y días malos
par(mfrow=c(2,2))
#2003-2006
  #11 hrs
plot(acomp(x1[dfDatos.train$HORA==11 & year1,]-cenx1.11),col="darkgreen")
plot(acomp(x1[O3.MayorA.indlogic11 & year1,]-cenx1.11),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic11 & year1,]-cenx1.11),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos
  #15 hrs
plot(acomp(x1[dfDatos.train$HORA==15 & year1,]-cenx1.15),col="darkgreen")
plot(acomp(x1[O3.MayorA.indlogic15 & year1,]-cenx1.15),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic15 & year1,]-cenx1.15),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos

#2007-2009
#11 hrs
plot(acomp(x1[dfDatos.train$HORA==11 & year2,]-cenx1.11),col="chocolate4")
plot(acomp(x1[O3.MayorA.indlogic11 & year2,]-cenx1.11),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic11 & year2,]-cenx1.11),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos
#15 hrs
plot(acomp(x1[dfDatos.train$HORA==15 & year2,]-cenx1.15),col="chocolate4")
plot(acomp(x1[O3.MayorA.indlogic15 & year2,]-cenx1.15),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic15 & year2,]-cenx1.15),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos

#2010-2012
#11 hrs
plot(acomp(x1[dfDatos.train$HORA==11 & year3,]-cenx1.11),col="darkcyan")
plot(acomp(x1[O3.MayorA.indlogic11 & year3,]-cenx1.11),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic11 & year3,]-cenx1.11),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos
#15 hrs
plot(acomp(x1[dfDatos.train$HORA==15 & year3,]-cenx1.15),col="darkcyan")
plot(acomp(x1[O3.MayorA.indlogic15 & year3,]-cenx1.15),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic15 & year3,]-cenx1.15),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos

#2013-2015
#11 hrs
plot(acomp(x1[dfDatos.train$HORA==11 & year4,]-cenx1.11),col="darkmagenta")
plot(acomp(x1[O3.MayorA.indlogic11 & year4,]-cenx1.11),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic11 & year4,]-cenx1.11),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos
#15 hrs
plot(acomp(x1[dfDatos.train$HORA==15 & year4,]-cenx1.15),col="darkmagenta")
plot(acomp(x1[O3.MayorA.indlogic15 & year4,]-cenx1.15),col=alpha("red",.3),pch=20,add=TRUE) #dias malos
plot(acomp(x1[O3.menor.indlogic15 & year4,]-cenx1.15),col=alpha("blue",.3),pch=20,add=TRUE) #dias buenos


# #Otras 9 subcomposiciones
# idx2=c(3,4,6)
# x2=dfDatos.train[,idx2]
# x2=acomp(x2)
# 
# idx3=c(3,4,7)
# x3=dfDatos.train[,idx3]
# x3=acomp(x3)
# 
# idx4=c(3,5,6)
# x4=dfDatos.train[,idx4]
# x4=acomp(x4)
# 
idx5=c(3,5,7)
x5=dfDatos.train[dfDatos.train$HORA==15,idx5]
x5=acomp(x5)
# 
# idx6=c(3,6,7)
# x6=dfDatos.train[,idx6]
# x6=acomp(x6)
# 
# idx7=c(4,5,6)
# x7=dfDatos.train[,idx7]
# x7=acomp(x7)
# 
# idx8=c(4,5,7)
# x8=dfDatos.train[,idx8]
# x8=acomp(x8)
# 
# idx9=c(4,6,7)
# x9=dfDatos.train[,idx9]
# x9=acomp(x9)
# 
# idx10=c(5,6,7)
# x10=dfDatos.train[,idx10]
# x10=acomp(x10)

# #2.5 Diagramas ternarios 
# #2.5.1 Diagramas ternarios de las 10 subcomposiciones de 3 partes
# par(mfrow=c(3,2))
# plot(x1)
# plot(x2)
# plot(x3)
# plot(x4)
# plot(x5)
# plot(x6)
# plot(x7)
# plot(x8)
# plot(x9)
# plot(x10)
# 
# #2.5.2 Diagramas Ternarios con colores por Rango 
# #(De las subcomposiciones que tienen O3 y PM10...5,8 y 10)
# x5.1=dfDatos1[,idx5]
# x5.1=acomp(x5.1)
# x5.2=dfDatos2[,idx5]
# x5.2=acomp(x5.2)
# x5.3=dfDatos3[,idx5]
# x5.3=acomp(x5.3)
# x5.4=dfDatos4[,idx5]
# x5.4=acomp(x5.4)
# x5.O3Mayor=dfDatos.train[O3.MayorA,idx5]
# x5.O3Mayor=acomp(x5.O3Mayor)
# x5.PM10Mayor=dfDatos.train[PM10.MayorA,idx5]
# x5.PM10Mayor=acomp(x5.PM10Mayor)
# #x5.O3yPM10Mayor=dfDatos.train[O3yPM10.MayorA,idx5]
# #x5.O3yPM10Mayor=acomp(x5.O3yPM10Mayor)
# 
# x8.1=dfDatos1[,idx8]
# x8.1=acomp(x8.1)
# x8.2=dfDatos2[,idx8]
# x8.2=acomp(x8.2)
# x8.3=dfDatos3[,idx8]
# x8.3=acomp(x8.3)
# x8.4=dfDatos4[,idx8]
# x8.4=acomp(x8.4)
# x8.O3Mayor=dfDatos.train[O3.MayorA,idx8]
# x8.O3Mayor=acomp(x8.O3Mayor)
# x8.PM10Mayor=dfDatos.train[PM10.MayorA,idx8]
# x8.PM10Mayor=acomp(x8.PM10Mayor)
# #x8.O3yPM10Mayor=dfDatos.train[O3yPM10.MayorA,idx8]
# #x8.O3yPM10Mayor=acomp(x8.O3yPM10Mayor)
# 
# x10.1=dfDatos1[,idx10]
# x10.1=acomp(x10.1)
# x10.2=dfDatos2[,idx10]
# x10.2=acomp(x10.2)
# x10.3=dfDatos3[,idx10]
# x10.3=acomp(x10.3)
# x10.4=dfDatos4[,idx10]
# x10.4=acomp(x10.4)
# x10.O3Mayor=dfDatos.train[O3.MayorA,idx10]
# x10.O3Mayor=acomp(x10.O3Mayor)
# x10.PM10Mayor=dfDatos.train[PM10.MayorA,idx10]
# x10.PM10Mayor=acomp(x10.PM10Mayor)
# #x10.O3yPM10Mayor=dfDatos.train[O3yPM10.MayorA,idx10]
# #x10.O3yPM10Mayor=acomp(x10.O3yPM10Mayor)
# 
# 
# #2.5.3 Ternario:Composición original vs. Centrada
# par(mfrow=c(2,1))
# plot(x5)
# #legend("center",title="Rango de años",c("1995-2000","2001-2005","2006-2010","2011-2015"),fill=c("darkgreen","chocolate4","darkcyan","darkmagenta"))
# plot(x5.1,col="darkgreen",add=TRUE)
# plot(x5.2,col="chocolate4",add=TRUE)
# plot(x5.3,col="darkcyan",add=TRUE)
# plot(x5.4,col="darkmagenta",add=TRUE)
# plot(x5.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE)
# plot(x5.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE)
# 
# plot(x5,center=TRUE)
# plot(x5.1,col="darkgreen",add=TRUE,center=TRUE)
# plot(x5.2,col="chocolate4",add=TRUE,center=TRUE)
# plot(x5.3,col="darkcyan",add=TRUE,center=TRUE)
# plot(x5.4,col="darkmagenta",add=TRUE,center=TRUE)
# plot(x5.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE,center=TRUE)
# plot(x5.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE,center=TRUE)
# 
# plot(x8)
# plot(x8.1,col="darkgreen",add=TRUE)
# plot(x8.2,col="chocolate4",add=TRUE)
# plot(x8.3,col="darkcyan",add=TRUE)
# plot(x8.4,col="darkmagenta",add=TRUE)
# plot(x8.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE)
# plot(x8.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE)
# 
# plot(x8,center=TRUE)
# plot(x8.1,col="darkgreen",add=TRUE,center=TRUE)
# plot(x8.2,col="chocolate4",add=TRUE,center=TRUE)
# plot(x8.3,col="darkcyan",add=TRUE,center=TRUE)
# plot(x8.4,col="darkmagenta",add=TRUE,center=TRUE)
# plot(x8.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE,center=TRUE)
# plot(x8.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE,center=TRUE)
# 
# plot(x10)
# plot(x10.1,col="darkgreen",add=TRUE)
# plot(x10.2,col="chocolate4",add=TRUE)
# plot(x10.3,col="darkcyan",add=TRUE)
# plot(x10.4,col="darkmagenta",add=TRUE)
# plot(x10.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE)
# plot(x10.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE)
# 
# plot(x10,center=TRUE)
# plot(x10.1,col="darkgreen",add=TRUE,center=TRUE)
# plot(x10.2,col="chocolate4",add=TRUE,center=TRUE)
# plot(x10.3,col="darkcyan",add=TRUE,center=TRUE)
# plot(x10.4,col="darkmagenta",add=TRUE,center=TRUE)
# plot(x10.O3Mayor,col=alpha("red",.3),pch=20,add=TRUE,center=TRUE)
# plot(x10.PM10Mayor,col=alpha("blue",.3),pch=20,add=TRUE,center=TRUE)

#2.6 Gráficos de dispersión con Log-cocientes log(CO/O3) y log(NO2/O3)
#vemos cómo se va moviendo el máximo
par(mfrow=c(3,3))
for (i in 0:7){
  plot(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[dfDatos.train$HORA==i+8,]),xlim=c(-3,3),ylim=c(-1,7))
  abline(lm(log(CO/O3)~log(NO2/O3),data=acomp(dfDatos.train[dfDatos.train$HORA==i+8,]))) 
  points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[i+33,]),col=alpha("red",.3),pch=20) #máximo
  points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[i+241,]),col=alpha("red",.3),pch=20) #2do máximo
  #points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[i+153,]),col=alpha("red",.3),pch=20) #3er máximo
  points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[O3.menor.ind+i,]),col=alpha("blue",.3),pch=20) #3 mejores días
}

#Vemos cómo se van moviendo los días donde se rebasa el límte
for (i in 0:7){
  plot(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[dfDatos.train$HORA==i+8,]),xlim=c(-3,3),ylim=c(-1,7))
  abline(lm(log(CO/O3)~log(NO2/O3),data=acomp(dfDatos.train[dfDatos.train$HORA==i+8,]))) 
  #points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[O3.MayorA.ind+i,]),col=alpha("red",.3),pch=20)#Dias malos
  #points(log(CO/O3)~log(NO2/O3),acomp(dfDatos.train[O3.menor.ind+i,]),col=alpha("blue",.3),pch=20)#Dias buenos
}


par(mfrow=c(2,2))
#Log cocientes coloreados por rango de años 11 y 15 hrs
  #11 hrs
plot(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==11 & year1,],xlim=c(-3,3),ylim=c(-1,7),col="darkgreen")
abline(lm(log(CO/O3)~log(NO2/O3),data=dfDatos.train[dfDatos.train$HORA==11,])) 
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==11 & year2,],col="chocolate4")
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==11 & year3,],col="darkcyan")
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==11 & year4,],col="darkmagenta")
  #15 hrs
plot(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==15 & year1,],xlim=c(-3,3),ylim=c(-1,7),col="darkgreen")
abline(lm(log(CO/O3)~log(NO2/O3),data=dfDatos.train[dfDatos.train$HORA==15,])) 
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==15 & year2,],col="chocolate4")
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==15 & year3,],col="darkcyan")
points(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==15 & year4,],col="darkmagenta")

#Log cocientes 11 y 15 hrs señalando dias buenos y malos
  #11 hrs
plot(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==11,],xlim=c(-3,3),ylim=c(-1,7), main="11:00 hrs")
abline(lm(log(CO/O3)~log(NO2/O3),data=dfDatos.train[dfDatos.train$HORA==11,]))
points(log(CO/O3)~log(NO2/O3),dfDatos.train[O3.MayorA.indlogic11,],col=alpha("red",.3),pch=20)#Dias malos
points(log(CO/O3)~log(NO2/O3),dfDatos.train[O3.menor.indlogic11,],col=alpha("blue",.3),pch=20)#Dias buenos
  #15 hrs
plot(log(CO/O3)~log(NO2/O3),dfDatos.train[dfDatos.train$HORA==15,],xlim=c(-3,3),ylim=c(-1,7),main="15:00 hrs")
abline(lm(log(CO/O3)~log(NO2/O3),data=dfDatos.train[dfDatos.train$HORA==15,]))
points(log(CO/O3)~log(NO2/O3),dfDatos.train[O3.MayorA.indlogic15,],col=alpha("red",.3),pch=20)#Dias malos
points(log(CO/O3)~log(NO2/O3),dfDatos.train[O3.menor.indlogic15,],col=alpha("blue",.3),pch=20)#Dias buenos



# #sO2-NO2-PM10
# for (i in 0:7){
#   plot(log(SO2/PM10)~log(NO2/PM10),acomp(dfDatos.train[dfDatos.train$HORA==i+8,]))
#   abline(lm(log(SO2/PM10)~log(NO2/PM10),data=acomp(dfDatos.train[dfDatos.train$HORA==i+8,]))) 
#   points(log(SO2/PM10)~log(NO2/PM10),acomp(dfDatos.train[i+745,]),col=alpha("red",.3),pch=20)
# }


#PENSAR CUALES GRÁFICOS QUEREMOS VER


##3.-AJUSTES DE MODELOS
  #3.0 Isodensidades(curvas de nivel) Normal 3 componentes x5(CO,O3,PM10)
  #3.0.2 Isodensidades(curvas de nivel) Dirichlet 3 componentes x5(CO,O3,PM10)
  #3.1 Ajuste Modelo Normal en el Simplex
    #3.1.1 Prueba General de Normalidad Composicional del paquete acomp
    #3.1.2 Prueba de Normalidad aplicado a ilr(X) del paquete energy
    #3.1.3 Prueba basada en DVS (Descomposición de Valores Singulares) pag 54 libro R
    #3.1.4 QQ plots para cada uno de los pares de log cocientes log(xj,xi)

#3.0 Isodensidades(curvas de nivel) Normal 3 componentes x5(CO,O3,PM10)

par(mfrow=c(1,2))
ilrX=ilr(x5)
mymn=mean(x5)
myvr=ilrvar2clr(var(ilrX))
plot(mymn,pch=".",labels=names(x5))
for(p in c(0.5,1:9,9.5)/10){
  r=sqrt(qchisq(p=p,df=2))
  ellipses(mymn,myvr,r,col="grey")
}
plot(x5,pch=19,cex=0.5,add=TRUE)


mymnC=acomp(c(1,1,1)) #Si lo quieres centrado
#myvr=ilrvar2clr(var(ilrX))
plot(mymn,pch=".",labels=names(x5))
for(p in c(0.5,1:9,9.5)/10){
  r=sqrt(qchisq(p=p,df=2))
  ellipses(mymnC,myvr,r,col="grey")
}
x5M=(x5-mymn) #Si lo quieres centrado
plot(x5M,pch=19,cex=0.5,add=TRUE)


#3.0.2 Isodensidades(curvas de nivel) Dirichlet 3 componentes x5(CO,O3,PM10)
par(mfrow=c(1,1))
myalpha=fitDirichlet(x5)$alpha
plot(acomp(myalpha),pch="",labels=names(x5))
aux=seq(from=0,to=1,by=0.01)
myx=expand.grid(x=aux,y=aux)
c60=cos(pi/3)
s60=sin(pi/3)
myfun=function(x){
  y=c(x[1]-x[2]*c60/s60,x[2]/s60)
  y=c(1-sum(y),y)
  dd=ifelse(any(y<0),NA,ddirichlet(y,alpha=myalpha))
  return(dd)
}
dx=apply(myx,1,myfun)
dim(dx)=c(101,101)
contour(dx,asp=1,add=TRUE,col="grey")
plot(x5,pch=19,cex=0.5,add=TRUE)
Amean=acomp(clo(myalpha))
Cmean=clrInv(digamma(myalpha))
plot(Amean,pch=19,cex=0.5,col="red",add=TRUE)
plot(Cmean,pch=19,cex=0.5,col="blue",add=TRUE)



#3.1 Ajuste Modelo Normal en el Simplex
#orden de los contaminantes ("CO","NO2","O3","SO2","PM10")
par(mfrow=c(1,1))

#3.1.1 Prueba General de Normalidad Composicional del paquete acomp
NormalGOF<-replicate(1000,acompNormalGOF.test(dCDatos,R=999)$p.value)
hist(NormalGOF) #La base no pasa la prueba General de Normalidad Composicional

#3.1.2 Prueba de Normalidad aplicado a ilr(X) del paquete energy
E.test<-replicate(100,mvnorm.etest(ilr(dCDatos),R=999)$p.value)
hist(E.test)  #La base no pasa la prueba General de Normalidad  energy

#3.1.3 Prueba basada en DVS (Descomposición de Valores Singulares) pag 54 libro R
u=svd(scale(clr(dCDatos),center=TRUE,scale=TRUE))$u
#Prueba de normalidad Shapiro-Wilk
miprueba=function(...)shapiro.test(...)$p.value
apply(u,2,miprueba) #Los componentes que no pasan la prueba son O3 y SO2
#Prueba de ángulos
miprueba2=function(i,j){
  ks.test(atan2(u[,i],u[,j]),y="punif",min=-pi,max=pi)$p.value
}
autoload("combn",package ="combinat")
(paresAComparar=combn(1:5,2))
sapply(paresAComparar,miprueba2) #Pos creo que ningun par pasa la prueba
#Prueba del radio
radio=sqrt(rowSums(u^2))
ks.test(radio,y="pchisq",df=4)$p.value #Tampoco se pasa esta prueba

#3.1.4 QQ plots para cada uno de los pares de log cocientes log(xj,xi)
qqnorm(dCDatos,alpha=0.05) #En los QQplots no salta ningún par a simple vista

#3.2 Ajuste Modelo Dirichlet
fitDirichlet(dCDatos) #Este modelo arroja el alpha
#acompDirichletGOF.test(dCDatos,R=999)

#Modelo lineal xon x1=log(CO/O3) a las 11 hrs,  y1=log(CO/O3) a las 15 hrs
                  #x2=log(NO2/O3) a las 11 hrs, y2=log(NO2/O3) a las 15 hrs
attach(dfDatos.train)
x1.log=log(CO[HORA==11]/O3[HORA==11])
y1.log=log(CO[HORA==15]/O3[HORA==15])

x2.log=log(NO2[HORA==11]/O3[HORA==11])
y2.log=log(NO2[HORA==15]/O3[HORA==15])

x3.log=log(CO[HORA==11]/NO2[HORA==11])
y3.log=log(CO[HORA==15]/NO2[HORA==15])
detach(dfDatos.train)

d_malos=(O3.MayorA.ind+7)/8
d_buenos=(O3.menor.ind+7)/8

par(mfrow=c(3,1))
lineal_y1=lm(y1.log~x1.log)
summary(lineal_y1)
plot(x1.log,y1.log,main="log(CO/O3)",xlab="11 hrs",ylab="15 hrs")
abline(lineal_y1)
points(x1.log[d_malos],y1.log[d_malos],col=alpha("red",.3),pch=20)
points(x1.log[d_buenos],y1.log[d_buenos],col=alpha("blue",.3),pch=20)

lineal_y2=lm(y2.log~x2.log)
summary(lineal_y2)
plot(x2.log,y2.log,main="log(NO2/O3)",xlab="11 hrs",ylab="15 hrs")
abline(lineal_y2)
points(x2.log[d_malos],y2.log[d_malos],col=alpha("red",.3),pch=20)
points(x2.log[d_buenos],y2.log[d_buenos],col=alpha("blue",.3),pch=20)

lineal_y3=lm(y3.log~x3.log)
summary(lineal_y3)
plot(x3.log,y3.log,main="log(CO/NO2)",xlab="11 hrs",ylab="15 hrs")
abline(lineal_y3)
points(x3.log[d_malos],y3.log[d_malos],col=alpha("red",.3),pch=20)
points(x3.log[d_buenos],y3.log[d_buenos],col=alpha("blue",.3),pch=20)

# #Composición de los 2 mejores y 2 peores días a las 11
# par(mfrow=c(1,2))
# plot(acomp(dCDatos.11[c(64,91,5,31),1:3]))
# #Composición de los 2 mejores y 2 peores días a las 15
# plot(acomp(dCDatos.15[c(64,91,5,31),1:3]))

#Modelo lineal Sabado 15 hrs vs viernes 15 hrs
  #Para obtener los días a descargar
  # O3.MayorA=dfDatos_orig$O3>=limites[3]*1
  # O3.MayorA.fechas=as.Date(dfDatos_orig$FECHA[O3.MayorA],"%Y-%m-%d")
  # O3.MayorA.fechas=unique(O3.MayorA.fechas)
  # write.csv(O3.MayorA.fechas, file = paste(directorio,"fechas_ozono_viernes.csv",sep=""),row.names=FALSE)
dfDatos_s.orig=read.csv(paste(directorio,"DFSabado_Resumida_unidadesorig.csv",sep=""),sep=",", header=TRUE)
dfDatos_s.micro=read.csv(paste(directorio,"DFSabado_Resumida_micro.csv",sep=""),sep=",", header=TRUE) 

#Entrenamiento y prueba
year.final=as.numeric(format(as.Date(dfDatos_s.orig$FECHA,"%Y-%m-%d"),'%Y')) 
year.2016=year.final==2016
dfDatos_s.train.orig=dfDatos_s.orig[!year.2016,]
dfDatos_s.train.micro=dfDatos_s.micro[!year.2016,]
dfDatos_s.test.orig=dfDatos_s.orig[year.2016,]
dfDatos_s.test.micro=dfDatos_s.micro[year.2016,]

O3.MayorA_s=dfDatos_s.train.orig$O3>=limites[3]
table(O3.MayorA_s)
O3.MayorA_s.fechas=as.Date(dfDatos_s.train.orig$FECHA[O3.MayorA_s],"%Y-%m-%d")
O3.MayorA_s.indlogic=(is.element(as.Date(dfDatos_s.train.orig$FECHA,"%Y-%m-%d"),O3.MayorA_s.fechas))
O3.MayorA_s.ind=which(O3.MayorA_s.indlogic)

dfDatos_v.train.micro=dfDatos.train[O3.MayorA.indlogic15,]
dfDatos_v.test.micro=dfDatos.test.15[O3.MayorA.test,]
Temperatura.train=Temperatura[1:928]
Temperatura.test=Temperatura[929:length(Temperatura)]
Temperatura.test=Temperatura.test[dfDatos.test.orig$HORA==15]
temp_v.train=Temperatura.train[O3.MayorA.indlogic15]
temp_v.test=Temperatura.test[O3.MayorA.test]


y_CO_O3_s=log(dfDatos_s.train.micro$CO/dfDatos_s.train.micro$O3)
y_NO2_O3_s=log(dfDatos_s.train.micro$NO2/dfDatos_s.train.micro$O3)
y_SO2_O3_s=log(dfDatos_s.train.micro$SO2/dfDatos_s.train.micro$O3)
y_PM10_O3_s=log(dfDatos_s.train.micro$PM10/dfDatos_s.train.micro$O3)
x1_CO_O3_v=log(dfDatos_v.train.micro$CO/dfDatos_v.train.micro$O3)
x2_NO2_O3_v=log(dfDatos_v.train.micro$NO2/dfDatos_v.train.micro$O3)
x3_SO2_O3_v=log(dfDatos_v.train.micro$SO2/dfDatos_v.train.micro$O3)
x4_PM10_O3_v=log(dfDatos_v.train.micro$PM10/dfDatos_v.train.micro$O3)
x5_temp_v=temp_v.train
x6_logtemp_v=log(temp_v.train)

#Se realiza transformacion alr a los datos de viernes y sabado tomando O3 como denominador
#Se decidió tomar O3 como denominador por ser el contaminante que más contingencia provoca
#Y también para analizar a sus precursores (CO y NO2)
alry=alr(acomp(dfDatos_s.train.micro[,2:6]),ivar=3)
alrx=alr(acomp(dfDatos_v.train.micro[,3:7]),ivar=3)
lineal_ymult=lm(alry~alrx+x5_temp_v+x6_logtemp_v)
summary(lineal_ymult)

opar<-par(mar=c(4,4,0,0))
pairwisePlot(alrx,alry)
par(opar)

#y=log(CO/O3)
lineal_y_CO=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v+x3_SO2_O3_v+x4_PM10_O3_v+x5_temp_v+x6_logtemp_v)
summary(lineal_y_CO)
#Sacamos del modelo una por una a las variables con el valor p más alto
#hasta quedarnos con un modelo que tenga sólo variables significantes (al 0.05)
  #1.- Sacamos log(temperatura)
lineal_y_CO=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v+x3_SO2_O3_v+x4_PM10_O3_v+x5_temp_v)
summary(lineal_y_CO)
  #2.- Sacamos temperatura
lineal_y_CO=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v+x3_SO2_O3_v+x4_PM10_O3_v)
summary(lineal_y_CO)
  #3.- Sacamos SO2
lineal_y_CO=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v+x4_PM10_O3_v)
summary(lineal_y_CO)
  #4.- Sacamos PM10
lineal_y_CO=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v)
summary(lineal_y_CO)#Modelo final

#Para hacer un gráfico Ternario de los Residuales si Y es una composición
#(poniéndole curvas de nivel de una distribucion Normal en en Simplex)
alry=alr(acomp(dfDatos_s.train.micro[,2:4]),ivar=3)
alrx=alr(acomp(dfDatos_v.train.micro[,3:5]),ivar=3)
lineal_ymult=lm(alry~alrx)
summary(lineal_ymult)
Resid=alrInv(resid(lineal_ymult),orig=acomp(dfDatos_s.train.micro[,2:4]))
mean(Resid)#los residuales siempre están centrados
#3.0 Isodensidades(curvas de nivel) Normal 3 componentes x5(CO,O3,PM10)
opar<-par(mar=c(3,3,1,1))
mymnC=acomp(c(1,1,1)) #Si lo quieres centrado
myvr=ilrvar2clr(var(ilr(Resid)))
plot(mymnC,pch=".",labels=names(Resid))
for(p in c(0.5,1:9,9.5)/10){
  r=sqrt(qchisq(p=p,df=2))
  ellipses(mymnC,myvr,r,col="grey")
}
plot(Resid,pch=19,cex=0.5,add=TRUE)
par(opar)

#Gráficos de dispersión con sábados malos señalados
par(mfrow=c(1,1))
plot(x1_CO_O3_v,y_CO_O3_s)
abline(lm(y_CO_O3_s~x1_CO_O3_v))
points(x1_CO_O3_v[O3.MayorA_s.ind],y_CO_O3_s[O3.MayorA_s.ind],col=alpha("red",.3),pch=20)
points(x1_CO_O3_v[c(21,24,29)],y_CO_O3_s[c(21,24,29)],col=alpha("blue",.3),pch=20)

# plot(x2_NO2_O3_v,y_CO_O3_s)
# abline(lm(y_CO_O3_s~x2_NO2_O3_v))
# points(x2_NO2_O3_v[O3.MayorA_s.ind],y_CO_O3_s[O3.MayorA_s.ind],col=alpha("red",.3),pch=20)

plot(x5_temp_v,y_CO_O3_s)
#abline(lm(y_CO_O3_s~x5_temp_v))
points(x5_temp_v[O3.MayorA_s.ind],y_CO_O3_s[O3.MayorA_s.ind],col=alpha("red",.3),pch=20)

#Gráficos resumen de modelo lineal
par(mfrow=c(2,2))
plot(lineal_y_CO,add.smooth=FALSE,which=c(1:4))


#Revisamos los posibles datos atípicos
comp_meanX=mean(acomp(dfDatos_v.train.micro[-c(3,21,24,29),3:5]))
X_atip=rbind(acomp(dfDatos_v.train.micro[c(3,21,24,29),3:5]),comp_meanX)
comp_meanY=mean(acomp(dfDatos_s.train.micro[-c(3,21,24,29),2:4]))
Y_atip=rbind(acomp(dfDatos_s.train.micro[c(3,21,24,29),2:4]),comp_meanY)
Y_prom=mean(y_CO_O3_s[-c(3,21,24,29)])
Y_res_atip=c(y_CO_O3_s[c(3,21,24,29)],Y_prom)
(cbind(X_atip,Y_atip))
(cbind(c(x1_CO_O3_v[c(3,21,24,29)],log(comp_meanX[1]/comp_meanX[3])),c(x2_NO2_O3_v[c(3,21,24,29)],log(comp_meanX[2]/comp_meanX[3])),Y_res_atip))

#Se decidió que el 3 no es dato atípico, así que se crean variables indicadoras
#para los otros 3 puntos
ind_21=logical(length(x1_CO_O3_v))
ind_21[21]=TRUE
ind_24=logical(length(x1_CO_O3_v))
ind_24[24]=TRUE
ind_29=logical(length(x1_CO_O3_v))
ind_29[29]=TRUE

lineal_y_CO_atip=lm(y_CO_O3_s~x1_CO_O3_v+x2_NO2_O3_v+ind_21+ind_24+ind_29)
summary(lineal_y_CO_atip)#Modelo final con indicadoras en atípicos
par(mfrow=c(2,2))
plot(lineal_y_CO_atip,add.smooth=FALSE,which=c(1:4))
# #Preubas de autocorrelación
# acf(resid(lineal_y_CO_atip))
# bgtest(lineal_y_CO_atip)
# bgtest(lineal_y_CO_atip,order=8)

#Pruebas de Homoscedasticidad
bptest(lineal_y_CO_atip)

#Pruebas de Normalidad
hist(rstandard(lineal_y_CO_atip))
# library(tseries)
# jarque.bera.test(rstandard(lineal_y_CO_atip))

plot(y_CO_O3_s,predict(lineal_y_CO_atip))
abline(0,1)
alrx_new=cbind(unclass(alr(acomp(dfDatos_v.test.micro[,3:5]))),FALSE,FALSE,FALSE)
a = coef(lineal_y_CO_atip)[1]
b = coef(lineal_y_CO_atip)[-1]
y_predicted=a+alrx_new%*%b
y_true=log(dfDatos_s.test.micro$CO/dfDatos_s.test.micro$O3)
plot(y_true,y_predicted,xlim=c(0,2.5),ylim=c(0,2.5))
abline(0,1)
# #Para dibujar la regresión en el Simplex
# #(En estos datos no se aprecia muy bien, por eso no se usa)
# par(mfrow=c(1,1))
# X=acomp(dfDatos_v.train.micro[,3:5])
# (model=lm(y_CO_O3_s~alr(X)))
# (a = coef(model)[1])
# (b = alrInv(coef(model)[-1],orig=X))
# plot(X)
# straight(mean(X), b, lwd = 2, col = "black",lty=2)
# myY = pretty(y_CO_O3_s)
# refX = mean(X) + ((myY - a)/norm(b)^2) * b
# plot(refX, add = TRUE, pch = 19)
# orthoComp = function(x) { alrInv(alr(x) %*% matrix(c(0, 1, -1, 0), ncol = 2)) }
# mygreyscale = grey((0:nrow(refX))/nrow(refX))
# for (i in 1:nrow(refX)) {
#   straight(acomp(refX[i, ]), orthoComp(b), col = mygreyscale[i],lwd = 2)
# }


nico=cbind(y_CO_O3_s,x1_CO_O3_v,x2_NO2_O3_v)
par(mfrow=c(1,1))
pairs(nico)
cor(nico)

y_simple=lm(y_CO_O3_s~x1_CO_O3_v)
summary(y_simple)
par(mfrow=c(2,2))
plot(y_simple,add.smooth=FALSE,which=c(1:4))

y_simple=lm(y_CO_O3_s~x1_CO_O3_v+ind_21+ind_24+ind_29)
summary(y_simple)
par(mfrow=c(2,2))
plot(y_simple,add.smooth=FALSE,which=c(1:4))
# #Preubas de autocorrelación
# acf(resid(lineal_y_CO_atip))
# bgtest(lineal_y_CO_atip)
# bgtest(lineal_y_CO_atip,order=8)

#Pruebas de Homoscedasticidad
bptest(y_simple)

#Pruebas de Normalidad
par(mfrow=c(1,1))
hist(rstandard(y_simple),main="Histograma de residuales estandarizados",xlab="rstandard")
library(tseries)
jarque.bera.test(rstandard(y_simple))

#Gráficos de dispersión con sábados malos señalados
par(mfrow=c(1,1))
plot(x1_CO_O3_v,y_CO_O3_s)
abline(y_simple)
points(x1_CO_O3_v[O3.MayorA_s.ind],y_CO_O3_s[O3.MayorA_s.ind],col=alpha("red",.3),pch=20)
points(x1_CO_O3_v[c(21,24,29)],y_CO_O3_s[c(21,24,29)],col=alpha("blue",.3),pch=20)

plot(x5_temp_v,y_CO_O3_s)
points(x5_temp_v[O3.MayorA_s.ind],y_CO_O3_s[O3.MayorA_s.ind],col=alpha("red",.3),pch=20)



plot(y_CO_O3_s,predict(y_simple),ylab="y_predicted")
abline(0,1)
alrx_new=cbind((log(dfDatos_v.test.micro$CO/dfDatos_v.test.micro$O3)),FALSE,FALSE,FALSE)
a = coef(y_simple)[1]
b = coef(y_simple)[-1]
y_predicted=a+alrx_new%*%b
y_true=log(dfDatos_s.test.micro$CO/dfDatos_s.test.micro$O3)
points(y_true,y_predicted,pch=8,cex=0.8,col="blue")

