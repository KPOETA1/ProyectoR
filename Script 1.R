# Librerias requeridas

install.packages("easypackages")        # libreria easypackages usada para la verificación, instalación y carga de librerias.
library("easypackages")

lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot", "readxl", "ggplot2", "scales", "ggpubr")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.

# Lectura del excel la tabla de paises y carga a datos.

Datos <- read_excel("paises.xls")
View(Datos)


str(Datos)
summary(Datos)

# Reemplazo de los carateres que pueden generar conflicto a la hora de leer las variables dentro de la tabla
Datos$GRUPOS <- gsub("\\s",'_',Datos$GRUPOS) 
Datos$GRUPOS <- gsub("-",'_',Datos$GRUPOS) 
Datos$País <- gsub("\\s", "_", Datos$País)




# Observacion de las etiquetas de los grupos para identificar las mal escritas
table (Datos$GRUPOS)

# Declaracion de los niveles correctos para las variables
level_GRUPOS <- c(africa="AFRICA", Africa="AFRICA", AFRICA="AFRICA", asia="ASIA", Asia="ASIA", ASIA="ASIA", EO_NA_JAPON_AUST_NZ="EO_NA_JAPON",
                  Europa_Oriental="EUROPA_ORIENTAL", EUROPA_ORIENTAL="EUROPA_ORIENTAL", iberoamerica="IBEROAMERICA", Iberoamerica="IBEROAMERICA",
                  IBEROAMERICA="IBEROAMERICA", Oriente_Medio="ORIENTE_MEDIO")

# Modificación del formato y transformación de variables
Datos <- transform(Datos,
                   GRUPOS=factor(dplyr::recode(GRUPOS, !!!level_GRUPOS)))

str(Datos)
summary(Datos)


#----------------------------------------------------------------------#
####    Validación de reglas de consistencia en los datos - editrules   ####
#----------------------------------------------------------------------#


# Carga del archivo de reglas de validación
Rules <- editrules::editfile("Consistencia.txt")
Rules 

# Conexión entre las  reglas
windows();plot(Rules)

# Verificación de las reglas sobres los datos
editrules::violatedEdits(Rules, Datos)
Valid_Data = editrules::violatedEdits(Rules, Datos)
summary(Valid_Data)

# Visualización del diagnóstico
windows();plot(Valid_Data)


#------------------------------------------------------------------------#
#### Identificación y cuantificación de datos faltantes                  ####
#------------------------------------------------------------------------#

# Identificion los datos NA o faltantes
View(Datos)
is.na(Datos)

# Visualizacion de los datos
x11()
visdat::vis_miss(Datos) 

# Función que evalua e identifica los datos faltantes por variable e individuo.

miss<-function(Datos,plot=T){  
  n=nrow(Datos);p=ncol(Datos)
  names.obs<-rownames(Datos)
  
  
  nobs.comp=sum(complete.cases(Datos))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(Datos))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(Datos))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Datos))       # Identifica los registros con datos faltantes.
  
  Datos.NA<-is.na(Datos)
  Var_Num<- sort(colSums(Datos.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Datos.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}

Summary.NA = miss(Datos) # Asignacion de la funcion a una lista

attach(Datos)

Visualizar.AQ= function(Datos){   #Una función para visualizar los datos AQ
  with(Datos,{
    ## Tendencia por pais
    windows(height=10,width=15)
    par(mfrow=c(4,2))
    plot(Grupo,type="l",col="Red")
    plot(Tasa.natalidad,type="l",col="Blue")
    plot(Tasa.mortalidad,type="l",col="Gray")
    plot(Mortalidad.infantil,type="l",col="Black")
    plot(Esperanza.vida.hombre,type="l",col="Green")
    plot(Esperanza.vida.mujer,type="l",col="Yellow")
    plot(PNB,type="l",col="Brown")
    plot(Población..miles.,type="l",col="Purple")
    
    ## Variación por grupo
    windows(height=10,width=15)
    par(mfrow=c(4,2))
    plot(Tasa.natalidad~GRUPOS,type="l",col="Red")
    plot(Tasa.mortalidad~GRUPOS,type="l",col="Blue")
    plot(Mortalidad.infantil~GRUPOS,type="l",col="Gray")
    plot(Esperanza.vida.hombre~GRUPOS,type="l",col="Green")
    plot(Esperanza.vida.mujer~GRUPOS,type="l",col="Yellow")
    plot(PNB~GRUPOS,type="l",col="Brown")
    plot(Población..miles.~GRUPOS,type="l",col="Purple")
  })
  
  ## Correlación entre covariables cuantitativas
  Datos.cuant=Datos[,2:8]
  AQ.cor = cor(Datos.cuant,method="pearson")
  print(AQ.cor)
  windows(height=10,width=15)
  corrplot::corrplot(AQ.cor, method = "ellipse",addCoef.col = "black",type="upper")
  windows(height=10,width=15)
  pairs(Datos.cuant,lower.panel = panel.smooth, pch = 15)
}

## Visualización.
Visualizar.AQ(Datos)


## Imputación por regresion.
ImputR = mice::mice(Datos, maxit = 1,seed = 2018,print=F)
Datos = mice::complete(ImputR)
Visualizar.AQ(Datos)


#1. Visualizacion de Conformacion de la muestra
porcentaje = c(percent(sum(Datos$Grupo == 1) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 2) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 3) / nrow(Datos), accuracy = 0.01),
                percent(sum(Datos$Grupo == 4) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 5) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 6) / nrow(Datos), accuracy = 0.01)) 
Reg = c("Europa Oriental", "Iberoamerica", "EO_NA_JAPON_AUSTR_NZ", "Oriente Medio", "Asia", "Africa")
conformacion.muestra = data.frame(Reg, porcentaje)

x11()
ggplot(conformacion.muestra, aes(x="", y = porcentaje, fill = Reg)) +
  geom_col(color = c("Red", "Yellow", "Green", "Blue", "Brown", "Orange")) +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = porcentaje), 
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Conformacion de la muestra")+
  scale_fill_discrete(name = "Region")


#2. Visualizacion de diferencias de los indicadores por grupo

gg1 <- ggplot(Datos, aes(x= GRUPOS, y= Tasa.natalidad))+
  geom_boxplot()+
  ggtitle(label = "Tasa de natalidad por grupos")

gg2 <- ggplot(Datos, aes(x= GRUPOS, y= Tasa.mortalidad))+
  geom_boxplot()+
  ggtitle(label = "Tasa de mortalidad por grupos")

gg3 <- ggplot(Datos, aes(x= GRUPOS, y= Mortalidad.infantil))+ 
  geom_boxplot()+ 
  ggtitle(label = "Mortalidad infantil por grupos")

x11()
ggarrange(gg1, gg2, gg3, nrow = 3)


#3. Adicion de la columna PNB per capita

Datos <- dplyr::mutate(Datos,
                       PNB.per.capita = round(PNB * 1000000000/(Población..miles.* 1000)))


# Visualizacion de los datos PNB per capita por pais agrupado

x11()
ggplot(Datos, aes(x = Grupo, y = PNB.per.capita, fill = País))+
  geom_col(position = "Dodge")+
  scale_x_continuous(breaks = seq(1, 6, 1))+
  scale_y_continuous(breaks = seq(0, 14000000, 500000))+
  theme(legend.text = element_text(margin = margin(t= 10, b= 10)))+
  scale_fill_discrete(name = "País (PNB per capita)",labels = paste(as.character(Datos$País), " (", as.character(Datos$PNB.per.capita), ")", sep = ""))


#4. Adicion de la clasificacion del PNB per capita

Datos <- dplyr::mutate(Datos,
                       PNB.Capita.Clasificado = ifelse(PNB.per.capita <= quantile(PNB.per.capita, 0.25), "BAJO", 
                                                       ifelse(PNB.per.capita > quantile(PNB.per.capita, 0.25) & PNB.per.capita <= quantile(PNB.per.capita, 0.50), "MEDIO BAJO",
                                                              ifelse(PNB.per.capita > quantile(PNB.per.capita, 0.50)& PNB.per.capita <= quantile(PNB.per.capita, 0.75), "MEDIO ALTO",
                                                                     "ALTO"))))


# Calculo del porcentaje de paises por niveles de pobreza

tabla.pobreza <- Datos %>% count(PNB.Capita.Clasificado)

tabla.pobreza <- dplyr::mutate(tabla.pobreza,
                               porcentaje = percent(n/sum(n), accuracy = 0.01))


# Creacion de la tabla de frecuencias de los niveles de pobreza

Frecuencias.PNB.clas <- as.data.frame(table(Datos$PNB.Capita.Clasificado))

Frecuencias.PNB.clas <- transform(Frecuencias.PNB.clas,
                                  Frecuencia.acumulada = cumsum(Frecuencias.PNB.clas$Freq),
                                  Frecuencia.relativa = round(prop.table(Frecuencias.PNB.clas$Freq), 2),
                                  Frecuencia.relativa.acumulada = round(cumsum(prop.table(Frecuencias.PNB.clas$Freq)), 2))

# Visualizacion de la tabla de frecuencias

pl <- ggplot(Frecuencias.PNB.clas, aes(Var1, Freq)) + 
  geom_bar(stat = "identity") +
  labs(x= "Nivel de pobreza",
       y = "Frecuencia absoluta",
       title = "Visualizacion de la tabla de frecuencias")+
  scale_y_continuous(breaks = c(1:30))
x11()
pl