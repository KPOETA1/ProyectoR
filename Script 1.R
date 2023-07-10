install.packages("easypackages")        # libreria easypackages usada para la verificación, instalación y carga de librerias.
library("easypackages") # Carga de la libreria easypackages

lib_req<-c("lubridate","dplyr","visdat","missMDA","mice","DMwR2","editrules", "corrplot", "readxl", "ggplot2", "scales", "ggpubr")# Listado de librerias requeridas por el script
easypackages::packages(lib_req)         # Verificación, instalación y carga de librerias.


Datos <- read_excel("paises.xls") # Lectura del archivo de datos en este caso un excel llamado paises.xls
View(Datos)


str(Datos)  # Verificación de la estructura de los datos
summary(Datos) # Resumen de los datos

# Reemplazo de los carateres que pueden generar conflicto a la hora de leer las variables dentro de la tabla
Datos$GRUPOS <- gsub("\\s",'_',Datos$GRUPOS)  # Reemplazo de los espacios en blanco por guiones bajos de la variable GRUPOS
Datos$GRUPOS <- gsub("-",'_',Datos$GRUPOS) # Reemplazo de los guiones por guiones bajos de la variable GRUPOS
Datos$País <- gsub("\\s", "_", Datos$País) # Reemplazo de los espacios en blanco por guiones bajos de la variable País


# Observacion de las etiquetas de los grupos para identificar las mal escritas
table (Datos$GRUPOS) # Tabla de frecuencias de la variable GRUPOS

# Corrección de las etiquetas de los grupos para mostrar los niveles correctos
level_GRUPOS <- c(africa="AFRICA", Africa="AFRICA", AFRICA="AFRICA", asia="ASIA", Asia="ASIA", ASIA="ASIA", EO_NA_JAPON_AUST_NZ="EO_NA_JAPON",
                  Europa_Oriental="EUROPA_ORIENTAL", EUROPA_ORIENTAL="EUROPA_ORIENTAL", iberoamerica="IBEROAMERICA", Iberoamerica="IBEROAMERICA",
                  IBEROAMERICA="IBEROAMERICA", Oriente_Medio="ORIENTE_MEDIO")

# Actualizacion del factor de la variable GRUPOS con los niveles correctos 
Datos <- transform(Datos,
                   GRUPOS=factor(dplyr::recode(GRUPOS, !!!level_GRUPOS)))

# Se vuelve a observar las etiquetas de los grupos para verificar que se hayan corregido
str(Datos) # Verificación de la estructura de los datos
summary(Datos) # Retificacion de los valores de las variables


# EN ESTE APARTADO SE REALIZA LA VALIDACION DE LAS REGLAS DE CONSISTENCIA DE LOS DATOS

# Carga del archivo de reglas de validación
Rules <- editrules::editfile("Consistencia.txt") 
Rules

# Visualización de las reglas de validación
# windows();plot(Rules) # significa que no hay violaciones de las reglas de validación

# Verificación de las reglas sobres los datos  Esta parte se puede omitir ya que no hay violaciones de las reglas de validación
# es mas por estética *-* 
editrules::violatedEdits(Rules, Datos)
Valid_Data = editrules::violatedEdits(Rules, Datos)
summary(Valid_Data)

# Visualización del diagnóstico
# windows();plot(Valid_Data)

# FIN DEL APARTADO DE VALIDACION DE LAS REGLAS DE CONSISTENCIA DE LOS DATOS QUE PUEDES OMITIR

#------------------------------------------------------------------------------------------------------------

# EN ESTE APARTADO SE ENCUENTRA LAS FUNCION QUE PERMITE IDENTIFICAR LOS DATOS FALTANTES EN LA BASE DE DATOS

# Identificion los datos NA o faltantes
# View(Datos) # Visualización de los datos
is.na(Datos) # Identificación de los datos NA o faltantes y los muestra como TRUE o FALSE

# Visualizacion de los datos
# x11();visdat::vis_miss(Datos) # Abre una ventana para visualizar los datos faltantes
#visdat::vis_miss(Datos) # Visualización de los datos faltantes en la base de datos en forma de gráfica de barras

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

# Summary.NA = miss(Datos) # Asignacion de la funcion a una lista
 
attach(Datos)  # Permite acceder a las variables de la base de datos sin necesidad de escribir el nombre de la base de datos

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
  Datos.cuant=Datos[,2:8] # Selecciona las variables cuantitativas
  AQ.cor = cor(Datos.cuant,method="pearson") # Calcula la correlación entre las variables cuantitativas
  print(AQ.cor) # Imprime la matriz de correlación
  windows(height=10,width=15) # Abre una ventana para visualizar la matriz de correlación
  corrplot::corrplot(AQ.cor, method = "ellipse",addCoef.col = "black",type="upper") # Visualiza la matriz de correlación
  windows(height=10,width=15) # Abre una ventana para visualizar la matriz de correlación
  pairs(Datos.cuant,lower.panel = panel.smooth, pch = 15) # Visualiza la matriz de correlación
}

# Visualización.
# Visualizar.AQ(Datos)  # Visualiza los datos AQ


## Imputación por regresion.
ImputR = mice::mice(Datos, maxit = 1,seed = 2018,print=F) # Imputación por regresión
Datos = mice::complete(ImputR) # Asigna los datos imputados a la base de datos original
# Visualizar.AQ(Datos) # Visualiza los datos AQ imputados

View(Datos) # DATOS COMPLETOS


#1. Visualizacion de Conformacion de la muestra
porcentaje = c(percent(sum(Datos$Grupo == 1) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 2) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 3) / nrow(Datos), accuracy = 0.01),
                percent(sum(Datos$Grupo == 4) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 5) / nrow(Datos), accuracy = 0.01),percent(sum(Datos$Grupo == 6) / nrow(Datos), accuracy = 0.01)) 
Reg = c("Europa Oriental", "Iberoamerica", "EO_NA_JAPON_AUSTR_NZ", "Oriente Medio", "Asia", "Africa")
conformacion.muestra = data.frame(Reg, porcentaje)

x11();ggplot(conformacion.muestra, aes(x="", y = porcentaje, fill = Reg)) +
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

# Asignar colores a los boxplots
gg1 <- ggplot(Datos, aes(x = GRUPOS, y = Tasa.natalidad)) +
  geom_boxplot(fill = "red") +
  ggtitle(label = "Tasa de natalidad por grupos")

gg2 <- ggplot(Datos, aes(x = GRUPOS, y = Tasa.mortalidad)) +
  geom_boxplot(fill = "blue") +
  ggtitle(label = "Tasa de mortalidad por grupos")

gg3 <- ggplot(Datos, aes(x = GRUPOS, y = Mortalidad.infantil)) +
  geom_boxplot(fill = "gray") +
  ggtitle(label = "Mortalidad infantil por grupos")

gg4 <- ggplot(Datos, aes(x = GRUPOS, y = Esperanza.vida.hombre)) +
  geom_boxplot(fill = "green") +
  ggtitle(label = "Esperanza de vida hombre por grupos")

gg5 <- ggplot(Datos, aes(x = GRUPOS, y = Esperanza.vida.mujer)) +
  geom_boxplot(fill = "yellow") +
  ggtitle(label = "Esperanza de vida mujer por grupos")

gg6 <- ggplot(Datos, aes(x = GRUPOS, y = PNB)) +
  geom_boxplot(fill = "brown") +
  ggtitle(label = "PNB por grupos")

# Mostrar los boxplots con colores
x11(); ggarrange(gg1, gg2, gg3, gg4, gg5, gg6, ncol = 3, nrow = 3)



#3. Adicion de la columna PNB per capita

Datos <- dplyr::mutate(Datos,
                       PNB.per.capita = round(PNB * 1000000000/(Población..miles.* 1000)))


# Visualizacion de los datos PNB per capita por pais agrupado

x11(); ggplot(Datos, aes(x = factor(Grupo), y = PNB.per.capita, fill = País)) +
  geom_col(position = "Dodge") +
  scale_x_discrete(labels = paste("Grupo", unique(Datos$GRUPOS))) +
  scale_y_continuous(breaks = seq(0, 14000000, 500000)) +
  labs(x = "Grupo", y = "PNB per capita", fill = "País (PNB per capita)") +
  theme(legend.text = element_text(margin = margin(t = 10, b = 10))) +
  theme_minimal()

#Muestra la nueva tabla
# View(Datos)

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

pl <- ggplot(Frecuencias.PNB.clas, aes(Var1, Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  labs(x = "Nivel de pobreza",
       y = "Frecuencia absoluta",
       title = "Visualizacion de la tabla de frecuencias") +
  scale_y_continuous(breaks = c(1:30)) +
  scale_fill_discrete(guide = FALSE);x11(); pl

############### AREA DE HIPOTESIS #####################

# Copiar la tabla de datos H1
Datos_1 <- Datos
# hipotesis 1: El PNB per capita promedio es diferente para los paises en diferentes regiones
# Realizar el test de Kruskal-Wallis para comparar el PNB per cápita entre diferentes regiones
# H0: El PNB per cápita es igual para todas las regiones
# H1: El PNB per cápita es diferente para al menos una región
resultado_kruskal <- kruskal.test(PNB.per.capita ~ GRUPOS, data = Datos_1)

# Imprimir los resultados del test de Kruskal-Wallis
print(resultado_kruskal)

# Graficar los resultados del test de Kruskal-Wallis en un gráfico de cajas y bigotes
boxplot(PNB.per.capita ~ GRUPOS, data = Datos_1,
        main = "Comparación del PNB per cápita entre regiones",
        xlab = "Regiones",
        ylab = "PNB per cápita")

# Crear una copia de la tabla de datos H2
Datos_2 <- Datos

# Hipothesis 2: La tasa de mortalidad es diferente para los paises con diferentes niveles de PNB per capita
# Recodificar PNB.Capita.Clasificado para que tenga solo dos niveles: BAJO y ALTO
# H0: La tasa de mortalidad es igual para los paises con diferentes niveles de PNB per capita
# H1: La tasa de mortalidad es diferente para los paises con diferentes niveles de PNB per capita
Datos_2$PNB.Capita.Clasificado <- factor(Datos_2$PNB.Capita.Clasificado, levels = c("BAJO", "ALTO"))
t.test(Mortalidad.infantil ~ PNB.Capita.Clasificado, data = Datos_2)

# Crear el gráfico de barras
ggplot(Datos_2, aes(x = PNB.Capita.Clasificado, y = Mortalidad.infantil, fill = PNB.Capita.Clasificado)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "PNB por Cápita Clasificado", y = "Mortalidad Infantil Media") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()


# OTRA PRUEBA: MORTALIDAD

datos_3 <- Datos %>% group_by(GRUPOS) %>% summarise(Tasa.natalidad = mean(Tasa.natalidad), Tasa.mortalidad = mean(Tasa.mortalidad), PNB.per.capita = mean(PNB.per.capita),Mortalidad.infantil = mean(Mortalidad.infantil), Esperanza.hombre = mean(Esperanza.vida.hombre), Esperanza.mujer = mean(Esperanza.vida.mujer), Poblacion = mean(Población..miles.))
show(datos_3)
datos_4 <- Datos
# Grupo pais con mayor tasa de mortalidad
mortalidad_max <- datos_3 %>% filter(Tasa.mortalidad == max(Tasa.mortalidad))
show(mortalidad_max)

# Grupo pais con menor tasa de mortalidad
mortalidad_min <- datos_3 %>% filter(Tasa.mortalidad == min(Tasa.mortalidad))
show(mortalidad_min)

# Hipotesis diferencia entre la tasa de mortalidad entre los paises con mayor y menor tasa de mortalidad
# H0: no hay una diferencia significativa entre los paises con mayor y menor tasa de mortalidad
# H1: hay una diferencia significativa entre los paises con mayor y menor tasa de mortalidad

# Toma la media de la tasa de mortalidad del grupo de pais AFRICA y la compara con la media de la tasa de mortalidad del grupo de pais ORIENTE_MEDIO EN un test de hipotesis
t.test(datos_4$Tasa.mortalidad[datos_4$GRUPOS == "AFRICA"], datos_4$Tasa.mortalidad[datos_4$GRUPOS == "ORIENTE_MEDIO"], two.sided = TRUE, conf.level = 0.95)

# OTRA PRUEBA:  NATALIDAD

datos_5 <- Datos %>% group_by(GRUPOS) %>% summarise(Tasa.natalidad = mean(Tasa.natalidad), Tasa.mortalidad = mean(Tasa.mortalidad), PNB.per.capita = mean(PNB.per.capita),Mortalidad.infantil = mean(Mortalidad.infantil), Esperanza.hombre = mean(Esperanza.vida.hombre), Esperanza.mujer = mean(Esperanza.vida.mujer), Poblacion = mean(Población..miles.))
show(datos_5)
datos_6 <- Datos
# Grupo pais con mayor tasa de natalidad
natalidad_max <- datos_5 %>% filter(Tasa.natalidad == max(Tasa.natalidad))
show(natalidad_max)

# Grupo pais con menor tasa de natalidad
natalidad_min <- datos_5 %>% filter(Tasa.natalidad == min(Tasa.natalidad))
show(natalidad_min)

# Hipotesis diferencia entre la tasa de natalidad entre los paises con mayor y menor tasa de natalidad
# H0: no hay una diferencia significativa entre los paises con mayor y menor tasa de natalidad
# H1: hay una diferencia significativa entre los paises con mayor y menor tasa de natalidad

# Toma la media de la tasa de natalidad del grupo de pais AFRICA y la compara con la media de la tasa de natalidad del grupo de pais EO_NA_JAPON_AUSTR_NZ EN un test de hipotesis
t.test(datos_6$Tasa.natalidad[datos_6$GRUPOS == "AFRICA"], datos_6$Tasa.natalidad[datos_6$GRUPOS == "EO_NA_JAPON_AUSTR_NZ"], two.sided = TRUE, conf.level = 0.95)

# TABLAS EXTRAS 
#Por grupo de pais mostrar la natalidad y mortalidad * pnb
# Gráfico de dispersión de Natalidad y PNB por grupo de país
ggplot(data = Datos, aes(x = Tasa.natalidad, y = PNB.per.capita, color = GRUPOS)) +
  geom_point() +
  labs(x = "Natalidad", y = "PNB per cápita", color = "Grupo de país") +
  theme_minimal() +
  ggtitle("Gráfico de dispersión de Natalidad y PNB por grupo de país")

# Gráfico de dispersión de Mortalidad y PNB por grupo de país
ggplot(data = Datos, aes(x = Tasa.mortalidad, y = PNB.per.capita, color = GRUPOS)) +
  geom_point() +
  labs(x = "Mortalidad", y = "PNB per cápita", color = "Grupo de país") +
  theme_minimal() +
  ggtitle("Gráfico de dispersión de Mortalidad y PNB por grupo de país")

# Grafico de barras apiladas de esperanza de vida hommbres por grupo de pais
ggplot(data = Datos, aes(x = GRUPOS)) +
  geom_bar(aes(y = Esperanza.vida.hombre, fill = "Hombres"), stat = "identity", width = 0.5) +
  labs(x = "Grupo de país", y = "Esperanza de vida", fill = "Género") +
  scale_fill_manual(values = c("Hombres" = "steelblue")) +
  theme_minimal() +
  ylim(0, 100)

# Grafico de barras apiladas de esperanza de vida mujeres por grupo de pais
ggplot(data = Datos, aes(x = GRUPOS)) +
  geom_bar(aes(y = Esperanza.vida.mujer, fill = "Mujeres"), stat = "identity", width = 0.5) +
  labs(x = "Grupo de país", y = "Esperanza de vida", fill = "Género") +
  scale_fill_manual(values = c("Mujeres" = "pink")) +
  theme_minimal() +
  ylim(0, 100)

################# PARA ESTOS GRAFICOS PRESIONAR EL BOTON DE ZOOM PARA UNA MEJOR PREV ##########################################

# Gráfico de dispersión de Tasa de Natalidad vs Tasa de Mortalidad
ggplot(data = Datos, aes(x = Tasa.natalidad, y = Tasa.mortalidad, color = País)) +
  geom_point() +
  labs(x = "Tasa de Natalidad", y = "Tasa de Mortalidad", color = "País") +
  theme_minimal()

# Gráfico de dispersión de Mortalidad Infantil vs Tasa de Mortalidad
ggplot(data = Datos_1, aes(x = Mortalidad.infantil, y = Tasa.mortalidad, color = País)) +
  geom_point() +
  labs(x = "Mortalidad Infantil", y = "Tasa de Mortalidad", color = "País") +
  theme_minimal()


# Gráfico de dispersión de Mortalidad Infantil vs Población en miles
ggplot(data = Datos_1, aes(x = Mortalidad.infantil, y = Población..miles., color = País)) +
  geom_point() +
  labs(x = "Mortalidad Infantil", y = "Población (miles)", color = "País") +
  theme_minimal()

# Calcular el promedio por grupos clasificados
promedios <- aggregate(PNB.per.capita ~ GRUPOS, data = Datos, FUN = mean)

# Gráfico de barras del promedio por grupos clasificados
ggplot(data = promedios, aes(x = GRUPOS, y = PNB.per.capita)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Grupos clasificados", y = "PNB per cápita promedio") +
  theme_minimal()

# Que pais tiene una mejor calidad de vida (tasa natalidad - mortalidad)
# Calcular la diferencia entre la tasa de natalidad y la tasa de mortalidad
Datos$CalidadVida <- Datos$Tasa.natalidad - Datos$Tasa.mortalidad

# Obtener el país con la mejor calidad de vida
mejorPais <- Datos$País[which.max(Datos$CalidadVida)]

# Imprimir el país con la mejor calidad de vida
print(paste("El país con mejor calidad de vida es:", mejorPais))

# Crear un gráfico de barras de la diferencia de calidad de vida por país
colores <- rainbow(length(Datos$CalidadVida))

# Crear el gráfico de barras con colores individuales
barplot(Datos$CalidadVida, xlab = "País", ylab = "Diferencia de calidad de vida",
        main = "Diferencia de calidad de vida por país", names.arg = rep("", length(Datos$CalidadVida)),
        col = colores, ylim = c(-5, 45)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + text(x = 1:length(Datos$CalidadVida), y = Datos$CalidadVida, labels = Datos_1$País, pos = 3, cex = 0.7)

# Grafica Esperanza.vida.hombre, Esperanza.vida.mujer vs Tasa.Mortalida filtrada por grupo de pais == AFRICA
datos_filtrados <- subset(Datos, GRUPOS == "AFRICA")

ggplot(data = datos_filtrados, aes(x = Tasa.mortalidad, y = Esperanza.vida.hombre, color = País, shape = "Hombres")) +
  geom_point() +
  labs(x = "Tasa de Mortalidad", y = "Esperanza de vida hombres y mujeres", color = "País", shape = "") +
  geom_point(aes(y = Esperanza.vida.mujer, shape = "Mujeres")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal()

# TASA DE MORTALIDAD POR GRUPO DE PAIS filtrada AFRICA vs PNB.per.capita
datos_filtrados <- subset(Datos, GRUPOS == "AFRICA")

ggplot(data = datos_filtrados, aes(x = PNB.per.capita, y = Tasa.mortalidad, color = País)) +
  geom_point() +
  labs(x = "PNB.per.capita", y = "Tasa de Mortalidad", color = "País") +
  theme_minimal()

# TASA DE Tasa.mortalidad VS POBLACION filtrada por grupo de pais == AFRICA
datos_filtrados <- subset(Datos, GRUPOS == "AFRICA")

ggplot(data = datos_filtrados, aes(x = Población..miles., y = Tasa.mortalidad, color = País)) +
  geom_point() +
  labs(x = "Población (miles)", y = "Tasa de Mortalidad", color = "País") +
  theme_minimal()

# Tasa.natalidad VS Tasa.mortalidad filtrada por grupo de pais == AFRICA
ggplot(data = datos_filtrados, aes(x = Tasa.natalidad, y = Tasa.mortalidad, color = País)) +
  geom_point() +
  labs(x = "Tasa de Natalidad", y = "Tasa de Mortalidad", color = "País") +
  theme_minimal()

# Tasa.natalidad mas alta filtrada por grupo de pais == AFRICA VS PNB.per.capita
ggplot(data = datos_filtrados, aes(x = PNB.per.capita, y = Tasa.natalidad, color = País)) +
  geom_point() +
  labs(x = "PNB.per.capita", y = "Tasa de Natalidad", color = "País") +
  theme_minimal()


# Tasa.natalidad mas alta filtrada por grupo de pais == AFRICA VS Población..miles.
ggplot(data = datos_filtrados, aes(x = Población..miles., y = Tasa.natalidad, color = País)) +
  geom_point() +
  labs(x = "Población (miles)", y = "Tasa de Natalidad", color = "País") +
  theme_minimal()


# Calcular el intervalo de confianza para PNB.per.capita
result <- t.test(datos_filtrados$PNB.per.capita)
confidence_interval <- result$conf.int

# Imprimir el intervalo de confianza
print(paste("Intervalo de confianza (95%) para PNB.per.capita:", confidence_interval))

# imprimir: "el intervaalo de confianza esta entre (valor1) y (valor2)" entre corchetes
print(paste("El intervalo de confianza esta entre [", confidence_interval[1], ",", confidence_interval[2], "]"))


#                                    FIN  

