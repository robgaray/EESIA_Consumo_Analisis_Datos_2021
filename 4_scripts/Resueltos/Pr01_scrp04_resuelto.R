##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 4
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Apertura de 1 archivo CSV
### 
### referencia
###
### 3. Sumario de carga de un día de la semana
### 4. Sumario de carga de todos los días de la semana
###
##################

##################
### 0. Gestión de directorios
##################
# Gestión de directorios
# Directorio de trabajo
wd<-getwd()
# NOTA: Revísese que wd apunta al directorio en el que se ha dejado la información descargada
# por ejemplo, "C:/.../PhD_Course_Practice_01_R"
# en caso contrario, ejecutar la instancia de más abajo (reemplazando la ruta por la que corresponda)
# wd<-"C:/.../PhD_Course_Practice_01_R"

##################
### 1. Instalación y activación de librerías, carga de funciones
##################

# Carga de funciones
# Archivo de funciones
file<-"/4_Scripts/Pr01_fnct03.R"
# Ruta completa
ruta<-paste(wd,file, sep="")
# Carga de funciones
source(ruta)

##################
### 2. Apertura de 1 archivo CSV
##################

# Archivo de datos
file<-"/2_Data/Data_Pr01.csv"

# Ruta completa
ruta<-paste(wd,file, sep="")

# Lectura de un archivo csv
dataset <- read.csv(ruta, sep=";")
# View(dataset)

##################
### Referencia
##################

# Seleccionar una variable concreta de un dataframe: "$"
# sintaxis: DATAFRAME$VARIABLE
# ejemplo: dataset$Year
dataset$Year

# Seleccionar una posición concreta de un dada frame por número: [,]
# sintaxis: DATAFRAME[NUMERO_FILA,NUMERO_COLUMNA]
# ejemplo: dataset[3,5]
dataset[3,5]
# si se quiere toda la fila/columna, se puede dejar el valor vacío
# ejemplo: dataset[,5]
dataset[,5]

# guardar información en un objeto nuevo "<-"
# sintaxis: VAR_NUEVA <- VAR_VIEJA
# ejemplo: var<-dataset$Year
var<-dataset$Year
rm(var) # Eliminamos la variable para evitar problemas

# generar un vector inicializado a X
# sintaxis: NOMBRE_VECTOR<-rep (X,NUM_DIMENSIONES)
# ejemplo: vec<-rep (0,12)
vec<-rep (0,12)
rm(vec) # Eliminamos la variable para evitar problemas

# obtener valores únicos de una variable
# sintaxis: unique(VARIABLE)
# ejemplo: unique(dataset$Week_Day)
unique(dataset$Week_Day)

##################
### 3. Sumario de carga de un día de la semana
##################
# Coger un mes cualquiera (1:12)
month<-2
# Extraer los datos de un mes concreto
subset_month<-dataset[dataset$Month==month,]
# Seleccionar un día cualquiera 
day<- unique(dataset$Week_Day)[3]
# Extraer los datos de un día de la semana concreto
subset<-subset_month[subset_month$Week_Day==day,]

# Sacar sumario
summary<-summary(subset$Power.kW.)
min<-summary[1]
min<-as.numeric(min)
mean<-as.numeric(summary[4])
max<-as.numeric(summary[6])

##################
### 4. Sumario de carga de todos los días de la semana
##################
# EJERCICIO A REALIZAR POR LOS ALUMNOS
#
# Se ha generado una función que calcula un sumario de la carga térmica para un dia de la semana concreto.
# Con la siguiente forma:
#     sumario_carga_dia(DATOS,DIA_SEMANA)
#     La salida es un vector de 3 posiciones. MIN, MEAN y MAX
#
# Realizar los siguientes ejercicios
#
# 4.1 Sacar los valores minimo, promedio y máximo diarios para todos los días de la semana de un mes
# 4.1.1 Crear e inicializar a 0 los valores de salida
# 4.1.2 Recorrer (de forma automática) todos los días de la semana y guardar los valores en 3 vectores separados
# 4.1.3 Plotear los valores
#
# 4.2 Comparar los valores promedio correspondientes a todos los meses 
# 4.2.1 Crear e inicializar a 0 la matriz de salida
# 4.2.2 Recorrer (de forma automática) los meses seleccionaros para cada díade la semana y guardar los valores en 1 matriz
# 4.2.3 Convertir matriz a data frame
# 4.2.4 Plotear los valores

# 4.1 Sacar los valores minimo, promedio y máximo diarios para todos los días de la semana de un mes
# 4.1.1 Crear e inicializar a 0 los valores de salida
vect_MIN  <- rep(0, 7)
vect_MEAN <- rep(0, 7)
vect_MAX  <- rep(0, 7)

# 4.1.2 Recorrer (de forma auomática) todos los meses del año y guardar los valores en 3 vectores separados
month <-3
weekdays<- c("MON","TUE","WED","THU","FRI","SAT","SUN")

subset_month<-dataset[dataset$Month==month,]
for (i in 1:7)
{
  salida<-sumario_carga_dia(subset_month,weekdays[i])
  vect_MIN[i]  <- salida[1]
  vect_MEAN[i] <- salida[2]
  vect_MAX[i]  <- salida[3]
}

# 4.1.3 Plotear los valores
plot(vect_MAX, col="red",
     ylim=c(0,max(vect_MAX)),
     xlab="Dia", ylab="Carga térmica [kW]",
     main="Sumario de carga semanal",
     xaxt="n")
axis(1,at=1:7,labels=weekdays)
points (vect_MEAN, col="green")
points (vect_MIN, col="blue")

# 4.2 Comparar los valores promedio correspondientes a todos los meses 
# 4.2.1 Crear e inicializar a 0 la matriz de salida
# 1 columna para cada mes
# i fila para cada día de la semana
mat_MEAN<-matrix(0,nrow = 7, ncol =12)

# 4.2.2 Recorrer (de forma automática) los meses seleccionaros para cada díade la semana y guardar los valores en 1 matriz
months<-1:12
weekdays<- c("MON","TUE","WED","THU","FRI","SAT","SUN")


for (i in months)
{
  subset_month<-dataset[dataset$Month==i,]
  for (j in 1:length(weekdays))
  {
    salida<-sumario_carga_dia(subset_month,weekdays[j])
    mat_MEAN[j,i] <- salida[2]
  }  
}

# 4.2.3 Convertir matriz a data frame
df_MEAN<-as.data.frame(mat_MEAN)
names(df_MEAN)<-c("JAN","FEB","MAR","APR","MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

# 4.2.4 Plotear los valores
# coloreado automático
colores<-rainbow(12)

# gráfico vacío
plot(df_MEAN$JAN,type="n",
     ylim=c(0,max(df_MEAN)),
     xlab="Dia", ylab="Carga térmica [kW]",
     main="Sumario de carga semanal, por mes",
     xaxt="n")
axis(1,at=1:7,labels=weekdays)

# series de datos
for (i in months)
  {
    points (df_MEAN[,i], col=colores[i])
    lines (df_MEAN[,i], col=colores[i])
  }

# leyenda
legend("right",legend=names(df_MEAN),
       col=colores, lty=1, cex=0.8)



##################
# Un poco más limpio
##################
# (líneas con cambios marcadas com #$#)

colores<-rainbow(12)
par(mar=c(5.1, 4.1, 4.1, 7))                             #$#

# gráfico vacío
plot(df_MEAN$JAN,type="n",
     ylim=c(0,max(df_MEAN)),
     xlab="Dia", ylab="Carga térmica [kW]",
     main="Sumario de carga semanal, por mes",
     xaxt="n")
axis(1,at=1:7,labels=weekdays)

# series de datos
for (i in months)
{
  points (df_MEAN[,i], col=colores[i])
  lines (df_MEAN[,i], col=colores[i])
}

# leyenda
legend("topright",legend=names(df_MEAN),
       inset = c(-0.3, -0),                              #$#
       xpd = TRUE,                                       #$#
       col=colores, lty=1, cex=0.8)
