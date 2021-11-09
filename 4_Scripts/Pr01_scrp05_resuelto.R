##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 5
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 
### referencia
###
### 2. Lectura desde archivos CSV
### 3. Corrección de archivos
### 4. Marcas de tiempo
### 5. Cálculos
### 6. Escritura de archivos CSV
### 7. Archivos RDS
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

# Instalación y activación de librerías
# readr
library(readr)

# lubridate
library(lubridate)

##################
### Referencia
##################

# Cambio de formatos de datos
# En general, con las funciones "as.XXX()"
# ¡OJO, verificar que funciona bien!
# Da problemas con los valores categóricos. aparéntemente son texto, pero están indexados

# Calcular máximos
# max calcula el valor máximo (1 número) de un conjunto de datos
# pmax calcula el valor máximo de 2 vectores por pares. Retorna un vector.

##################
### 2. Lectura desde archivos CSV
### 2.1 Apertura de archivos en editor de texto
### 2.2 Importación de archivos (visual)
### 2.3 Importación de archivos (código)
### 2.4 Inspección de dataframe
##################

# 2.1 Apertura de archivos en editor de texto
# En el cuadro inferior-derecha
# Seleccionar la pestaña "Files".
#    Es un pequeño explorador de archivos
#    Debería estar inicializado en la ruta de la práctica
# Acceder al directorio 2_Data
# Clickar sobre "Data_Pr01.csv"
# Seleccionar "view file"
#    Se abrirá el archivo en la pestaña superior-izquierda
# Revisar tamaño del archivo
#    Debería tener 7974 filas (+1 vacía)
#    Si una fila del archivo es demasiado larga para mostrarse en la ventana
#       Se continúa en la siguiente fila, pero sólo se numera la primera
# Revisar el formato
#    Primera fila: cabecera
#    Resto de filas: datos
#    Separación por columnas: ";"
#    Decimales: "."

# 2.2 Importación de archivos (visual)
# Hay (al menos) dos formas
#     File>...
#     Con el explorador de archivos + readr (se explica al final)
#
# File>Import Dataset>From Text (base)
# Seleccionar el archivo ".../2_Data/Data_Pr01.csv" (idem que en el punto anterior)
# Se abre una ventana
# Elegir el formato de importación
#    ";" -> Semicolon
#    "." -> Period
# Import
# Observar lo que se ha escrito en la consola
#    Debería ser algo como esto:
#    Data_Pr01 <- read.csv("C:/.../PhD_Course_Practice_01_R/2_Data/Data_Pr01.csv", sep=";")
#    View(Data_Pr01)
# Observar el data frame importado
#    Se debería haber abierto en la ventana superior-izquierda. Como una pestaña adicional
#    En caso contrario, Seleccionar en la ventana superior-derecha, peastaña "Environment"
#
# Alternativamente, se puede importar un archivo CSV mediente readr
#    File>Import Dataset>From Text (readr)
#    también desde el explorador de archivos, clickando sobre el archivo y luego "Import Dataset"
# Es ligeramente más avanzado. Permite definir el tipo de datos en cada columna

# 2.3 Importación de archivos (código)
# A realizar por los alumnos
# 
# Aprovechar el código generado en el punto 2.2
#    Data_Pr01 <- read.csv("C:/.../PhD_Course_Practice_01_R/2_Data/Data_Pr01.csv", sep=";")
# Importar el archivo Data_Pr01_FORMAT2.csv
# Verificar el formato del archivo (puede tener alguna sorpresa)
# El dataframe de salida debería llamarse "df_formato2"
file<-"/2_Data/Data_Pr01_FORMAT2.csv"
ruta<-paste(wd,file, sep="")
df_formato2 <- read.csv(ruta)

# Importar el archivo Data_Pr01_FORMAT3.csv
# Verificar el formato del archivo (puede tener alguna sorpresa)
# El dataframe de salida debería llamarse "df_formato3"
# NOTA los decimales se establecen con: dec="XXX"
file<-"/2_Data/Data_Pr01_FORMAT3.csv"
ruta<-paste(wd,file, sep="")
df_formato3 <- read.csv(ruta, sep=" ",dec = ",")


# 2.4 Inspección de dataframe
# (df_formato3)
View(df_formato3)
summary(df_formato3)
head(df_formato3)
tail(df_formato3)
dim(df_formato3)
is.na(df_formato3)
summary(is.na(df_formato3))

##################
### 3. Corrección de archivos
### 3.1 Importación de datos con errores de formato
### 3.2 Inspección de tipos de dato
### 3.3 Corrección de tipos de dato
##################
# 3.1 Importación de datos con errores de formato
# Archivo de datos
file<-"/2_Data/Data_Pr01.csv"
# Ruta completa
ruta<-paste(wd,file, sep="")
# Lectura de un archivo csv
dataset <- read.csv(ruta, sep=";", dec=",")

# 3.2 Inspección de tipos de dato
# En general
summary(dataset)
head(dataset)

# trabajemos sobre la variable dataset$Power.kW.
summary(dataset$Power.kW.)
head(dataset$Power.kW.)

# 3.3 Corrección de tipos de dato
# dataset$Power.kW.
# A realizar por los alumnos
summary(as.numeric(dataset$Power.kW.))
head(as.numeric(dataset$Power.kW.))

dataset$Power.kW.<-as.numeric(dataset$Power.kW.)
summary(dataset)

# todas las variables numéricas [, 7:11]
# A realizar por los alumnos
names(dataset)
for (i in 7:11)
{
  dataset[,i]<-as.numeric(dataset[,i])
}

summary(dataset)
##################

##################
### 4. Marcas de tiempo
### 4.1 Revisión de marcas de tiempo en el archivo
### 4.2 Generación de variable POSIX
### 4.3 Operaciónes con fechas
##################
# 4.1 Revisión de marcas de tiempo en el archivo
summary(dataset)
plot(dataset$Year, type="l")
plot(dataset$Month, type="l")
plot(dataset$Day_Month, type="l")
plot(dataset$Hour_Day, type="l")

# 4.2 Generación de variable POSIX
# Generar una variable única con "forma" de marca de tiempo
dataset$time<-paste(dataset$Year, "-", dataset$Month, "-", dataset$Day_Month, " ", dataset$Hour_Day, ":00:00", sep="")

# de momento es sólo una variable de texto
summary (dataset$time)

# Convertir en marca de tiempo unificada
dataset$time<-as.POSIXct(dataset$time, format="%Y-%m-%d %H:%M:%OS")

summary (dataset$time)
plot(dataset$time)
dataset$time[3]-dataset$time[2]

# 4.3 Operaciónes con fechas
hour(dataset$time[1:24]) # Hour of the day
year(dataset$time[1:24])
month(dataset$time[1:24])
yday(dataset$time[1:48]) # day of the year

as.numeric(strftime(dataset$time[500],format="%W"))  # Week of year
wday(dataset$time[150]) # day of the week
##################

##################
### 5. Cálculos
### 5.1 Agregación de datos por día
### 5.2 Cálculo de GD
##################
# 5.1 Agregación de datos por día
# Generar variable índice (fecha sin horas)
# A realizar por los alumnos
# aprovechar el ejemplo de dataset$time más arriba
dataset$date<-paste(dataset$Year, "-", dataset$Month, "-", dataset$Day_Month, sep="")
dataset$date<-as.POSIXct(dataset$date, format="%Y-%m-%d")

# Agregar datos por variable índice
dataset.aggr<-aggregate(dataset, by=list(dataset$date), FUN=mean)

# Quedarnos sólo con las variables de interés: date, Temperature, Power.kW. 
TF<-names(dataset.aggr) %in% c("date","Temperature", "Power.kW.")
dataset.aggr<-dataset.aggr[,TF]

# Reordenar orden de columnas dataframe
dataset.aggr<-dataset.aggr[,c(3,2,1)]

# 5.2 Cálculo de GD
# HDD = valor positivo de 15-temperatura ambiental promedio diaria
dataset.aggr$difDD<-15-dataset.aggr$Temperature

plot(dataset.aggr$date, dataset.aggr$difDD, type="l")
abline(h = 0, col="red")

#Convertir en 0
# A realizar por los alumnos
max (dataset.aggr$difDD,0)
dataset.aggr$HDD15<-pmax(dataset.aggr$difDD, 0)

plot (dataset.aggr$date, dataset.aggr$difDD, type="l")
abline(h = 0, col="red")
lines(dataset.aggr$date, dataset.aggr$HDD15, col="blue")

# Quedarnos sólo con las variables de interés: date, Temperature, Power.kW. , HDD15
TF<-names(dataset.aggr) %in% c("date","Temperature", "Power.kW.", "HDD15")
dataset.aggr<-dataset.aggr[,TF]

##################
### 6. Escritura de archivos CSV
### 6.1 Escritura a archivo CSV
### 6.2 Inspección del archivo con editor de texto
### 6.3 Correcciones
##################
# 6.1 Escritura a archivo CSV
write.csv(dataset.aggr, file="dataset.aggr.csv")

# 6.2 Inspección del archivo con editor de texto
# A realizar por los alumnos
# Dónde se ha guardado??
getwd()

# Formato, separadores y decimales
# Formato, marcas de fila

# 6.3 Correcciones
# A realizar por los alumnos. Consultar el archivo de ayuda de write.csv()
# Quitar marca de fila
write.csv(dataset.aggr, file="dataset.aggr.csv", row.names=FALSE)

##################
### 7. Archivos RDS
##################
# Los archivos RDS son archivos binarios que contienen un objeto de R
# Escribir RDS a archivo
write_rds(dataset.aggr, file="dataset.aggr.RDS")

# Abrir RDS con editor de texto

# Importar a R
dataset.aggr_IMPORT <- readRDS("C:/GIT/PhD_Course/PhD_Course_Practice_01_R/dataset.aggr.RDS")

# A responder por los alumnos. ¿Ventajas e inconvenientes RDS vs CSV?

