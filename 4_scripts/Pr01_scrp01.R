##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 1
###
### 1. Instalación y activación de librerías
### 2. Apertura de 1 archivo CSV
### 3. Graficación inicial
### 4. Sumario de señales
##################

##################
### 1. Instalación y activación de librerías, carga de funciones
##################

# Instalación y activación de librerías
# readr
install.packages("readr")
library(readr)

# lubridate
install.packages("lubridate")
library(lubridate)

##################
### 2. Apertura de 1 archivo CSV
##################

# Gestión de directorios
# Directorio de trabajo
wd<-getwd()
# NOTA: Revísese que wd apunta al directorio en el que se ha dejado la información descargada
# por ejemplo, "C:/.../PhD_Course_Practice_01_R"
# en caso contrario, ejecutar la instancia de más abajo (reemplazando la ruta por la que corresponda)
# wd<-"C:/.../PhD_Course_Practice_01_R"

# Archivo de datos
file<-"/2_Data/Data_Pr01.csv"

# Ruta completa
ruta<-paste(wd,file, sep="")

# Lectura de un archivo csv
dataset <- read.csv(ruta, sep=";")
View(dataset)

##################
### 3. Graficación inicial
##################
# Seleccionar un día de datos
subset<-dataset[dataset$Day_Year==1,]

# Graficar carga térmica vs hora del día
plot(subset$Hour_Day,subset$Power.kW.)

# Mejorando algo el gráfico (leyendas)
plot(subset$Hour_Day,subset$Power.kW.,
     xlab="hora", ylab="potencia [kW]")

# Mejorando algo el gráfico (escalas)
plot(subset$Hour_Day,subset$Power.kW.,
     xlab="hora", ylab="potencia [kW]",
     xlim= c(0,23), ylim= c(0,50))


# Graficar carga térmica vs temperatura ambiental exterior
plot(subset$Temperature,subset$Power.kW.)

# Mejorando algo el gráfico (leyendas y escalas)
plot(subset$Temperature,subset$Power.kW.,
     xlab="temperatura [ºC]", ylab="potencia [kW]",
     xlim= c(-5,5), ylim= c(0,50))


##################
### 4. Sumario de señales
##################
# La función sumary() da información sobre los estadísticos de los valores numéricos
# Si los valores no son numéricos, te indica el tipo de clase
# NOTA. Que una variable tenga aperéntemente números, no significa que R los entienda como número
summary(dataset)
summary(dataset$Power.kW.)
summary(dataset$Week_Day)

# las funciones head() y tail() dan los valores iniciales y finales de un objeto
head(dataset)
head(dataset$Wind.speed)
tail(dataset)
tail(dataset$Wind.speed)

# las funciones min(), max() y mean() dan los valores mínimos, máximos y promedio de un objeto
min(dataset$Power.kW.)
max(dataset$Power.kW.)
mean(dataset$Power.kW.)
# con dataframes completos y valores no numéricos dan errores y/o valores anómalos
min(dataset)            #Error
min(dataset$Week_Day)   #Aparéntemente valor erróneo
mean(dataset$Week_Day)  #Error

# Las funciones dim() y length() dan las dimensiones (2D) y longitudes (1D) de objetos
dim(dataset)
length(dataset$Power.kW.)
length(dataset)

