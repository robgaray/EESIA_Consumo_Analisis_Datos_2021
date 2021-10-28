##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 6
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Carga de archivos
###    Formateo de marcas de tiempo
### 3. Gráfico de series temporales(carga térmica vs tiempo)
### 4. División del area gráfica
### 5. Gráfico XY
### 6. Boxplot
### 7. Gráfico a archivo
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

# lubridate
library(lubridate)

# Carga de funciones
# Archivo de funciones
file<-"/4_Scripts/Pr01_fnct05.R"
# Ruta completa
ruta<-paste(wd,file, sep="")
# Carga de funciones
source(ruta)


##################
### 2. Carga de archivos
##################

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

##################
###    Formateo de marcas de tiempo
##################
dataset<-Date_Marks(dataset)

##################
### 3. Gráfico de series temporales(carga térmica vs tiempo)
##################
### 3.1 Plotear 1 mes
### 3.2 Plotear 1 semana
### 3.3 Coloreando cada día de un color
### 3.4 Leyendas y títulos
##################

# 3.1 Plotear 1 mes
mes<-3
subset<-dataset[dataset$Month==mes,]
plot(subset$date,subset$Power.kW.)


# 3.2 Plotear 1 semana
# A realizar por los alumnos
semana<-
subset<-
plot(            )

# 3.3 Coloreando cada día de un color
colores<-rainbow(              )
for (i in         :            )
{
  # seleccionar cada día concreto
  subset2<-subset[subset$                  ==i,]
  # apadir las líneas y puntos correspondientes
  lines(                                    , col=colores[i])
  points(                                   , col=colores[i])
}

# 3.4 Leyendas y títulos
# Ahora más limpio
plot(subset$Hour_Day, subset$Power.kW.,
     type="n",
     xlab= "hora del día",
     ylab= "carga térmica [kW]",
     xlim=c(0,24),
     ylim=c(0,75),
     xaxt = "n",
     yaxt = "n")
axis(1, at=seq(0,24, by=1 ))
axis(2, at=seq(0,75, by=25))

colores<-rainbow(7)
for (i in 1:7)
{
  # seleccionar cada día concreto
  subset2<-subset[subset$Week_Day==i,]
  # apadir las líneas y puntos correspondientes
  lines(subset2$Hour_Day, subset2$Power.kW., col=colores[i])
  points(subset2$Hour_Day, subset2$Power.kW., col=colores[i])
}

legend("topright",legend=1:7, col=colores, lty=1, cex=0.8)

##################
### 4. División del area gráfica
### 4.1 Gráfico único con perfiles mensual, semanal y diario. Carga térmica
### 4.2 Gráfico único con perfiles mensual, semanal y diario. Carga térmica y temperatura
##################

### 4.1 Gráfico único con perfiles mensual, semanal y diario. Carga térmica
# Dividir en 3 gráficos
par(mfrow = c(1, 3))

# Plotear 1 mes
mes<-1
subset<-dataset[dataset$Month==mes,]
plot(subset$date,subset$Power.kW.,type="l")

# Plotear 1 semana
semana<-1
subset<-dataset[dataset$Week_Year==semana,]
plot(subset$date,subset$Power.kW.,type="l")

# Plotear 1 día
dia<-3
subset<-dataset[dataset$Day_Year==dia,]
plot(subset$date,subset$Power.kW.,type="l")

par(mfrow = c(1, 1))

### 4.2 Gráfico único con perfiles mensual, semanal y diario. Carga térmica y temperatura
# A realizar por los alumnos
# Graficos VAR vs tiempo
# fila superior: VAR=Carga
# fila inferior: VAR=Temperatura
# fijar excalas: Carga: 0:75kW; Temperatura: -20-40ºC

# Dividir en 2x3 gráficos
par(mfrow = c(            ,            ))

# Plotear 1 mes, carga térmica



# Plotear 1 semana, carga térmica



# Plotear 1 día, carga térmica



# Plotear 1 mes, temperatura



# Plotear 1 semana, temperatura



# Plotear 1 día, temperatura



par(mfrow = c(1, 1))

##################
### 5. Gráfico XY
### 5.1 Gráfico XY. Carga vs Temperatura
### 5.2 Gráfico XY. Carga vs Temperatura. Color por mes
##################

# 5.1 Gráfico XY. Carga vs Temperatura
#(A realizar por los alumnos)
# Y: carga, escala 0-75
# X: temperatura, escala -20-40

plot(dataset$Temperature, dataset$Power.kW.,
     xlab= "temperatura [ºC]",
     ylab= "carga térmica [kW]",
     xlim=c(-20,40),
     ylim=c(0,75),)

# 5.2 Gráfico XY. Carga vs Temperatura. Color por mes
colores<-rainbow(12)
for (i in 1:12)
{
  # seleccionar cada día concreto
  subset<-dataset[dataset$Month==i,]
  # apadir los puntos correspondientes
  points(subset$Temperature, subset$Power.kW., col=colores[i])
}
legend("topright",legend=1:12, col=colores, lty=1, cex=0.8)


##################
### 6. Boxplot
##################
boxplot(dataset$Power.kW.)

boxplot(dataset[dataset$Month== 1,]$Power.kW.,
        dataset[dataset$Month== 2,]$Power.kW.,
        dataset[dataset$Month== 3,]$Power.kW.,
        dataset[dataset$Month== 4,]$Power.kW.,
        dataset[dataset$Month== 5,]$Power.kW.,
        dataset[dataset$Month== 6,]$Power.kW.,
        dataset[dataset$Month== 7,]$Power.kW.,
        dataset[dataset$Month== 8,]$Power.kW.,
        dataset[dataset$Month== 9,]$Power.kW.,
        dataset[dataset$Month==10,]$Power.kW.,
        dataset[dataset$Month==11,]$Power.kW.,
        dataset[dataset$Month==12,]$Power.kW.)

##################
### 7. Gráfico a archivo
##################
# (Manualmente, mediante el botón "export" del cuadro de plot)
jpeg('archivoimagen.jpg')
boxplot(dataset$Power.kW.)
dev.off()

