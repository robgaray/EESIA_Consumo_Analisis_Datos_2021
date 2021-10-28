##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 3
### 
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Carga de archivos
###    Formateo de marcas de tiempo
### 3. Indexación y acceso a variables
### 4. IF
### 5. FOR
### 6. WHILE
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
### 3. Indexación y acceso a variables
### 3.1 Matrices y vectores
### 3.2 Data frames
### 3.2.1 Acceso por posición
### 3.2.2 Subseteado por posición
### 3.2.3 Subseteado por booleano
### 3.2.4 Acceso por variables
### 3.3 Listas
##################


# 3.1 Matrices y vectores
mat<-matrix(1:6, ncol=3)
mat
mat[1,3]
mat[2,1]

# 3.2 Data frames
# 3.2.1 Acceso por posición
head(dataset)
dataset[3,12]

# 3.2.2 Subseteado por posición
dataset[1:3,12:15]
dataset[1:3,c(12,7,8)]

# 3.2.3 Subseteado por booleano
names(dataset)=="Power.kW."
dataset[1:3,names(dataset)=="Power.kW."]
dataset[dataset$Day_Year<=2,names(dataset)=="Power.kW."]

# 3.2.4 Acceso por variables
dataset$Power.kW.

# 3.3 Listas/Tablas
tabla<-summary(dataset)
tabla
tabla[[25]]


##################
### 4. IF
### 4.1 Definir períodos activación calefacción
### Si T<15ºC ACTIVO
### Si Y>=15ºC DESACTIVADO
##################
ACT_CALEF<-rep(0,dim(dataset)[1])
for (i in 1:length(ACT_CALEF))
{
  if (dataset$Temperature[i]<15)
  {
    ACT_CALEF[i]<-1
  } else
  {
    ACT_CALEF[i]<-0
  }
}

summary(ACT_CALEF)
plot(ACT_CALEF)

##################
### 5. FOR
### 5.1 Generar las primeras 10 potencias de 2
##################
for (i in 1:10)
{
  print(2^i)
}

##################
### 6. WHILE
### 6.1 Generar las primeras 10 potencias de 2
##################
i<-1
while (i<=10)
{
  print(2^i)
  i<-i+1
}