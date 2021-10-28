##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 1
### Script 2
###
### 0. Gestión de directorios
### 1. Instalación y activación de librerías, carga de funciones
### 2. Apertura de 1 archivo CSV
### 
### referencia
###
### 3. Sumario de cargas de un mes
### 4. Sumario de cargas mensual
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
file<-"/4_Scripts/Pr01_fnct02.R"
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

# generar una lista de valores
# sintaxis: VALOR_MINIMO:VALOR_MAXIMO
# ejemplo: 0:15
0:15

##################
### 3. Sumario de cargas de un mes
### 3.1 Sacar un subconjunto de datos con el mes elegido
### 3.2 Calcular los valores mínimo, promedio y máximo
##################
# EJERCICIO A REALIZAR POR LOS ALUMNOS
#
# Obtener los valores mínimo, promedio y máximo de la carga térmica para un mes concreto
# El alumno elegirá un mes (número entero en el rango 1-12)
# Se almacenarán los valores en las variables Power.kW.MIN, Power.kW.MEAN, Power.kW.MAX

# 3.1 Sacar un subconjunto de datos con el mes elegido
month  <- 
subset <- 

# 3.2 Calcular los valores mínimo, promedio y máximo
Power.kW.MIN  <-
Power.kW.MEAN <-
Power.kW.MAX  <-

##################
### 4. Sumario de cargas mensual
### 4.1 Crear e inicializar a 0 los valores de salida
### 4.2 Recorrer (de forma automática) todos los meses del año y guardar los valores en 3 vectores separados
### 4.3 Plotear los valores
##################
# EJERCICIO A REALIZAR POR LOS ALUMNOS
#
# Se ha generado una función que calcula un sumario de la carga térmica para un mes concreto.
# Con la siguiente forma:
#     sumario_carga_mes(DATOS,MES)
#     La salida es un vector de 3 posiciones. MIN, MEAN y MAX

# 4.1 Crear e inicializar a 0 los valores de salida
vect_MIN  <-
vect_MEAN <-
vect_MAX  <-
  
# 4.2 Recorrer (de forma automática) todos los meses del año y guardar los valores en 3 vectores separados
for (i in      :    )
{
  salida<-sumario_carga_mes(dataset,      )
  vect_MIN[i]  <- salida[     ]
  vect_MEAN[i] <- salida[     ]
  vect_MAX[i]  <- salida[     ]
}

# 4.3 Plotear los valores
plot(          )
