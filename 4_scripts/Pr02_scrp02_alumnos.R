#################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 02
### Script 2: ANALISIS DE CORRELACIONES
rm(list = ls())
### LIMPIAMOS EL ENTORNO

### VAMOS A CARGAR EL ARCHIVO GENERADO CON LOS DATOS DIARIOS
## PARA ELLO CREAMOS UNA FUNCION: INPUT: DIRECTORIO, OUTPUT: DATOS
getwd()
Direc <- "C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_2/Practica_02_CursoR-master/Data/"
LecturaDatos <- function(DirectorioDatos)
{
  setwd(DirectorioDatos)
  BuildingData <- read.csv(paste(DirectorioDatos , "BuildingDataDay.csv" , sep = "") , header = T , sep = ",")
  BuildingData <- BuildingData[,-1]
  ### QUE FALTA AQUÍ?
  return(BuildingData)
}

DatosEdificio <- LecturaDatos(Direc)

## PARA VER QUE ESTÁN TODOS LOS DATOS CORRECTAMENTE:
summary(DatosEdificio)

## COMO VERÉIS, HAY DATOS QUE SON NAs <-- QUITARLOS
## ES VITAL LIMPIAR LOS DATOS <-- SI NO, SE PUEDE LLEGAR A COINCLUSIONES ERRONEAS
## PASO 1: EJECUTAR
is.na(DatosEdificio$Wind.direction)
## PASO 2: EJECUTAR
which(is.na(DatosEdificio$Wind.direction) == T)
## PASO 3: ESCRBIR EL CODIGO QUE NOS QUITE LOS NAs
DatosEdificio <- DatosEdificio[-which(is.na(DatosEdificio$Wind.direction) == T),]

###

### COMPROBAMOS QUE YA NO HAY NAs
summary(DatosEdificio)


### VAMOS A ANALIZAR LA CORRELACION ENTRE VARIABLES
## PARA ELLO, VAMOS A INSTALAR Y CARGAR UNA LIBRERIA DE R LLAMADA ggpubr
## LO PODEMOS HACER DE VARIAS MANERAS: EN CODIGO:
#install.packages("ggpubr")
library("ggpubr")

## EXISTEN OTRAS MANERAS DE CARGAR Y BUSCAR LIBRERIAS <- ver

### cor() y cor.test() son funciones que están implementadas dentro de esta librería
## Estas funciones tienen implementadas las ecuaciones de pearson, spearman entre otros

## EJECUTAR LAS SIGUIENTES LINEAS PARA VER COMO SE LLAMAN A ESTAS FUNCIONES
help("cor")
help("cor.test")



#The result of rquery.cormat function is a list containing the following components :
#r : The table of correlation coefficients
#p : Table of p-values corresponding to the significance levels of the correlations
#sym : A representation of the correlation matrix in which coefficients are replaced by symbols according to the strength of the dependence. For more description, see this article: Visualize correlation matrix using symnum function
#In the generated graph, negative correlations are in blue and positive ones in red color.

## 1. CORRELATION TEST (VOLVEMOS A LA TEORIA 10')

## PEARSON
### EJEMPLO PARA CONSEGUIR COEF. DE PEARSON ENTRE DEMANDA Y TEMPERATURA EXTERIOR
cor(DatosEdificio$Temperature , DatosEdificio$Power.kW., method = c("pearson"))
cor.test(DatosEdificio$Temperature , DatosEdificio$Power.kW., method = c("pearson"))
## SPEARMAN <-- HACER LOS MISMO QUE CON EL COEFICIENTE DE PEARSON PERO CON SPEARMAN
## UTILIZAR LA AYUDA DE R DE EXPLICACION DE LAS FUNCIONES
cor(DatosEdificio$Temperature , DatosEdificio$Power.kW., method = c("spearman"))
cor.test(DatosEdificio$Temperature , DatosEdificio$Power.kW., method = c("spearman"))

### PLOTEAMOS ESTE EJEMPLO DE CORRELACION
### PARA GENERAR UN NUEVO DIBUJO dev.new()
dev.new()
ggscatter(DatosEdificio, x = "Temperature", y = "Power.kW.", 
        add = "reg.line", conf.int = TRUE, 
        cor.coef = TRUE, cor.method = "pearson",
        xlab = "OUTDOOR TEMPERATURE", ylab = "DEMAND")

## DE ESTA FORMA PODRIAMOS ANALIZAR UNA A UNA LA CORRELACION ENTRE VARIABLES
## HACER LO MISMO PERO AHORA CON LA RADIACION SOLAR
{
  cor(DatosEdificio$Irradiation.flux , DatosEdificio$Power.kW., method = c("pearson"))
  cor.test(DatosEdificio$Irradiation.flux , DatosEdificio$Power.kW., method = c("pearson"))
  ## SPEARMAN <-- HACER LOS MISMO QUE CON EL COEFICIENTE DE PEARSON PERO CON SPEARMAN
  ## UTILIZAR LA AYUDA DE R DE EXPLICACION DE LAS FUNCIONES
  cor(DatosEdificio$Irradiation.flux , DatosEdificio$Power.kW., method = c("spearman"))
  cor.test(DatosEdificio$Irradiation.flux , DatosEdificio$Power.kW., method = c("spearman"))
  
  ### PLOTEAMOS ESTE EJEMPLO DE CORRELACION
  ### PARA GENERAR UN NUEVO DIBUJO dev.new()
  dev.new()
  ggscatter(DatosEdificio, x = "Irradiation.flux", y = "Power.kW.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "W/m2", ylab = "DEMAND")
  
}

## 2. CORRELATION MATRIX
## ESTE PASO SE PODRÍA HACER UNO A UNO PARA CADA VARIABLE O USAR LA SIGUIENTE LIBRERIA

## GENERAMOS LA DATAFRAME CON LAS VARIABLES QUE NOS INETERESAN
FrameCorrelation <- cbind.data.frame(DatosEdificio$Power.kW. , DatosEdificio$Temperature , 
                                     DatosEdificio$Wind.speed , DatosEdificio$Wind.direction , DatosEdificio$Irradiation.flux)


install.packages("corrplot")
library("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
{
  dev.new()
  a <- rquery.cormat(FrameCorrelation)
}

PearsonCoef <- a[[1]]


### FIN DEL SCRIPT 2
## VOLVEMOS A LIMPIAR EL ENVIRONMENT

rm(list = ls())

##########################################################################################################





