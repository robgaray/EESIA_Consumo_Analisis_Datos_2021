#################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 02
### Script 3: INTRODUCCION A REGRESIONES LINEALES CON R

rm(list=ls())

### VAMOS A COMPARAR DOS REGRESIONES LINEALES, PLOTEANDOLAS, CALCULAR METRICAS DE ERROR
### VAMOS A APRENDER A USAR GGPLOT <-- HERRAMIENTA MUY POTENTE PARA DIBUJAR

## PARA ELLO, VAMOS A CARGAR LOS DATOS PREVIAMENTE TRATADOS

## PASO 1: SEPARAR DATOS DE ENTRENAMIENTO Y DATOS DE TESTEO <-- Funcion 1
## PASO 2: CALCULAR COEFICIENTES DE REGRESION <-- MODELO ENTRENAMIENTO
## PASO 3: PROBAR MODELO EN DATOS DE TESTEO
## PASO 4: CALCULAR METRICAS DE ERROR

### CADA UNO PONER LA SUYA
Direc <-  "C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_2/Practica_02_CursoR-master/Data/"

#### FUNCION PARA LEER DATOS DIARIOS
LecturaDatos <- function(DirectorioDatos)
{
 ### ESCRIBIR AHORA VOSOTROS EL CODIGO PARA LEER Y DEVOLVER EL FRAME CREADO EN EL PRIMER SCRIPT
  setwd(DirectorioDatos)
  BuildingData <- read.csv(paste(DirectorioDatos , "BuildingDataDay.csv" , sep = "") , header = T , sep = ",")
  BuildingData <- BuildingData[,-1]
  ### QUE FALTA AQUÍ?
  return(BuildingData)
}

### AL IGUAL QUE ANTES VAMOS A QUITAR LOS DATOS DEFECTUOSOS
DatosEdificio <- LecturaDatos(Direc)
DatosEdificio <- DatosEdificio[-which(is.na(DatosEdificio$Wind.direction) == T),]

## FUNCION 1 <- SEPARAR TRAIN Y TEST (80% TRAIN Y 20% TEST)
## NOS DEVOLVERÁ DOS FRAMES <-- TRAIN Y TEST, RESPECTIVAMENTE
TrainingTesting <- function(FrameEdificio)
{
  ### ESTO NOS SIRVE PARA QUE ESTE CODIGO SEA SIMEPRE REPRODUCIBLE Y SE IBTENGAN LOS MISMOS INDICES ALEATORIOS
  set.seed(101)
  ## 80% of the sample size
  smp_size <- floor(0.8 * nrow(FrameEdificio))
  ## set the seed to make your partition reproducible
  train_indexes <- sample(seq_len(nrow(FrameEdificio)), size = smp_size)
  FrameTraining <- FrameEdificio[train_indexes, ]
  FrameTesting <- FrameEdificio[-train_indexes, ]
  ### DEFINIMOS CUALES SON NUESTROS 
  return(list(FrameTraining , FrameTesting))
}

RegresionLinealUnivariable <- function(FrameTraining)
{
  ## Q = A + BxT
  ## DEFINIMOS EL VECTOR DE SALIDA COMO UN FRAME
  CoeficientesUV <- data.frame(matrix(ncol = 2 , nrow = 1))
  colnames(CoeficientesUV) <- c("A" , "B")
  CoeficientesUV[1,] <- lm(FrameTraining$Power.kW. ~ FrameTraining$Temperature , data = FrameTraining)[[1]]
  return(CoeficientesUV)
}

RegresionLinealMultivariable <- function(FrameTraining)
{
  ## Q = A + BxT + CxGt + DxWs + ExWd
  ### UTILIZAR LA FUNCIOON DE LA REGRESIOON UNIVARIABLE COMO BASE
  CoeficientesMV <- data.frame(matrix(ncol = 5 , nrow = 1))
  colnames(CoeficientesMV) <- c("A" , "B", "C", "D", "E")
  CoeficientesMV[1,] <- lm(FrameTraining$Power.kW. ~ FrameTraining$Temperature + FrameTraining$Irradiation.flux +
                             FrameTraining$Wind.speed + FrameTraining$Wind.direction                           , 
                           data = FrameTraining)[[1]]
  return(CoeficientesMV)
}

TesteandoCoeficientes <- function(FrameTesting , Coef_UV , Coef_MV)
{
  ### Opcion 1: usar los coeficientes calculados anteoriormente
  Regresion_1 <- c()
  Regresion_2 <- c()
  for (i in 1:length(FrameTesting[,1])) ## RECORREMOS TODO EL FRAME DE TESTING
  {
    Regresion_1[i] <- Coef_UV$A + FrameTesting$Temperature[i]*Coef_UV$B 
    Regresion_2[i] <- Coef_MV$A + FrameTesting$Temperature[i]*Coef_MV$B + FrameTesting$Irradiation.flux[i]*Coef_MV$C + 
      FrameTesting$Wind.speed[i]*Coef_MV$D + FrameTesting$Wind.direction[i]*Coef_MV$E
  }
  ### PARA DEVOLVER MAS DE UNA VARIABLE HAY QUE PONER LIST
  return(list(Regresion_1 , Regresion_2))
}

#### CARGAMOS EL PAQUETE
install.packages("Metrics")
library("Metrics")

### VUELTA A LA TEORIA PARA REPASAR ECUACIONES (10')
MetricasError <- function(Regresion , FrameTesting)
{
  ## VAMOS A CALCULAR DOS METRICAS DE ERROR: R2 Y RMSE
  ## PODEMOS CALCULARLO UTLIZANDO LA FORMULA ORIGINAL
  SSE <- c()
  SSYY <- c()
  SSE <-  sum((FrameTesting$Power.kW.- Regresion)^2)
  SSYY <- sum((FrameTesting$Power.kW. - mean(Regresion))^2)
  RSquaredValue <- 1-(SSE/SSYY)
  ### RMSE
  ## PODEMOS USAR LIBRERÍAS YA PREDEFINIDAS
  RMSE <- rmse(FrameTesting$Power.kW., Regresion)
  return(list(RSquaredValue, RMSE))
}

### PLOTEANDO LAS DOS REGRESIONES USANDO GGPLOT
## INSTALAR Y CARGAR LIBRERÍA 
install.packages("ggplot2")
library("ggplot2")
PloteandoRegresiones <- function(RegresionUV, RegresionMV , FrameTesting)
{
  ### ESTA LIBRERIA EXIJE CAMBIAR DE FORMATO A LOS FRAMES ANTERIORES
  Frameggplot <- data.frame(matrix(ncol = 3 , nrow = length(FrameTesting[,1])*3))
  colnames(Frameggplot) <- c("OUTDOOR.TEMP [ºC]" , "DEMAND [kWh]" , "TYPE")
  Frameggplot[,1] <- rep(FrameTesting$Temperature , times = 3)
  Frameggplot[,2] <- c(FrameTesting$Power.kW. , RegresionUV , RegresionMV)
  Frameggplot[,3] <- c(rep("REAL" , times = length(FrameTesting[,1])) , rep("Regresion UV" , times = length(FrameTesting[,1])),
                       rep("Regresion MV" , times = length(FrameTesting[,1])))
  dev.new()
  ggplot(data = Frameggplot , aes(x = `OUTDOOR.TEMP [ºC]` , y = `DEMAND [kWh]` , color = TYPE)) + 
    geom_point(size = 1.6) + 
    scale_color_brewer(palette = "Set1") +
    theme(axis.title = element_text(size = 14 , face = "bold") , 
          axis.text = element_text(size = 12 , angle = 0 , face = "bold")) + 
    ggtitle("Regresiones") + 
    theme(panel.background = element_blank() , panel.grid.major =  element_line(colour = "grey" , size = 0.2)) + 
    theme(title = element_text(size = 16 , face = "bold")) + 
    theme(legend.title = element_text(size = 16 , face = "bold") , legend.text = element_text(size = 16)) + 
    guides(colour = guide_legend(override.aes = list(size=4)))
  
}

#### AHORA USANDO LAS FUNCIONES QUE HEMOS CREADO VAMOS A IR CREANDO EL SCRIPT PRINCIPAL
### PASO 1 <-- DIVIDIMOS TRAINING Y TESTING
TrainingFrame <- TrainingTesting(FrameEdificio)[[1]]
TestingFrame <- TrainingTesting(FrameEdificio)[[2]]

### PASO 2 <-- CALCULAMOS LOS COEFICIENTES DE REGRESION
Coef_UniqueVariable <- RegresionLinealUnivariable(TrainingFrame)
Coef_MultiVariable <- RegresionLinealMultivariable(TrainingFrame)

### PASO 3 <-- HACEMOS LA REGRESION
Regresion_UniqueVariable <- TesteandoCoeficientes(TestingFrame,Coef_UniqueVariable,
                                                  Coef_MultiVariable)[[1]]
Regresion_MultiVariable <- TesteandoCoeficientes(TestingFrame,Coef_UniqueVariable,
                                                Coef_MultiVariable)[[2]] 

#### GUARDAMOS EL TESING FRAME Y EL TRAINING FRAME CON LAS REGRESIONES
setwd("C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_2/Practica_02_CursoR-master/Results")
write.csv(TrainingFrame, file = "TrainingFrame.csv")
TestingFrame <- cbind.data.frame(TestingFrame , Regresion_UniqueVariable , Regresion_MultiVariable)
colnames(TestingFrame)[c(22:23)] <- c("Regresion_UV" , "Regresion_MV")
write.csv(TestingFrame, file = "TestingFrame.csv")


FrameMetricasError <- data.frame(matrix(ncol = 2 , nrow = 2))
colnames(FrameMetricasError) <- c("R2" , "RMSE")
rownames(FrameMetricasError) <- c("UV" , "MV")
#### RELLENAR EL FRAME Y LO DIBUJAREMOS

### DIBUJAMOS Y LO GUARDAMOS AUTOMATICAMENTE <-- USAMOS LA FUNCION DEFINIDA PREVIAMNET
PloteandoRegresiones(Regresion_UniqueVariable , Regresion_MultiVariable ,FrameTesting)
ggsave(filename = "Regresiones.png" , plot = last_plot() , width = 7, height = 7 , units = "in")
dev.off()


