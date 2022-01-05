##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 02
### Script 1: REPASO DE CONCEPTOS

rm(list = ls())
#### VAMOS A RECORDAR COMO CARGAR UN CSV DE UN DIRECTORIO EN CONCRETO

## PARA OBTENER LA DIRECCION DE TRABAJO ACTUAL
getwd()

## PARA CAMBIAR EL DIRECTORIO A UNO EN CONCRETO <-- CAMBIAR CADA 1
Directorio <- "C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_2/Practica_02_CursoR-master/Data"   ### INSERTAR EL DIRECTORIO
setwd(Directorio)
## LEEMOS EL ARCHIVO QUE CONTIENE LOPS DATOS DEL EDIFICIO EN LA CARPETA DATA
DatosEdificioHora <- read.csv("BuildingData.csv" , header = T , sep = ",")
## QUITAR LA PRIMERA COLUMNA DEL 
DatosEdificioHora <- DatosEdificioHora[,-1] ### LA POSICION DE LA COLUMNA QUE QUE QUEREMOS QUITAR

### PASAMOS DE DATOS HORARIOS A DATOS DIARIOS <-- CREAR LA FUNCIÓN
DeHorarioaDiario <- function(DatosEdificioHora)
{
  ### GENERAMOS UN DATAFRAME DIARIO CON LAS MISMAS COLUMNAS QUE EL HORARIO, PERO SIN LAS HORAS
  #### ESTE FRAME TIENE 21 COLUMNAS, QUITANDO HOUR_DAY, HOUR_YEAR, PAR_INPAR Y MOVAVG
  ### EL RESTO PASAMOS A FORMATO DIARIO <-- ALGUNAS SE SUMAN, OTRAS MEDIAS, ETC. 
  DatosEdificioDia <- data.frame(matrix(ncol = 21 , nrow = max(DatosEdificioHora$Day_Year)))
  colnames(DatosEdificioDia)[c(1:6)] <- colnames(DatosEdificioHora)[1:6] 
  colnames(DatosEdificioDia)[7] <- "Day_Year"
  colnames(DatosEdificioDia)[8:21] <- colnames(DatosEdificioHora)[8:21]
  for (n in 1:length(DatosEdificioDia[,1]))
  {
    DatosDiarios <- DatosEdificioHora[DatosEdificioHora$Day_Year == n,]
    for (j in 1:6)
      DatosEdificioDia[n,j] <- DatosDiarios[1,j]
    DatosEdificioDia[n,7] <- n
    DatosEdificioDia[n,8] <- as.character(DatosDiarios[1,8])
    DatosEdificioDia[n,9] <- DatosDiarios[1,9]
    DatosEdificioDia[n,10] <- DatosDiarios[1,10]
    DatosEdificioDia[n,11] <- DatosDiarios[1,11]
    DatosEdificioDia[n,12] <- DatosDiarios[1,12]
    for (j in 13:20)
      DatosEdificioDia[n,j] <- mean(DatosDiarios[,j])
    DatosEdificioDia[n,21] <- sum(DatosDiarios$Irradiation.flux)
  }
  return(DatosEdificioDia)
}

### LLAMAMOS A LA FUNCION CREADA PARA CAMBIAR EL FORMATO DE HORARIO A DIARIO
DatosDiarios <- DeHorarioaDiario(DatosEdificioHora)

### GUARDAMOS EL ARCHIVO DIARIO (Mismo Directorio)
write.csv(DatosDiarios , file = "BuildingDataDay.csv")


### FIN DEL REPASO A LA PRACTICA 1
## VOLVEMOS A LIMPIAR EL ENVIRONMENT

rm(list = ls())

##########################################################################################################




