#### SCRIPT 4.3: CLUSTERIZACION NO-SUPERVISADA: K-MEANS
##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 04
### Script 3: CLUSTERING
#### LIMPIAMOS EL ENTORNO POR SI ACASO
rm(list = ls())

#### AHORA VAMOS A VER LAS LIBRERIAS QUE VAMOS A CARGAR E INSTALAR
## ESTA LIBRERIA CONTIENE EL ALGORITMO DE K-MEANS
install.packages("stats")
library("stats")
help("stats")
## ESTA LIBRERIA TRAE INCLUIDOS LOS CVI-S QUE NOS INTERESAN
install.packages("clusterCrit")
library("clusterCrit")
help("clusterCrit")
### ESTA LIBRERIA SE UTILIZARÁ PARA DIBUJAR
install.packages("ggplot2")
library("ggplot2")
help("ggplot2")

## PRIMER PASO: CARGAMOS LOS DATOS EN BRUTO, LES CAMBIAMOS EL LA FORMA
setwd("C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_4/Practica_04_CursoR-main/Data/")
BuildingData <- read.csv(file = "BuildingData.csv" , header = T , sep = ",")
BuildingData <- BuildingData[,-1]

#### ESTA FUNCION LA PODEIS COPIAR Y PEGAR, NO PERDAIS MAS TIEMPO
## ESTA DUNCION SIRVE PARA CAMBIAR EL FROMATO A LOS DATOS
## CLUSTERIZAMOS LOS DIAS DEL AÑO <- PERFILES TIPO
CambiandoFormato <- function(Building_Frame)
{
  Dias_Año <- unique(Building_Frame$Day_Year)
  Frame_Dia <- data.frame(matrix(ncol = 24 , nrow = length(Dias_Año)))
  for (n in 1:24)
    colnames(Frame_Dia) <- c(0:23)
  #### VAMOS A CREAR EL FRAME DE SALIDA
  ### HACEMOS LA TRANFORMADA PERO SOLO COGIENDO LOS DIAS ENTEROS
  Vector_sin_NA <- c()
  for (h in 1:length(Dias_Año))
  {
    Vector_Demanda <- Building_Frame[which(Building_Frame$Day_Year == Dias_Año[h]),]$Dela_T
    if (length(Vector_Demanda) == 24)
    {
      Vector_sin_NA <- c(Vector_sin_NA , h)
      Frame_Dia[h,1:24] <- Vector_Demanda
    }
    #### SI FALTA ALGUN DATO NO NOS VALE
  }
  ### BORRAMOS LAS FILAS QUE SEAN NAs
  Frame_Dia <- Frame_Dia[Vector_sin_NA, ] 
  ### RELLENAR 
  return(Frame_Dia)
}

### SIGUIENTE PASO: NORMALIZAR LOS DATOS
## LA FUNCION SCALE <-- help("scale")
NormalizandoDatos <- function(FrameProfiles)
{
  ### EJECUTAR EN LA CONSOLA: help("sd") y help("scale")
  for (i in 1:length(FrameProfiles[,1]))
  {
    #### DAN ERRORES EN LOS DIAS QUE LA SD = 0
    ### AHI DIAS SIN DEMANDA EN TODO EL DÍA
    if (sd(matrix(t(FrameProfiles[i, c(1:24)]))) == 0)
      FrameProfiles[i,] <- 0
    else
      FrameProfiles[i,] <- scale(matrix(t(FrameProfiles[i,])) , scale =  T) 
  }
  ### DEVOLBEMOS EL FRAME
  return(FrameProfiles)
}

### CREAMOS LA FUNCION PARA EL K-MEANS
### inputs <- K y el frame
ClusteringKmeansFunction <- function(FrameProfilesNormalizado , K_mean)
{
  set.seed(101)
  ## nstart es las veces que se inicia el algoritmo desde puntos diferentes
  ## centers = en cuantos grupos se quiere clusterizar
  ClustersList <- kmeans(FrameProfilesNormalizado , nstart = 50 , centers = K_mean , iter.max = 50)
  ### OBSERVAR LA LISTA DE LOS CLUSTERS
  #summary(ClustersList)
  Clusters <- ClustersList[["cluster"]]
  #summary(Clusters)
  ### AQUI RELLENAMOS LA FUNCION RETURN
  return(Clusters)
}

CVIAnalysis <- function(VectorCluster , FrameProfilesNormalizado)
{
  ## EL 1: DUNN, EL 2: SILHOUETTE Y EL 3: DAVIES-BOULDIN
  VectorCVI <- c()
  #PODEIS EJECUTAR EL SIGUIENTE LINEA, ARA VER COMO SE ORDENAN LOS CVIs 
  #getCriteriaNames(TRUE)
  NombresCVI <- c("Dunn" , "Silhouette" , "Davies_Bouldin")
  for (i in 1:length(NombresCVI))
    VectorCVI[i] <- intCriteria(as.matrix(FrameProfilesNormalizado) , VectorCluster , NombresCVI[i])[[1]]
  return(VectorCVI)
}

##### 1. GENERAMOS EL FRAME
FrameProfiles <- CambiandoFormato(BuildingData)
##### 2. NORMALIZAMOS
FrameProfilesNormalizado <- NormalizandoDatos(FrameProfiles)
##### 3. APLICAMOS EL KMEANS PARA K <- 3 Y PARA K <- 4
TresClusters <- ClusteringKmeansFunction(FrameProfilesNormalizado , 3)
CuatroClusters <- ClusteringKmeansFunction(FrameProfilesNormalizado , 4)

##### AHORA QUEDA ANALIZAR CUAL ES MEJOR CLUSTERIZACION: PARA ELLO VAMOS A ANALIZAR LOS INDICES DE SILHOUETTE, Y DUNN
### GENERAMOS EL FRAME PARA GUARDAR LOS INDICES
ClusterValidation <- data.frame(matrix(ncol = 2 , nrow = 3))
colnames(ClusterValidation) <- c("3 Clusters" , "4 Clusters")
rownames(ClusterValidation) <- c("Dunn Index" , "Silhouette Index" , "Davies-Bouldin Index")
ClusterValidation[,1] <- CVIAnalysis(TresClusters , FrameProfilesNormalizado)
ClusterValidation[,2] <- CVIAnalysis(CuatroClusters , FrameProfilesNormalizado)

## ¿QUE PROCESO ES MEJOR?
ClusterFrame <- cbind.data.frame(TresClusters , CuatroClusters)
colnames(ClusterFrame) <- c("K=3" , "K=4")

### FIN DE LA PRACTICA OBLIGATORIA
### VAMOS A UTILIZAR ESTAS FUNCIONES PARA DIBUJARLOS
TransformandoFrame <- function(FrameBuilding, ClusterFrame , Cluster_K)
{
  FrameggPlotCluster <- data.frame(matrix(ncol = 5 , nrow = 24*length(FrameBuilding[,1])))
  colnames(FrameggPlotCluster) <- c("HOUR" , "TEMP" , "CLUSTER" , "DAY" , "TYPE")
  VectorDia <- c(0:23)
  FrameggPlotCluster[,1] <- rep(VectorDia , times = length(FrameBuilding[,1]))
  VectorDem <- as.vector(FrameBuilding[1,1:24])
  for (n in 2:length(FrameBuilding[,1]))
    VectorDem <- cbind(VectorDem, FrameBuilding[n,1:24])
  FrameggPlotCluster[,2] <- t(VectorDem[1,])
  VectorCluster <- rep(ClusterFrame[1,Cluster_K-2] , times = 24)
  for (n in 2:length(ClusterFrame[,1]))
    VectorCluster <- c(VectorCluster , rep(ClusterFrame[n,Cluster_K-2] , times = 24))
  FrameggPlotCluster[,3] <- factor(VectorCluster , levels = as.factor(1:Cluster_K))
  VectorDay <- rep(1, times = 24)
  for (n in 2:length(FrameBuilding[,1]))
    VectorDay <- c(VectorDay , rep(n, times = 24))
  FrameggPlotCluster[,4] <- VectorDay 
  FrameggPlotCluster[,5] <- "REAL"
  return(FrameggPlotCluster)
}

### FUNCION 3 <-- DIBUJAR
DibujandoClusteres <- function(FrameggPlotCluster , Cluster_K)
{
  FrameDefPlot <- rbind.data.frame(FrameggPlotCluster)
  #### PARA CADA
  NumeroCol <- c()
  if (Cluster_K == 3 | Cluster_K == 6 | Cluster_K == 5 | Cluster_K > 6)
    NumeroCol <- 3
  if (Cluster_K == 4)
    NumeroCol <- 2
  dev.new()
  ggplot(data = FrameggPlotCluster, aes(x = HOUR , y = `TEMP` , group = DAY , colour = CLUSTER)) + geom_line() + 
    facet_wrap(~ CLUSTER , ncol = NumeroCol) + scale_color_brewer(palette = "Paired") + 
    theme(axis.title = element_text(size = 18 , face = "bold") , axis.text = element_text(size = 18)) + 
    theme(panel.background = element_blank() , panel.grid.major =  element_line(colour = "grey" , size = 0.2)) + 
    theme(title = element_text(size = 16 , face = "bold")) + theme(legend.key=element_blank()) + 
    theme(legend.title = element_text(size = 20 , face = "bold") , legend.text = element_text(size = 24)) + 
    theme(strip.text.x = element_text(size = 20 , face = "bold")) + 
    guides(color = guide_legend(override.aes = list(size = 3))) + 
    theme(legend.key.width = unit(1.2,"cm"))
}

#### DIBUJANDO LOS CLUSTERES POR SEPARADO
### 3 clusteres
DibujandoClusteres(TransformandoFrame(FrameProfilesNormalizado , ClusterFrame , 3) , 3)
dev.off()
### 4 clusteres
DibujandoClusteres(TransformandoFrame(FrameProfilesNormalizado , ClusterFrame , 4) , 3)
dev.off()

### facet_wrap()

rm(list=ls())

########### FIN DEL CURSO ################ ---------------------------------------------------------
