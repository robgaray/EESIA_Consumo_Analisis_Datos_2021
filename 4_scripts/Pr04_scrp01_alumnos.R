### SCRIPT 4_1: ENTRAMOS EN CALOR/REPASO
##################
### Curso de Doctorado
### Análisis de datos para cargas energéticas en edificios. Contexto, Métodos de análisis, Herramientas y Aplicaciones
### UPV/EHU, 2021
### Roberto GARAY MARTINEZ, roberto@robertogaray.com / roberto.garay@Tecnalia.com
### Mikel LUMBRERAS MUGAGUREN
##################

##################
### Práctica 03
### Script 1: REPASO, entramos en calor

## CARGAMOS LOS ARCHIVOS QUE CREAMOS EN LA PREACTICA ANTERIOR
## INTRODUCIR EL DIRECTORIO DONDE HABEIS GUARDADO LOS DATOS DE LA PRACTICA 2
setwd("C:/Users/MikelLumbreras/OneDrive - Managing Innovation Strategies (MainStrat)/EHU/Sesion_2/Practica_02_CursoR-master/Results")

## LEER EL ARCHIVO
ResultadosPractica02 <- read.csv(file = "TestingFrame.csv" , header = T , sep = ",")
ResultadosPractica02 <- ResultadosPractica02[,-1]

### VAMOS A SEPARAR DIBUJAR POR RANGOS DE TEMPERATURAS
summary(ResultadosPractica02)

### SACAR LOS QUARTILES DE LA TEMPERATURA USANDO LA FUNCION quantile()
CuartilesTemperatura <- quantile(ResultadosPractica02$Temperature)

  
### APRENDEMOS A USAR LISTAS
## CADA ELEMENTO DE LA LISTA GIARDARA UN FRAME
RangosList <- list()
for (i in 1:(length(CuartilesTemperatura)-1))
{
  RangosList[[i]] <- ResultadosPractica02[which(ResultadosPractica02$Temperature >= CuartilesTemperatura[i] & 
                                                  ResultadosPractica02$Temperature <= CuartilesTemperatura[i+1]),]
}
### CREAMOS EL VECTOR FACTOR QUE NOS DESCRIBE EL RANGO DE TEMPERATURAS
VectorRangos <- c()
for (i in 1:length(RangosList)) ### del 1 al 4
{
  VectorRangos[i] <- paste(as.character(round(CuartilesTemperatura[i] ,digits = 2)), as.character(round(CuartilesTemperatura[i+1] ,digits = 2)) , sep = "-")
}

### EXPLICAR COMO SE FORMA LA ARQUITECTURA DE DATOS PARA USAR ggplot()
## GENERAR FRAME QUE ALIMENTARÁ AL GGPLOT
### AHORA VAMOS A DIBUJAR LOS CUARTILES PARA DIFERENTES RANGOS DE TEMPERATURA QUE DEIFNEN RANGOS DE CARGA TERMICA
FramePlotear <- data.frame(matrix(ncol = 3 , nrow = 216))
colnames(FramePlotear) <- c("TEMP RANGE" , "kWh" , "TYPE")
FramePlotear[,1] <- c(rep(VectorRangos[1] , times = length(RangosList[[1]][,1])*3), rep(VectorRangos[2] , times = length(RangosList[[2]][,1])*3),
                      rep(VectorRangos[3] , times = length(RangosList[[3]][,1])*3),rep(VectorRangos[4] , times = length(RangosList[[4]][,1])*3))

FramePlotear[,3] <- rep(c(rep("REAL" , times = length(RangosList[[1]][,1])) , rep("REGRESION UV" , times = length(RangosList[[1]][,1])) , 
                          rep("REGRESION MV" , times = length(RangosList[[1]][,1]))), times = 4)

VectorRango <- c(RangosList[[1]]$Power.kW. , RangosList[[1]]$Regresion_UV , RangosList[[1]]$Regresion_MV)
for (n in 2:length(RangosList))
{
  VectorRango <- c(VectorRango , c(RangosList[[n]]$Power.kW. , RangosList[[n]]$Regresion_UV , RangosList[[n]]$Regresion_MV))
}

FramePlotear[,2] <- VectorRango

### AHORA PLOTEAMOS EL BOXPLOT
library("ggplot2")
dev.new()
ggplot(data = FramePlotear , aes(x = `TEMP RANGE` , y = kWh , fill = TYPE)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.title = element_text(size = 14 , face = "bold") , 
        axis.text.x = element_text(size = 12 , angle = 0 , face = "bold") , 
        axis.text.y  = element_text(size = 12 , angle = 0 , face = "bold")) + 
  theme(panel.background = element_blank() , panel.grid.major =  element_line(colour = "grey" , size = 0.2)) + 
  theme(title = element_text(size = 16 , face = "bold")) + 
  theme(legend.title = element_text(size = 16 , face = "bold") , legend.text = element_text(size = 16)) + 
  theme(strip.text.x = element_text(size = 16 , face = "bold")) + ggtitle("Demanda por Temperaturas")

### LOS FACTORES SE ORDENAN MAL
FramePlotear$`TEMP RANGE` <- factor(FramePlotear$`TEMP RANGE` , levels = VectorRangos)

#### VOLVER A DIBUJAR: Rellenar primera linea
dev.new()
ggplot(data =  , aes()) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set1") + 
  theme(axis.title = element_text(size = 14 , face = "bold") , 
        axis.text.x = element_text(size = 12 , angle = 0 , face = "bold") , 
        axis.text.y  = element_text(size = 12 , angle = 0 , face = "bold")) + 
  theme(panel.background = element_blank() , panel.grid.major =  element_line(colour = "grey" , size = 0.2)) + 
  theme(title = element_text(size = 16 , face = "bold")) + 
  theme(legend.title = element_text(size = 16 , face = "bold") , legend.text = element_text(size = 16)) + 
  theme(strip.text.x = element_text(size = 16 , face = "bold")) + ggtitle("Demanda por Temperaturas")


#### FIN DEL SCRIPT -------------------------------------------------------------------------------------



