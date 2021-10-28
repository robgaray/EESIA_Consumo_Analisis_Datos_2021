## Archivo con funciones para su uso en la Práctica 01 Script 02

sumario_carga_mes<- function(dataset,month) {
  subset <- dataset[dataset$Month==month,]
  MIN  <-min(subset$Power.kW.)
  MEAN <-mean(subset$Power.kW.)
  MAX  <-max(subset$Power.kW.)
  return(c(MIN, MEAN, MAX))
}