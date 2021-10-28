## Archivo con funciones para su uso en la Práctica 01 Script 03

sumario_carga_mes<- function(dataset,month) {
  subset <- dataset[dataset$Month==month,]
  MIN  <-min(subset$Power.kW.)
  MEAN <-mean(subset$Power.kW.)
  MAX  <-max(subset$Power.kW.)
  return(c(MIN, MEAN, MAX))
}

sumario_carga_dia<- function(dataset,day) {
  subset <- dataset[dataset$Week_Day==day,]
  MIN  <-min(subset$Power.kW.)
  MEAN <-mean(subset$Power.kW.)
  MAX  <-max(subset$Power.kW.)
  return(c(MIN, MEAN, MAX))
}