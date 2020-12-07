install.packages(c("openxlsx", "tidyverse"))

library(openxlsx)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/salabiblioteca/Downloads/Tesis")

############################## Carga de datos ##############################

load("Datos_EMB2017.Rdata")

EMB2017 <- read.csv("http://www.sdp.gov.co/sites/default/files/variables_adicionales_hogar_v3.txt", sep = ";")

NameMun <- read.xlsx("Nombre municipios de Cundinamarca.xlsx")

NameMun$DPTOMPIO <- as.numeric(NameMun$DPTOMPIO)

Datos <- Datos %>% mutate(PEA = (Total_ocupados + Total_desocupados))

DatosEMB2017 <- Datos %>% inner_join(EMB2017[, c("DIRECTORIO_HOG", "INGRESOS_HOG", "INGRESOS_PER_CAPITA")],
                                     by = "DIRECTORIO_HOG")

estvar.mean <- function(x, weights, nsim){
  
  est_mean <- numeric(nsim) 
  
  for(i in 1:nsim){
    set.seed(i)
    pi_k <-  1/weights
    sel <- sample(length(x), replace = T, prob = pi_k) 
    est_mean[i] <- sum(x[sel] * weights[sel]) / sum(weights[sel])
  }
  est_var <- var(est_mean)
  est_var
}

estvar.ratio <- function(y, z, weights, nsim){
  
  est_ratio <- numeric(nsim) 
  
  for(i in 1:nsim){
    set.seed(i)
    pi_k <-  1/weights
    sel <- sample(length(y), replace = T, prob = pi_k) 
    est_ratio[i] <- sum(y[sel] * weights[sel]) / sum(z[sel] * weights[sel])
  }
  est_var <- var(est_ratio)
  est_var
}

#Estimaciones directas del ingreso medio mediante el estimador de HÃ¡jek y de la tasa de desempleo

set.seed(123456)

EstimacionesDirectas <- DatosEMB2017 %>% group_by(DPTOMPIO) %>% summarise(n = n(), TDD = sum(Total_desocupados*FEX_C)/sum(PEA*FEX_C),
                                                                          EstVarTDD = estvar.ratio(Total_desocupados, PEA, FEX_C, 5000),
                                                                          CVTDD = sqrt(EstVarTDD)/TDD*100,
                                                                          IncomeMean = sum(INGRESOS_PER_CAPITA*FEX_C)/sum(FEX_C),
                                                                          EstVarIncMean = estvar.mean(INGRESOS_PER_CAPITA, FEX_C, 5000),
                                                                          CVIncMean = sqrt(EstVarIncMean)/IncomeMean*100)

EstimacionesDirectas <- EstimacionesDirectas %>% inner_join(NameMun, by = "DPTOMPIO")

EstimacionesDirectas <- EstimacionesDirectas[, c(1, 9, 2, 3, 4, 5, 6, 7, 8)]



write.xlsx(EstimacionesDirectas, file = "EstimacionesDirectasPorMPIO.xlsx")
