install.packages(c("lmtest", "openxlsx", "dplyr", "stringi", "stringr", "FactoMineR", "tidyr", "sae"))

install.packages("sae")

library(stargazer)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(FactoMineR)
library(sae)
library(lmtest)
library(sae)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(spdep)
library(ggplot2)

rm(list = ls())

setwd("C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019")

EstimacionesDirectas <- read.xlsx("EstimacionesDirectasPorMPIO.xlsx")

###############################################################################################
############################ Depuración variables de Terridata ###############################
#################################################################################################

#----Eliminamos el registro referente a Bogotá debido a que tiene un comportamiento atípico en el análisis--#

#EstimacionesDirectas <- EstimacionesDirectas[EstimacionesDirectas$NOMBMPIO != "BOGOTA",] 

#---- Lecura de la información contenida en Terridata ------------------------------------------------------#

TerriData <- read.csv("TerriData.txt", sep = ";", header = F, dec = ",")

#---- Filtro de la información solo despues del año 2000 para los municipios escogidos de 
# Cundinamarca y escogecia de los departamentos a los cuales se les realizó la estimación directa ----#

TerriData2000Cundi <- TerriData[TerriData$V10 >= 2000 & TerriData$V2 %in% c("Cundinamarca", "Bogotá") &
                                  tolower(stri_trans_general(TerriData$V4,"Latin-ASCII")) %in% tolower(EstimacionesDirectas$NOMBMPIO),]

#---- Pegado de la variable  TDD con su respectiva varianza estimada y cve y depuración de la data --#

TerriData2000Cundi <- TerriData2000Cundi %>% filter(V8 != "")

class(max(TerriData2000Cundi$V11))

TerriData2000Cundi <- TerriData2000Cundi %>% group_by(V3, V4, V7, V10) %>% filter(V11 == max(V11)) %>% ungroup()

TerriData2000Cundi <- TerriData2000Cundi %>% left_join(EstimacionesDirectas[, c("DPTOMPIO","TDD", "EstVarTDD")], by = c("V3" = "DPTOMPIO"))

TerriData2000Cundi$V8 <- as.character(TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- gsub("[.]", "", TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- gsub("[,]", ".", TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- as.numeric(TerriData2000Cundi$V8) 

#---- Cálculo de la correlación de las variables con la TDD --------------------------------------------#

TerriData2000Cundi$V7 <- as.character(TerriData2000Cundi$V7)

#Eliminamos las variables referentes al rendimiento de diferentes productos

CorrelaTDD <- TerriData2000Cundi %>% group_by(V7, V10) %>% summarise(CorrTDD = cor(V8, TDD)) %>% 
  ungroup() %>% filter(!str_detect(V7, "Rendimiento"))

#Correlaciones mayores y menores a 0.5

CorrelaTDDFilt <- CorrelaTDD %>% filter(abs(CorrTDD) > 0.4)

#Escogemos el año mas cercano de las variables escogidas

CorrelaTDDFilt <- CorrelaTDDFilt %>% group_by(V7) %>% filter(V10 == max(V10)) %>% ungroup() %>% 
  arrange(desc(abs(CorrTDD)))

CorrelaTDDFilt$V7

#Selección intuitiva de variables y ordenamiento en base a la correlación


# CorrelaTDDFilt <- CorrelaTDDFilt[-c(1, 3, 9:11, 20, 30, 33, 37),] %>% 
#   arrange(desc(abs(CorrTDD)))

#Se escogen las 3 variables con la mayor correlación para ternerlas en cuenta en el modelo.

VariablesMayorCor <- CorrelaTDDFilt[c(7, 9:11),] 

CorrelaTDDFilt$Filt <- paste(CorrelaTDDFilt$V7, CorrelaTDDFilt$V10, sep = "_")

#---- Selección de variables intuitivamente derivado de la correlación mas alta ----------------------------#

TerriData2000Cundi$Filt <- paste(TerriData2000Cundi$V7, TerriData2000Cundi$V10, sep = "_")

colnames(TerriData2000Cundi)

TDDMunic <- TerriData2000Cundi %>% dplyr::select(V3, V4, TDD, EstVarTDD) %>% 
  filter(!duplicated(V4)) %>% arrange(V4)

TDD <- TerriData2000Cundi %>% filter(Filt %in% CorrelaTDDFilt$Filt) %>% 
  dplyr::select(V3, V4, V7, V8)

TDD <- TDD[!duplicated(TDD), ]

#---- Reestructuración de la base y estandarización de las variables ---------------------------------------#

SPR <- TDD %>% spread(V7, V8)

rownames(SPR) <- as.character(SPR$V4)

SPR <- SPR %>% dplyr::select(-V3, -V4) %>% as.data.frame()

#Escogemos las variables que tienen la mayor correlación y las quitamos de la base, posterior a ello
#se incluiran en el modelo

Variables <- SPR[, VariablesMayorCor$V7]

#SPR <- SPR[, -which(colnames(SPR) %in% VariablesMayorCor$V7)]

SPR <- t(SPR) ; SPR <- SPR[complete.cases(SPR), ] ; SPR = SPR[which(apply(SPR, 1, function(x) {ifelse(0 %in% x, 1, 2)}) == 2), ]

SPR <- as.data.frame(t(SPR))


for (j in 1:ncol(SPR)){
  
  for (i in 1:nrow(SPR)){
    
    SPR[i, j] <- (SPR[i, j] - mean(SPR[, j]))/(sd(SPR[, j]))
    
  } 
  
  print(j)
  
}

#---- Componentes principales ------------------------------------------------------------------------------#

pca <- PCA(SPR, ncp = 10)

#---- Aplicación del modleo Fay Herriot --------------------------------------------------------------------#

DataModelTDD <- cbind(TDDMunic[, c(1, 3, 4)], Variables, pca$ind$coord)

colnames(DataModelTDD)

View(cor(DataModelTDD))

Sisben <- read.xlsx("C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019/SisbenAprobados.xlsx")

colnames(Sisben)

Sisben <- Sisben %>% filter(Departamento %in% c("CUNDINAMARCA", "BOGOTÁ, D.C.")) %>% 
  group_by(Municipio) %>% filter(Corte.del.Sisbén == max(Corte.del.Sisbén))

DataModelTDD <- DataModelTDD %>% left_join(Sisben[, c("Código.Municipio", "Numero.de.personas.validadas")],
                                                 by = c("V3" = "Código.Municipio"))

modelocompleto <- paste0("TDD", " ~ `", paste(colnames(DataModelTDD[, -c(1:3)]), collapse = "` + `"), "`")

modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = DataModelTDD))

summary(modelo_optimo)

stargazer(modelo_optimo, title =  "Resultados coordenadas")

stargazer(lm(TDD ~ `Porcentaje de predios urbanos` + `Porcentaje de inversión - Transporte` + 
             `Porcentaje de predios rurales` + 
             `Porcentaje población rural` + Numero.de.personas.validadas, data = DataModelTDD))

summary(lm(TDD ~ `Porcentaje de predios urbanos` + `Porcentaje de inversión - Transporte` + 
             `Porcentaje de predios rurales` + `Porcentaje población urbana` + 
             `Porcentaje población rural` + Numero.de.personas.validadas, data = DataModelTDD))


summary(modelo_optimo)

colnames(DataModelTDD)

Model1 <- mseFH(TDD ~ Dim.2 + 
                  Dim.4 + Dim.5 + Dim.9, data = DataModelTDD,
                vardir = EstVarTDD)

DataModelTDD$`Porcentaje de predios rurales` <- DataModelTDD$`Porcentaje de predios rurales`/100

Model2 <- mseFH(TDD ~ `Porcentaje de predios urbanos` + `Porcentaje de inversión - Transporte` + 
                  
                  `Porcentaje población rural` + Numero.de.personas.validadas, data = DataModelTDD,
                 vardir = EstVarTDD)

EstimacionesDirectasFH <- EstimacionesDirectas %>% dplyr::select(DPTOMPIO, NOMBMPIO, TDD, 
                                                                 EstVarTDD, CVTDD) %>% 
  mutate(EBLUPTDD = Model1$est$eblup, MSEEBLUP = Model1$mse, 
         CVMSEComp = 100*sqrt(Model1$mse)/Model1$est$eblup,
         EBLUPTDD2 = Model2$est$eblup, MSEEBLUP2 = Model2$mse, 
         CVMSE = 100*sqrt(Model2$mse)/Model2$est$eblup)

CVES <- EstimacionesDirectasFH[, c("NOMBMPIO", "CVMSEComp", "CVMSE")]

library(data.table)

colnames(CVES) <- c("NOMBMPIO", "FayHerriotComp", "Fay Herriot")

CVES <- CVES %>% as.data.table() %>% dplyr::arrange(desc(FayHerriotComp))

CVES$NOMBMPIO <- factor(CVES$NOMBMPIO, levels = CVES$NOMBMPIO[order(CVES$FayHerriotComp)])

CVES <- gather(CVES, "Estimadores", "CVE", -NOMBMPIO)

p <- ggplot(CVES, aes(CVE, NOMBMPIO))
p + geom_point(aes(colour = Estimadores), stat = 'identity') +
  labs(fill = "Estimadores") + xlab("Cve") + ylab("Municipios") + theme_bw()

write.xlsx(EstimacionesDirectasFH, file = "EstimacionesDirectasFHTDDWBog.xlsx")

#---- Fay Herriot Espacial ---------------------------------------------------------------------------#

poligonos_mpios <- readOGR("C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019/Clase7/MGN2016_00_NACIONAL/MGN_ADM_MPIO_POLITICO.shp" , layer = "MGN_ADM_MPIO_POLITICO")

View(poligonos_mpios@data)

names(poligonos_mpios)
#X11()
#plot(poligonos_mpios)

# Municipios Cundinamarca
poligonos_mpios_cund <- poligonos_mpios[poligonos_mpios$DPTO_CCDGO %in% c("25", "11"),]

View(poligonos_mpios_cund@data)

#X11()
#plot(poligonos_mpios_cund)
poligonos_mpios_cund$MPIO_CCDGO <- as.character(poligonos_mpios_cund$MPIO_CCDGO)
lista_poligonosvecinos <- spdep::poly2nb(poligonos_mpios_cund, queen = FALSE, snap = 1.490116e-40)
W <- nb2mat(lista_poligonosvecinos)
row.names(W) <- poligonos_mpios_cund$MPIO_CCDGO
colnames(W) <- poligonos_mpios_cund$MPIO_CCDGO
row.names(W)[1:116] <- paste0("25", row.names(W)[1:116])
colnames(W)[1:116] <- paste0("25", colnames(W)[1:116])
row.names(W)[117] <- paste0("11", row.names(W)[117])
colnames(W)[117] <- paste0("11", colnames(W)[117])


W <- W[order(row.names(W)),]
W <- W[,order(colnames(W))]

W_s <- W[row.names(W) %in% EstimacionesDirectas$DPTOMPIO, colnames(W) %in% EstimacionesDirectas$DPTOMPIO]
DataModelTDD <- arrange(DataModelTDD, V3)

SFH <- mseSFH(TDD ~ Dim.2 + Dim.4 + Dim.5 + Dim.9, data = DataModelTDD,
              vardir = EstVarTDD, proxmat = W_s)

SFH

#-----------------------Comparación Fay Herriots --------------------------------------------------###

EstimacionesDirectasFH$Y_SFH <- SFH$est$eblup
EstimacionesDirectasFH$mseY_SFH <- SFH$mse
EstimacionesDirectasFH$cve_SFH <- sqrt(SFH$mse) / SFH$est$eblup * 100

CVES <- EstimacionesDirectasFH[, c("NOMBMPIO", "CVTDD", "CVMSEComp","cve_SFH")]

library(data.table)

CVES <- CVES %>% as.data.table() %>% dplyr::arrange(desc(CVTDD))

colnames(CVES) <- c("NOMBMPIO",  "Directo", "FayHerriotComp", "FH Espacial")

CVES$NOMBMPIO <- factor(CVES$NOMBMPIO, levels = CVES$NOMBMPIO[order(CVES$Directo)])

CVES <- gather(CVES, "Estimadores", "CVE", -NOMBMPIO)

p <- ggplot(CVES, aes(CVE, NOMBMPIO))
p + geom_point(aes(colour = Estimadores), stat = 'identity') +
  labs(fill = "Estimadores") + xlab("Cve") + ylab("Municipios") + theme_bw() +
  scale_color_hue(direction = 1)

#---------------------Comparación media de Coeficientes de varaición-------------------------------####

CompaCve <- data.frame(mean(EstimacionesDirectasFH$CVMSE), mean(EstimacionesDirectasFH$cve_SFH))

colnames(CompaCve) <- c("FayHerriotComp", "Fay Herriot Espacial")

CompaCve

library(xtable)

xtable(CompaCve)

#---- Aplicación modelo Fay Herriot --------------------------------------------------------------------#

TerriData2000CundiOM <- TerriData[TerriData$V10 >= 2000 & TerriData$V2 == "Cundinamarca" &
                                    !tolower(stri_trans_general(TerriData$V4,"Latin-ASCII")) %in% c(tolower(EstimacionesDirectas$NOMBMPIO)),]

TerriData2000CundiOM <- TerriData2000CundiOM %>% filter(V8 != "")

class(max(TerriData2000CundiOM$V11))

TerriData2000CundiOM <- TerriData2000CundiOM %>% group_by(V3, V4, V7, V10) %>% filter(V11 == max(V11)) %>% ungroup()

TerriData2000CundiOM$V8 <- as.character(TerriData2000CundiOM$V8)

TerriData2000CundiOM$V8 <- gsub("[.]", "", TerriData2000CundiOM$V8)

TerriData2000CundiOM$V8 <- gsub("[,]", ".", TerriData2000CundiOM$V8)

TerriData2000CundiOM$V8 <- as.numeric(TerriData2000CundiOM$V8) 

TerriData2000CundiOM$Filt <- paste(TerriData2000CundiOM$V7, TerriData2000CundiOM$V10, sep = "_")

colnames(TerriData2000CundiOM)

VariablesMayorCor$V7

TDDPred <- TerriData2000CundiOM %>% filter(Filt %in% CorrelaTDDFilt$Filt) %>% 
  dplyr::select(V3, V4, V7, V8)

TDDPred <- TDDPred[!duplicated(TDDPred), ]

SPRPred <- TDDPred %>% spread(V7, V8)

rownames(SPRPred) <- SPRPred$V4

SPRPred <- SPRPred %>% dplyr::select(-V3, -V4) %>% as.data.frame()

SPRPred <- SPRPred[, colnames(SPR)]

for (i in 1:ncol(SPRPred)){

  if (sum(is.na(SPRPred[, i])) != 0){

    mod <- lm(SPRPred[complete.cases(SPRPred), i] ~., data = SPRPred[complete.cases(SPRPred), -i])

    SPRPred[, i][is.na(SPRPred[, i])] <- predict(mod, SPRPred[is.na(SPRPred[, i]), -i])

  }

}

SPRPred <- SPRPred[-1, ]

for (j in 1:ncol(SPRPred)){
  
  for (i in 1:nrow(SPRPred)){
    
    SPRPred[i, j] <- (SPRPred[i, j] - mean(SPRPred[, j]))/(sd(SPRPred[, j]))
    
  } 
  
  print(j)
  
}

modelo_optimo

pcaPred <- PCA(SPRPred, ncp = 10)

colnames(DataModelTDD)

DataModelTDDPredInS <- cbind(1, DataModelTDD[, c(9, 11, 12, 16)])

DataModelTDDPred <- cbind(1, pcaPred$ind$coord[, c(2, 4, 5, 9)])

colnames(DataModelTDDPredInS)

Beta_TDD <- as.matrix(SFH$est$fit$estcoef[1]) 

eblup.FH_noA_TDD = as.numeric(as.matrix(DataModelTDDPred) %*% Beta_TDD)

sigma2_u_TDD <- SFH$est$fit$refvar

V_TDD <- diag(sigma2_u_TDD + EstimacionesDirectasFH$EstVarTDD)

V_Beta_TDD <- t(as.matrix(DataModelTDDPredInS)) %*% solve(V_TDD, tol = 1e-30) %*% as.matrix(DataModelTDDPredInS)

mse.FH_noA_TDD = diag(as.matrix(DataModelTDDPred) %*% solve(V_Beta_TDD, tol = 1e-30) %*% t(as.matrix(DataModelTDDPred)))

# cve out of sample
cv.FH_noA_TDD <- 100 * sqrt(mse.FH_noA_TDD) / eblup.FH_noA_TDD

colnames(TDDPred)

NOM <- TDDPred %>% group_by(V4) %>% summarise(DPTOMPIO = unique(V3))

NOM <- NOM[-17, ]

outcome_TDD_UnobservedAreas <- data.frame(DPTOMPIO = NOM$DPTOMPIO,
                                          NOMBMPIO = names(mse.FH_noA_TDD),
                                          Y_SFH = eblup.FH_noA_TDD, 
                                          mseY_SFH = mse.FH_noA_TDD, cve_SFH = cv.FH_noA_TDD)
write.xlsx(outcome_TDD_UnobservedAreas, "UnobservedareasTDD.xlsx")

#----------------------- Gráficos --------------------------------------------------------------#

Basetotal <- bind_rows(EstimacionesDirectasFH[, c("DPTOMPIO", "NOMBMPIO", "Y_SFH", "mseY_SFH", "cve_SFH")],
                       outcome_TDD_UnobservedAreas)

write.xlsx(Basetotal, "EstimacionesTotalesTDD.xlsx")

poligonos_mpios <- readOGR( "C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019/25_CUNDINAMARCA/ADMINISTRATIVO/MGN_MPIO_POLITICO.shp")

poligonos_Bta <- readOGR( "C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019/11_BOGOTA/ADMINISTRATIVO/MGN_ADM_MPIO_GRAFICO.shp")

poligonos_mpios@data$id <- poligonos_mpios@data$MPIO_CCDGO

poligonos_Bta@data$id <- paste0("11", poligonos_Bta@data$MPIO_CCDGO)

shpBta <- fortify(poligonos_Bta, region="id")

shp = fortify(poligonos_mpios, region="id")

shp <- rbind(shp, shpBta)

shp$id <- as.numeric(shp$id)

shp <- shp %>% left_join(Basetotal[, c("DPTOMPIO","Y_SFH")], by = c("id" = "DPTOMPIO"))

options(scipen = 999999)

Plot <- ggplot(shp) + 
  aes(long,lat,group=group,fill=Y_SFH) + 
  geom_polygon() +
  geom_path(color="black") + labs(fill = "Tasa Desempleo") + theme_bw() +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

Plot
