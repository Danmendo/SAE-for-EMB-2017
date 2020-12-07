install.packages(c("lmtest","openxlsx", "dplyr", "stringi", "stringr", "FactoMineR", "tidyr", "sae"))

install.packages(c("lmtest", "openxlsx", "stringi", "stringr", "FactoMineR", "tidyr"))

install.packages("xtable")

rm(list = ls())

library(stargazer)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(FactoMineR)
library(sae)
library(lmtest)
library(rgdal)
library(RColorBrewer)
library(classInt)
library(spdep)
library(ggplot2)

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

#---- Pegado de las variable  ingreso con su respectiva varianza estimada y cve y depuración de la data --#

TerriData2000Cundi <- TerriData2000Cundi %>% filter(V8 != "")

TerriData2000Cundi <- TerriData2000Cundi %>% group_by(V3, V4, V7, V10) %>% filter(V11 == max(V11)) %>% ungroup()

TerriData2000Cundi <- TerriData2000Cundi %>% left_join(EstimacionesDirectas[, c("DPTOMPIO","IncomeMean", "EstVarIncMean")], by = c("V3" = "DPTOMPIO"))

TerriData2000Cundi$V8 <- as.character(TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- gsub("[.]", "", TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- gsub("[,]", ".", TerriData2000Cundi$V8)

TerriData2000Cundi$V8 <- as.numeric(TerriData2000Cundi$V8) 

#---- Cálculo de la correlación de las variables con el ingreso --------------------------------------------#

TerriData2000Cundi$V7 <- as.character(TerriData2000Cundi$V7)

#Eliminamos las variables referentes al rendimiento de diferentes productos

CorrelaIncome <- TerriData2000Cundi %>% group_by(V7, V10) %>% summarise(CorrInc = cor(V8, IncomeMean)) %>% 
  ungroup() %>% filter(!str_detect(V7, "Rendimiento"))

#Correlaciones mayores y menores a 0.5

CorrelaIncomeFilt <- CorrelaIncome %>% filter(abs(CorrInc) > 0.5)

#Escogemos el año mas cercano de las variables escogidas

CorrelaIncomeFilt <- CorrelaIncomeFilt %>% group_by(V7) %>% filter(CorrInc == max(CorrInc)) %>% ungroup() %>% 
  arrange(desc(abs(CorrInc)))

CorrelaIncomeFilt$V7

#Selección intuitiva de variables y ordenamiento en base a la correlación

CorrelaIncomeFilt <- CorrelaIncomeFilt[-c(21, 26, 27, 30, 31, 37, 40, 42:46, 48:50, 53, 55, 57, 58, 62:68),] %>%
  arrange(desc(abs(CorrInc)))

#Se escogen las 3 variables con la mayor correlación para ternerlas en cuenta en el modelo.

CorrelaIncomeFilt$V7

VariablesMayorCor <- CorrelaIncomeFilt[c(1:5, 16, 31),]

CorrelaIncomeFilt[c(2:4, 16, 31, 42), 2] <- 2017

CorrelaIncomeFilt$Filt <- paste(CorrelaIncomeFilt$V7, CorrelaIncomeFilt$V10, sep = "_")

#---- Selección de variables intuitivamente derivado de la correlación mas alta ----------------------------#

TerriData2000Cundi$Filt <- paste(TerriData2000Cundi$V7, TerriData2000Cundi$V10, sep = "_")

colnames(TerriData2000Cundi)

VariablesMayorCor$V7

IncomeMunic <- TerriData2000Cundi %>% dplyr::select(V3, V4, IncomeMean, EstVarIncMean) %>% 
  filter(!duplicated(V4)) %>% arrange(V4)

Income <- TerriData2000Cundi %>% filter(Filt %in% CorrelaIncomeFilt$Filt) %>% 
  dplyr::select(V3, V4, V7, V8)

Income <- Income[!duplicated(Income), ]

#---- Reestructuración de la base y estandarización de las variables ---------------------------------------#

SPR <- Income %>% spread(V7, V8)

rownames(SPR) <- SPR$V4

SPR <- SPR %>% dplyr::select(-V3, -V4) %>% as.data.frame()

#Escogemos las variables que tienen la mayor correlación y las quitamos de la base, posterior a ello
#se incluiran en el modelo

Variables <- SPR[, VariablesMayorCor$V7]

SPR <- SPR[, -which(colnames(SPR) %in% VariablesMayorCor$V7)]

SPR <- t(SPR) ; SPR <- SPR[complete.cases(SPR), ] ; SPR <- as.data.frame(t(SPR))

# Variables <- Variables %>% dplyr::select(-`Porcentaje - Sin figura de OSPR`,
#                                          -`Porcentaje - Área de la entidad territorial con figuras de OSPR`,
#                                          -`Inversión - Equipamiento`,
#                                          -`Tasa de hurtos (x cada 100.000 habitantes)`)


# SPR <- SPR %>% dplyr::select(-`Avalúo catastral rural`, 
#                              -`Movilización de recursos`, 
#                              -`Porcentaje de personas ocupadas formalmente con respecto a la población total`)

for (j in 1:ncol(SPR)){
  
  for (i in 1:nrow(SPR)){
    
    SPR[i, j] <- (SPR[i, j] - mean(SPR[, j]))/(sd(SPR[, j]))
    
  } 
  
  print(j)
  
}

#---- Componentes principales ------------------------------------------------------------------------------#

pca <- PCA(SPR, ncp = 10)

#----- Aplicación del modelo Fay Herriot --------------------------------------------------------------------#

DataModelIncome <- cbind(IncomeMunic[, c(1,3, 4)], Variables, pca$ind$coord)

Sisben <- read.xlsx("C:/Users/pc12345643/Desktop/Tesis - Daniel Mendoza - 2019/SisbenAprobados.xlsx")

colnames(Sisben)

Sisben <- Sisben %>% filter(Departamento %in% c("CUNDINAMARCA", "BOGOTÁ, D.C.")) %>% 
  group_by(Municipio) %>% filter(Corte.del.Sisbén == max(Corte.del.Sisbén))

DataModelIncome <- DataModelIncome %>% left_join(Sisben[, c("Código.Municipio", "Numero.de.personas.validadas")],
                                                 by = c("V3" = "Código.Municipio"))

DataModelIncome$`Avalúo catastral rural` <- DataModelIncome$`Avalúo catastral rural`/1000000

DataModelIncome$`Gastos totales per cápita` <- DataModelIncome$`Gastos totales per cápita`/1000000

DataModelIncome$PromPuntajesPruebas = apply(DataModelIncome[, c(6, 7)], 1, mean)

colnames(DataModelIncome)

modelocompleto <- paste0("IncomeMean", " ~ `", paste(colnames(DataModelIncome[, -c(1, 2, 3)]), collapse = "` + `"), "`")

modelocompleto <- as.formula(modelocompleto)

modelo_optimo <- step(lm(modelocompleto, data = DataModelIncome))

stargazer((lm(IncomeMean ~ `Avalúo catastral rural` + `Cobertura bruta en educación media` + 
                `Penetración de banda ancha` + `Indicador de desempeño fiscal` + 
                `Gastos totales per cápita` + Numero.de.personas.validadas + 
                PromPuntajesPruebas, 
              data = DataModelIncome)), title = "Resultados")


Model1 <- mseFH(IncomeMean ~ `Avalúo catastral rural` + PromPuntajesPruebas + `Gastos totales per cápita` + 
                  Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.8 + Numero.de.personas.validadas, data = DataModelIncome,
                vardir = EstVarIncMean)

Model1

Model2 <- mseFH(IncomeMean ~ `Avalúo catastral rural` + `Cobertura bruta en educación media` +  
                  `Penetración de banda ancha` + `Indicador de desempeño fiscal` + 
                  `Gastos totales per cápita` + Numero.de.personas.validadas + PromPuntajesPruebas, data = DataModelIncome,
                vardir = EstVarIncMean)


EstimacionesDirectasFH <- EstimacionesDirectas %>% dplyr::select(DPTOMPIO, NOMBMPIO, IncomeMean, 
                                                                 EstVarIncMean, CVIncMean) %>% 
  mutate(EBLUPMean = Model1$est$eblup, MSEEBLUP = Model1$mse, 
         CVMSEComp = 100*sqrt(Model1$mse)/Model1$est$eblup,
         EBLUPMean2 = Model2$est$eblup, MSEEBLUP2 = Model2$mse, 
         CVMSE = 100*sqrt(Model2$mse)/Model2$est$eblup)

#-------------------------------Comparación modelo componentes y variables-----------------------######

CVES <- EstimacionesDirectasFH[, c("NOMBMPIO", "CVMSEComp", "CVMSE")]

library(data.table)

colnames(CVES) <- c("NOMBMPIO", "FayHerriotComp", "Fay Herriot")

CVES <- CVES %>% as.data.table() %>% dplyr::arrange(desc(FayHerriotComp))

CVES$NOMBMPIO <- factor(CVES$NOMBMPIO, levels = CVES$NOMBMPIO[order(CVES$FayHerriotComp)])

CVES <- gather(CVES, "Estimadores", "CVE", -NOMBMPIO)

p <- ggplot(CVES, aes(CVE, NOMBMPIO))
p + geom_point(aes(colour = Estimadores), stat = 'identity') + coord_cartesian(xlim = c(2, 6.5))+
  labs(fill = "Estimadores") + xlab("Cve") + ylab("Municipios") + theme_bw()


write.xlsx(EstimacionesDirectasFH, file = "EstimacionesDirectasFHIncomeWBog.xlsx")

#---- Fay Herriot Espacial -----------------------------------------------------------------------------#

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
DataModelIncome <- arrange(DataModelIncome, V3)

DataModelIncome$IncomeMean2 <- DataModelIncome$IncomeMean/1000

DataModelIncome$EstVarIncMean2 <- DataModelIncome$EstVarIncMean/1000000

SFH <- mseSFH(IncomeMean2 ~ `Avalúo catastral rural` + PromPuntajesPruebas + `Gastos totales per cápita` + 
                Dim.2 + Dim.3 + Dim.4 + Dim.5 + Dim.8 + Numero.de.personas.validadas, vardir = EstVarIncMean2, proxmat = W_s, data = DataModelIncome)

SFH

####-----------------Comparación Fay Herriot y Fay Herriot Espacial--------------------------------####

EstimacionesDirectasFH$Y_SFH <- SFH$est$eblup*1000
EstimacionesDirectasFH$mseY_SFH <- SFH$mse*1000000
EstimacionesDirectasFH$cve_SFH <- sqrt(SFH$mse*1000000) / (SFH$est$eblup*1000) * 100

colnames(EstimacionesDirectasFH)

CVES <- EstimacionesDirectasFH[, c("NOMBMPIO", "CVIncMean","CVMSEComp", "cve_SFH")]

library(data.table)

colnames(CVES) <- c("NOMBMPIO", "Directo","FayHerriotComp", "FH Espacial")

CVES <- CVES %>% as.data.table() %>% dplyr::arrange(desc(Directo))

CVES$NOMBMPIO <- factor(CVES$NOMBMPIO, levels = CVES$NOMBMPIO[order(CVES$Directo)])

CVES <- gather(CVES, "Estimadores", "CVE", -NOMBMPIO)

p <- ggplot(CVES, aes(CVE, NOMBMPIO))
p + geom_point(aes(colour = Estimadores), stat = 'identity') + coord_cartesian(xlim = c(2, 6.5))+
  labs(fill = "Estimadores") + xlab("Cve") + ylab("Municipios") + theme_bw()

CompaCve <- data.frame(mean(EstimacionesDirectasFH$CVMSE), mean(EstimacionesDirectasFH$cve_SFH))

colnames(CompaCve) <- c("Fay Herriot", "Fay Herriot Espacial")

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

IncomeMunicPred <- TerriData2000CundiOM %>% dplyr::select(V3, V4) %>% 
  filter(!duplicated(V4)) %>% arrange(V4)

IncomePred <- TerriData2000CundiOM %>% filter(Filt %in% CorrelaIncomeFilt$Filt) %>% 
  dplyr::select(V3, V4, V7, V8)

IncomePred <- IncomePred[!duplicated(IncomePred), ]

SPRPred <- IncomePred %>% spread(V7, V8) %>% as.data.frame()

rownames(SPRPred) <- SPRPred$V4

SPRPred <- as.data.frame(SPRPred[-1, ])#Quitamos Cundianamarca

VariablesPred <- SPRPred %>% dplyr::select(colnames(Variables))

SPRPred <- SPRPred %>% dplyr::select(colnames(SPR)) %>% as.data.frame()



#SPRPred <- SPRPred[!complete.cases(SPRPred),]

# i <- 26
# 
# mod <- glm(SPRPred[complete.cases(SPRPred), i] ~., data = SPRPred[complete.cases(SPRPred), -i], family = Gamma(link = "log"))
# 
# summary(mod)
# 
# predict(mod, SPRPred[7,-i], type = "response")

# for (i in 1:ncol(SPRPred)){
# 
#   if (sum(is.na(SPRPred[, i])) != 0){
# 
#     mod <- lm(SPRPred[complete.cases(SPRPred), i] ~., data = SPRPred[complete.cases(SPRPred), -i])
# 
#     SPRPred[, i][is.na(SPRPred[, i])] <- predict(mod, SPRPred[is.na(SPRPred[, i]), -i])
# 
#   }
# 
# }

for (j in 1:ncol(SPRPred)){
  
  for (i in 1:nrow(SPRPred)){
    
    SPRPred[i, j] <- (SPRPred[i, j] - mean(SPRPred[, j]))/(sd(SPRPred[, j]))
    
  } 
  
  print(j)
  
}

summary(modelo_optimo)

pcaPred <- PCA(SPRPred, ncp = 10)

colnames(DataModelIncome)

DataModelIncomePredInS <- cbind(1, DataModelIncome[, c(4, 22, 10, 12:15, 18, 21)])

colnames(DataModelIncomePredInS)

row.names(DataModelIncomePredInS) <- row.names(Variables)

VariablesPred$PromPuntajesPruebas <- apply(VariablesPred[, c(3,4)], 1, mean)

VariablesPred <- VariablesPred %>% dplyr::select(`Avalúo catastral rural`, PromPuntajesPruebas,
                                                 `Gastos totales per cápita`)

DataModelIncomePred <- cbind(IncomeMunicPred[-17, 1], VariablesPred, pcaPred$ind$coord[, c(2:5, 8)])

colnames(DataModelIncomePred)

DataModelIncomePred$`Avalúo catastral rural` <- DataModelIncomePred$`Avalúo catastral rural`/1000000

DataModelIncomePred$`Gastos totales per cápita` <- DataModelIncomePred$`Gastos totales per cápita`/1000000

nam <- rownames(DataModelIncomePred)

DataModelIncomePred <- DataModelIncomePred %>% left_join(Sisben[, c("Código.Municipio", "Numero.de.personas.validadas")],
                                                         by = c("V3" = "Código.Municipio")) %>% as.data.frame()

DataModelIncomePred <- cbind(1, DataModelIncomePred[, -1])

rownames(DataModelIncomePred) <- nam

colnames(DataModelIncomePred)

Beta_income <- as.matrix(SFH$est$fit$estcoef[1]) 

eblup.FH_noA_income = as.numeric(as.matrix(DataModelIncomePred) %*% Beta_income)*1000

sigma2_u_income <- SFH$est$fit$refvar*1000000

V_income <- diag(sigma2_u_income + EstimacionesDirectasFH$EstVarIncMean)

V_Beta_income <- t(as.matrix(DataModelIncomePredInS)) %*% solve(V_income, tol = 1e-30) %*% as.matrix(DataModelIncomePredInS)

mse.FH_noA_income = diag(as.matrix(DataModelIncomePred) %*% solve(V_Beta_income, tol = 1e-30) %*% t(as.matrix(DataModelIncomePred)))

# cve out of sample
cv.FH_noA_income <- 100 * sqrt(mse.FH_noA_income) / eblup.FH_noA_income

colnames(EstimacionesDirectasFH)

outcome_Income_UnobservedAreas <- data.frame(DPTOMPIO = as.numeric(IncomeMunicPred[-17,]$V3), NOMBMPIO = names(mse.FH_noA_income),
                                             Y_SFH = eblup.FH_noA_income, 
                                             mseY_SFH = mse.FH_noA_income, cve_SFH = cv.FH_noA_income) 

write.xlsx(outcome_Income_UnobservedAreas, "UnobservedareasIncome.xlsx")

#---- Gráfica de los resultados del modelo ---------------------------------------------------------#

colnames(EstimacionesDirectasFH)

Basetotal <- bind_rows(EstimacionesDirectasFH[, c("DPTOMPIO", "NOMBMPIO", "Y_SFH", "mseY_SFH", "cve_SFH")],
                       outcome_Income_UnobservedAreas)

write.xlsx(Basetotal, "EstimacionesTotalesIncome.xlsx")

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

#install.packages("plotly")

colnames(shp) <- c("long", "lat", "order", "hole", "piece", "id", "group", "Income") 

Plot <- ggplot(shp) + 
  aes(long,lat,group=group,fill=Income) + 
  geom_polygon() +
  geom_path(color="black") + theme_bw()

Plot

