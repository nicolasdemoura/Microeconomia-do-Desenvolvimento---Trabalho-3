rm(list=ls())
load.lib <- c("ggplot2","utils","terra","haven","readr","dplyr","readxl","writexl","stargazer","modelsummary","estimatr","did","plm","lmtest")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

brazil <- svc("map_brazil.shp")
brazil <- vect(brazil)

# Filter municipalities from brazil, a SpatVector from terra, where SIGLA_UF == "PI" 

piaui <- terra::subset(brazil, brazil$SIGLA_UF == "PI" | brazil$SIGLA_UF == "MA")

plot(piaui)
municipios_piaui <- read_excel("municipios piaui.xlsx")
colnames(municipios_piaui) <- "CD_MUN"

municipios_piaui$CD_MUN <- as.character(municipios_piaui$CD_MUN)
municipios_piaui$tratamento <- 1

piaui$tratamento <- ifelse(piaui$CD_MUN %in% municipios_piaui$CD_MUN, 1, 0
plot(piaui, col = piaui$tratamento + 1)
