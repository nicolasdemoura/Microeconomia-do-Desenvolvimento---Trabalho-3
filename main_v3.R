###############################################################################
# Topic: Microeconomia do Desenvolvimento - Trabalho 1
# Instructor: Amanda Arabage
# Course: Microeconomia do Desenvolvimento
# Autor: Daniel Colli, Diogo Leite, Michel Finger, Nícolas de Moura
# Goal: Criar Curvas de Lorenz, coeficiente de Gini e índice de pobreza para analisar a desigualdade e pobreza 
###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ipumsr", "ggplot2", "splines", "stargazer", "Hmisc", "AER","readxl", "tidyverse", "data.table", "stargazer", "lubridate", "fixest", "ggplot2", "pracma", "dplyr", "remotes", "tidyr", "mvProbit", "ipw", "MASS", "xtable", "quantreg", "nprobust", "chron", "WDI", "utils","terra","haven","readr","dplyr","readxl","writexl","stargazer","modelsummary","estimatr","did","plm","lmtest", "progress")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
gc()

# Set the random seed for reproducibility
set.seed(20250429)

###############################################################################
# Create the map
###############################################################################

saopaulo <- svc("map/35MIE250GC_SIR.shp")
saopaulo <- vect(saopaulo)

colors <- c("#294292","#CCCCCC","#212125","#C40008","#FFEF00")

# Join the data set with the map data
names(saopaulo)[1] <- "MCRR"
saopaulo <- saopaulo[order(saopaulo$MCRR), ]

# Plot the map with the MCRR variable
png("Figures/microrregiao.png", width = 800, height = 600)
plot(saopaulo, "MCRR", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
dev.off()

# Remove the São Paulo microregion from the map
saopaulo <- saopaulo[saopaulo$MCRR != "SÃO PAULO", ]

###############################################################################
# Load the data
###############################################################################

##################
# Microregion data
##################
# Load the microregion data 
raw_data <- read_excel("data/microrregiao.xls")
raw_data <- as.data.frame(raw_data)

# Select the variables 
raw_data <- raw_data[, c(1,2,5,6,8)]
colnames(raw_data) <- c("COD_UF", "UF", "COD_MCRR", "MCRR", "COD_MUN")
raw_data$COD_UF <- as.numeric(raw_data$COD_UF)
raw_data$COD_MCRR <- as.numeric(raw_data$COD_MCRR)
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)

# Store the data in a new variable
temp_data <- raw_data

##################
# Population data
##################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/populacao.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_POP")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_POP <- as.numeric(raw_data$NUM_POP)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

##################
# Income data
##################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/PIB_total.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_GDP")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_GDP <- as.numeric(raw_data$NUM_GDP)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/PIB_industria.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_GDP_IND")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_GDP_IND <- as.numeric(raw_data$NUM_GDP_IND)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/PIB_servicos.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_GDP_SER")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_GDP_SER <- as.numeric(raw_data$NUM_GDP_SER)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

##################
# Labor data
##################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/ocupacao.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "PER_DESOCC")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$PER_DESOCC <- as.numeric(raw_data$PER_DESOCC)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

##################
# Urban data
##################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/urbano.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_URBAN")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_URBAN <- as.numeric(raw_data$NUM_URBAN)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

#################
# PBF data
#################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/pbf.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_PBF")

raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_PBF <- as.numeric(raw_data$NUM_PBF)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

#################
# Inequality data
#################

# Load the population data from a csv ignoring the first row
raw_data <- read.csv("data/gini.csv", skip = 1, sep = ",", encoding = "UTF-8")
raw_data <- as.data.frame(raw_data)

# Select the variables
raw_data <- raw_data[, c(2,4)]
colnames(raw_data) <- c("COD_MUN", "NUM_GINI")
raw_data$COD_MUN <- as.numeric(raw_data$COD_MUN)
raw_data$NUM_GINI <- as.numeric(raw_data$NUM_GINI)

temp_data <- left_join(temp_data, raw_data, by = "COD_MUN")

#################
# Create the dataset
#################

# Filter the data set to only include the municipalities in the state of São Paulo
temp_data <- temp_data %>% filter(COD_UF == 35)
temp_data <- temp_data[order(temp_data$MCRR), ]

# Create the aggregated data set
temp_data <- temp_data %>% group_by(MCRR) %>%
                              mutate(MCRR = first(MCRR),
                                        PC_GDP = sum(NUM_GDP) / sum(NUM_POP),
                                        PER_IND = sum(NUM_GDP_IND) / sum(NUM_GDP),
                                        PER_SER = sum(NUM_GDP_SER) / sum(NUM_GDP),
                                        PER_DESOCC = sum(PER_DESOCC * NUM_POP) / sum(NUM_POP),
                                        PER_URBAN = sum(NUM_URBAN) / sum(NUM_POP),
                                        PER_PBF = sum(NUM_PBF) / sum(NUM_POP),
                                        NUM_GINI = sum(NUM_GINI * NUM_POP) / sum(NUM_POP),
                                        NUM_POP = sum(NUM_POP),
                                        NUM_GDP = sum(NUM_GDP),
                                        NUM_GDP_IND = sum(NUM_GDP_IND),
                                        NUM_GDP_SER = sum(NUM_GDP_SER),
                                        NUM_URBAN = sum(NUM_URBAN),
                                        NUM_PBF = sum(NUM_PBF)) 
# Remove the São Paulo microregion
temp_data <- temp_data %>% filter(COD_MCRR != 61)

##########################################################################
# Randomize the treatment variable
##########################################################################

# Randomize the treatment variable based on the median of the PER_URBAN variable 
# Make sure that for each group of PER_URBAN, 50% of the observations are treated and 50% are not treated

temp_data <- temp_data %>% group_by(PER_URBAN > median(PER_URBAN)) %>%
                           group_by(NUM_POP > median(NUM_POP)) %>% 
                              mutate(RAND = runif(n()),
                                     TREAT = ifelse(RAND > median(RAND), 1, 0)) %>%
                              ungroup()

# Incorporate the treatment variable into the map
saopaulo$TREAT <- temp_data$TREAT

# Plot the map with the TREAT variable
png("Figures/treatment_assignment.png", width = 800, height = 600)
plot(saopaulo, "TREAT", col = colors, axes = FALSE, legend = TRUE, border = "#FFFFFF")
dev.off()

##########################################################################
# Balance check
##########################################################################

# Create variables for the balance check
variables <- c("NUM_POP", "PC_GDP", "PER_IND", "PER_SER", "PER_DESOCC", "PER_URBAN", "PER_PBF", "NUM_GINI")
variables_names <- c("População", "PIB per capita", "% Indústria", "% Serviços", "% Desocupação", "% Urbano", "% PBF", "Gini")

# Create a data frame to store the balance check results
table_balance <- data.frame("Variável" = variables_names)

# Loop through the variables and calculate the mean for the control group,
# the difference between the treatment and control groups, and the p-value
for(i in 1:length(variables)) {
  var <- variables[i]
  mean_control <- mean(temp_data[[var]][temp_data$TREAT == 0])
  mean_treat <- mean(temp_data[[var]][temp_data$TREAT == 1])
  diff <- mean_treat - mean_control
  p_value <- t.test(temp_data[[var]][temp_data$TREAT == 0], 
                    temp_data[[var]][temp_data$TREAT == 1])$p.value
  
  table_balance[i, "Média Controle"] <- mean_control
  table_balance[i, "Diferença"] <- diff
  table_balance[i, "p-valor"] <- p_value
}
colnames(table_balance) <- c("Variável", "Média Controle", "Diferença", "p-valor")
stargazer(table_balance, type = "latex", summary = FALSE, title = "Tabela de Balanceamento", digits = 2, out = "Tables/balance_check.tex", label = "tab:balance_check", align = TRUE, font.size = "small", table.layout = "lcccc", rownames=FALSE)
