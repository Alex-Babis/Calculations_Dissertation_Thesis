library(ggplot2)
library(lubridate)
library(tidyverse)
#library(mvtnorm) 
library(Rsolnp)
library(tidyr)
library(mvnfast)
library(plotly)
library(reshape2)
library(DEoptim)
library(MASS)
library(Hmisc)
library(dplyr)
library(gridExtra)

source("Data_libraries/00_ginv_func.r")

# read data
#ger_yield <- read.csv("Data_libraries/ger_yields.csv", sep = ',', header = TRUE, row.names = 1)
#
ger_yield <- read.csv("Data_libraries/ger_yields2.csv", sep = ',', header = TRUE, row.names = 1)

all_dates <- ger_yield$Date
# start o 2023 to mid 2024
# pred_ger_yield <- ger_yield[ger_yield$Date >= "2023-01-01",]


yield_dates <- ger_yield[ger_yield$Date < "2023-01-01",1]

pred_ger_yield_w <-  as.matrix(ger_yield[ger_yield$Date >= "2023-01-01",-1])


pred_ger_yield <- as.matrix(ger_yield[,-1])

head(yield_dates)
 
ger_yield <- as.matrix(ger_yield[ger_yield$Date < "2023-01-01",-1])

mean_yield <- apply(ger_yield,2,mean)

set.seed(12345)

# 10% data will be missing
ind_del <- sample(1:(nrow(ger_yield)*ncol(ger_yield)),floor((nrow(ger_yield)*ncol(ger_yield))/10),replace = FALSE)
ger_yield_w <- ger_yield
ger_yield_w[ind_del] <- NA


load( file = "Nelson_Siegel_Svensson_Bliss/Output_data/start_BL_L.R")
load( file = "Nelson_Siegel_Svensson_Bliss/Output_data/start_NSS_L.R")
load( file = "Nelson_Siegel_Svensson_Bliss/Output_data/start_NS_L.R")

source("Nelson_Siegel_Svensson_Bliss/03_Score_norm.r")
source("Nelson_Siegel_Svensson_Bliss/03_Score_t.r")
source("Nelson_Siegel_Svensson_Bliss/GAS_Process.r")

# load NS starting values for beta and lambda
#load(file = "Data_libraries/Start_NS.RData")

# Load NSS starting values for betas and lambdas with delta_l = 0.5
#load(file ="Data_libraries/Start_NSS.R")

#load(file ="Data_libraries/Start_NSS_0.R")


