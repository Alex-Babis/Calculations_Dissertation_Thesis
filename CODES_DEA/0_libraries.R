# libraries and workplace 
# working directory 
#setwd("C:/Users/alexb/Desktop/DEA_TS_MODELS")

# libraries
library(quantmod) # importing data from yahoo.finance
library(rugarch)  # fitting GARCH models  
library(imputeTS) # interpolation of TS
library(stringr)  # string manipulation  
library(GAS)      # GAS models and value at risk backtesting
#library(topsis)   # TOPSIS
library(ggplot2)  # graph by Ggplot
library(CVXR)     # konvex optim
library(parallel) # parallel computing
library(MASS)

load("data/stocks_lr.RData")
load("data/stocks_km.RData")