library(ggplot2)    # Graphs
library(magic)      # Blockdiagonal matrix
library(Matrix)     # Blockdiagonal matrices
library(bdsmatrix)  # Quadratic forms using cholesky
library(MASS)
library(lubridate)
library(urca)
library(mvnfast)
library(rlist)
library(extraDistr)
library(Rsolnp)
library(DEoptim)
library(Rfast)
library(tidyr)
library(dplyr)
library(corrplot)
library(weights)
library(psych)
library(tseries)
library(FinTS)
library(Hmisc)
library(knitr)
library(quantmod)
library(readxl)
library(ggforce)

# loading functions
source("Libraries_data/00_ginv_func.r")
source("Libraries_data/03_Data_EU.r")
#source("Libraries_data/03_Data_new.r")

source("GAS_CHOLEKSY/00_Entropy_functions.r")
source("GAS_CHOLEKSY/M_GAS.r")
source("GAS_CHOLEKSY/M_norm.r")
source("GAS_CHOLEKSY/M_t.r")
source("GAS_CHOLEKSY/U_GAS.r")
source("GAS_CHOLEKSY/U_norm.r")
source("GAS_CHOLEKSY/U_t.r")





# source("GAS_codes/04_MTVGAUSSIAN.r")
# source("GAS_codes/04_MTVT2.r")
# source("GAS_codes/04_UNIGAUSSIAN2.r")
# source("GAS_codes/04_UNIT2.r")
# source("GAS_codes/05_GAS_M.r")
# source("GAS_codes/05_GAS_U.r")
# source("Fit_models/00_Entropy_functions.r")

#source("05_GAS_cMVN.r")
#source("05_GAS_cMVT.r")
#source("05_GAS_cUVN.r")
#source("05_GAS_cUVT.r")

# loading data
load(file = "Libraries_data/Data/Start_par.R")


# library(doSNOW)

# pbSapply <- function(cl, X, FUN, ...) {
#   registerDoSNOW(cl)
#   pb <- txtProgressBar(max=length(X), style = 3)
#   on.exit(close(pb))
#   progress <- function(n) setTxtProgressBar(pb, n)
#   opts <- list(progress=progress)
#   foreach(i=X, .options.snow=opts) %dopar% {
#     FUN(i, ...)
#   }
# }


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# clusterEvalQ(cl = cl, library("mvnfast"))
# clusterEvalQ(cl = cl, library("Rsolnp"))
# clusterEvalQ(cl = cl, library("dplyr"))

# pop <- opt_bank2y$member$pop

# clusterExport(cl = cl, list = c("returns","grad_norm_M","Start_par","ginv_2","gas_M",
#                                 "de_func","solnp","pop","last"))


# opt <- pbSapply(cl = cl, X= 1:180, FUN = function(x){
#     a_new <- solnp(pars = pop[x,] , 
#              fun = de_func, 
#              LB = c(rep(0,9),rep(-1,9)), 
#              UB = c(rep(Inf,9), rep(1,9)))
#     print(last(a_new$values))
#     return(c(a_new$pars,last(a_new$values)))

    
# })



# save(opt, file = "Libraries_data/Data/try_opt.R")

# new_pop <- c()
# hodnota <- c()
# for( i in 1:180){
#     hodnota <- c( hodnota, opt[[i]][19])
#     new_pop <- rbind(new_pop,opt[[i]][-19] )
# }
# ind <- which.min(hodnota)
