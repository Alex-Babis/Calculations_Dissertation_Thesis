Lopez.loss <- function(sim.data = NULL, var = NULL, loss.func = "RQL", alpha = 0.05){
  # Lopez quadratic -- RQL
  # Linear -- RL
  # Quadratic -- RQ
  # Caporin_1 -- RC_1
  # Caporin_2 -- RC_2
  
  # hit series
  hit <- sim.data < var
  
  if( loss.func == "RQL") 
  {
    Loss.ts <- 1 + (var - sim.data)^2
    
    #Loss.ts[!hit] <- 0
    Loss.ts <- Loss.ts[hit]
    
  } else if( loss.func == "RL") {
    
    Loss.ts <- (var - sim.data)
    
    #Loss.ts[!hit] <- 0
    Loss.ts <- Loss.ts[hit]
    
  } else if( loss.func == "RQ") {
    
    Loss.ts <- (var - sim.data)^2
    
    #Loss.ts[!hit] <- 0
    Loss.ts <- Loss.ts[hit]
    
  } else if( loss.func == "RC_1") {
    
    Loss.ts <- abs( 1 - abs(sim.data/var))
    
    #Loss.ts[!hit] <- 0
    #Loss.ts <- Loss.ts[hit]
    Loss.ts[hit] <- Loss.ts[hit]*(1-alpha)
    Loss.ts[!hit] <- Loss.ts[!hit]*(alpha)
    
  } else if( loss.func == "RC_2") {
    
    Loss.ts <- (abs(sim.data) - abs(var))^2/abs(var)
    #Loss.ts[!hit] <- 0
    #Loss.ts <- Loss.ts[hit]
    Loss.ts[hit] <- Loss.ts[hit]*(1-alpha)
    Loss.ts[!hit] <- Loss.ts[!hit]*(alpha)
    
  } else {
    
    stop("unspecified loss.func")
    
  }
  
  avg.loss <- sum(Loss.ts)
  
  return(avg.loss)
}


# computing value at risk for specific alpha 
VaR_aplpha <- function(data = NULL,
                       distribution = NULL,
                       alpha = NULL,
                       short = FALSE){
  # find which parameter from lambda skew and shape do exist in data
  param <- colnames(data)[colnames(data) %in% c("lambda","shape", "skew")]

  # create list with parameter available
  params <- lapply(param, function(i) data[,i])
  
  # name elements of list according to values
  names(params) <- param
  
  if(short)
  {
    alpha <- 1 - alpha
  }
  # create list of all parameters
  params <- c(list(distribution = distribution,
                   p = alpha,
                   mu = data[,"Mean_t_1"],
                   sigma = data[, "Sigma_t_1"]),params)

  # calculate VaR for specific alpha
  ret.var <- do.call(qdist,params)
  
  if( short){
    ret.var <-  -ret.var
  }
  # return value at risk 
  # 
  return(ret.var)
  
}

#VaR_aplpha(data = garches[[26]], distribution = "sstd", alpha = 0.01, short = TRUE)
# data = table with time-varying parameters
#b <- VaR_aplpha(data = garches[[1]], distribution = "sstd", alpha = 0.05, short = TRUE)


# back testing of value at risk 
VaR_bt <- function(data = NULL,
                   data.orig = NULL,
                   name_stock = NULL,
                   distribution = NULL,
                   alpha = NULL, 
                   short = FALSE){
  # initiate var data
  VaR.data <- c()
  
  # calculation of VaR
  for( i in 1:length(alpha))
  {
    VaR.data <- cbind(VaR.data, VaR_aplpha(data = data,
                                           distribution = distribution,
                                           alpha = alpha[i],
                                           short = short))
  }
  
  # colnames
  colnames(VaR.data) <- c(paste0("var_",alpha*100))
  
  # last row of VaR not used for testing obviously 
  VaR.data <- VaR.data[-nrow(VaR.data),]

  if(short)
  {
    data.orig <- -data.orig
  }
  
  back.test.var <- c()
  
  # backtesting
  for( i in 1:length(alpha))
  {
    # backtesting 
    back.var <- BacktestVaR(data = data.orig, VaR = VaR.data[,i],
                            alpha = alpha[i], Lags = 4)
    
    back.var$Loss$LossSeries <- NULL
    
    back.var$Loss$RC1 <- Lopez.loss(sim.data = data.orig,var = VaR.data[,i],
                                     loss.func = "RC_1")
    
    back.var$Loss$RC2 <- Lopez.loss(sim.data = data.orig,var = VaR.data[,i],
                                     loss.func = "RC_2")
    
    back.var$Loss$RL.rel <- Lopez.loss(sim.data = 
                                         (data.orig - data[-nrow(data),"Mean_t_1"])/(data[-nrow(data),"Sigma_t_1"]),
                                       var = 
                                         (VaR.data[,i] - data[-nrow(data),"Mean_t_1"])/(data[-nrow(data),"Sigma_t_1"]),
                                    loss.func = "RL")
    
    back.var$Loss$RQ.rel <- Lopez.loss(sim.data = 
                                         (data.orig - data[-nrow(data),"Mean_t_1"])/(data[-nrow(data),"Sigma_t_1"]),
                                       var = 
                                         (VaR.data[,i] - data[-nrow(data),"Mean_t_1"])/(data[-nrow(data),"Sigma_t_1"]),
                                    loss.func = "RQ")
    
    tests.var <- unlist(back.var)
    
    # names 
    name.var <- names(tests.var)
    
    # independence test
    ind.test <- tests.var["LRcc.Test"] - tests.var["LRuc.Test"]
    
    # p value 
    ind.pvalue <- pchisq(ind.test, df = 1, lower.tail = FALSE)
    
    # exceedence in percent
    ex.var <- sum(data.orig < VaR.data[,i])/length(data.orig)
    
    # absolute deviance from level alpha
    abs.ex.var <- abs(ex.var - alpha[i])
    
    # all test statistics and p values
    tests.var <- c(tests.var,ind.test,ind.pvalue,abs.ex.var*100)
    
    names(tests.var) <- c(name.var,"Ind.Test","Ind.Pvalue","Abs.exceedance")
    
    # all levels together
    back.test.var <- rbind(back.test.var,
                           tests.var)
  }
  
  # changing rownames
  rownames(back.test.var) <- c(paste0(name_stock,"_var_",alpha*100))
  
  # output values
  return(back.test.var)
}

# testing function
#a <- VaR_bt(data = garches[[1]],
#       data.orig = stocks.lr[505:nrow(stocks.lr),1],
#       name_stock = colnames(stocks.lr)[1],
#       alpha = c(0.1,0.05,0.01),
#       distribution = "sstd",
#       short = TRUE)



distrib <- c("norm", "snorm", "std","sstd")
garch.models <- data.frame( model = c("sGARCH","gjrGARCH","eGARCH",
                                      "iGARCH","fiGARCH",
                                      "csGARCH","fGARCH"),
                            submodels = I(list(NULL, NULL, NULL,
                                               NULL,NULL,
                                               NULL, "TGARCH")))

for(i in 1:length(distrib))
{
  
  garch.var.list <- list()
  
  for( j in 1:nrow(garch.models))
  {
    # load data for specific model 
    load.file <- paste0("data/",garch.models[[j,1]],
                          c("_")[!is.null(garch.models[[j,2]])],
                          garch.models[[j,2]],"_spec_",distrib[i],"_504_corrected.Rdata")
    
    load(load.file)
    # data loaded into garches file
    garch.tab <- c()
    
    # run VaR testing 
    for( mods in 1:length(garches))
    {
      garch.var <- VaR_bt(data = garches[[mods]],
                          data.orig = stocks.lr[505:nrow(stocks.lr),mods],
                          name_stock = colnames(stocks.lr)[mods],
                          alpha = c(0.1,0.05,0.025,0.01),
                          distribution = distrib[i],
                          short = TRUE)
     
      garch.tab <- rbind(garch.tab,garch.var)
       
      
    }
    garch.var.list[[j]] <- garch.tab
    names(garch.var.list)[j] <- paste0(garch.models[[j,1]],
                                       c("_")[!is.null(garch.models[[j,2]])],
                                       garch.models[[j,2]])
  }
  
  # output for each distribution
  out.file <- paste0("data/VaR",distrib[i],"_504_short.Rdata")
  
  # output data
  save(garch.var.list, file = out.file)
  gc()
  
}


