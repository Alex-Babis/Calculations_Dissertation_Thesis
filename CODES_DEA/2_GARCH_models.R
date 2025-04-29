# fitting GARCH models from 
# sourcing libraries and setdw
source("C:/Users/alexb/Desktop/DEA_TS_MODELS/codes/0_libraries.R")

# loading data about stock returns
load("data/stocks_lr.RData")


# fitting GARCH models from rugarch package
fit.garch <- function( data = NULL,
                       family = NULL,
                       garchorder = c(0,0),
                       armaorder = c(0,0),
                       submodel = NULL,
                       distribution = "norm",
                       param = NULL,
                       trunclag = 1000)
{
  # specify garch model 
  spec <- ugarchspec( variance.model = list(model = family,
                                            garchOrder  = garchorder,
                                            submodel = submodel),
                      mean.model = list(armaOrder = armaorder,
                                        include.mean = TRUE,
                                        archm = FALSE),
                      start.pars = param,
                      distribution.model = distribution)

  solvers.garch <- c("solnp","nlminb","lbfgs","nloptr","nloptr")
  solver <- list(NULL,NULL,NULL,10,9)
  
  for (i in 1:length(solvers.garch))
  {
    skip_to_next <- FALSE
    all.good <- FALSE
    
    tryCatch({
      all.good <- TRUE
      fit <- ugarchfit(spec = spec, data = data,
                solver = solvers.garch[i],
                solver.control = list(solver = solver[[i]]),
                fit.control = list(trunclag = trunclag))},
      error=function(e){ skip_to_next <<- TRUE })

    if(skip_to_next)
    {
      next
    }
    if(!is.null(fit@fit$coef) & all.good)
    {
      break
    }
    
    gc()
  }
  
  # If none of them converge -> try to fit it without 
  # starting parameters
  if(is.null(fit@fit$coef))
  {
    # specify garch model 
    spec <- ugarchspec( variance.model = list(model = family,
                                              garchOrder  = garchorder,
                                              submodel = submodel),
                        mean.model = list(armaOrder = armaorder,
                                          include.mean = TRUE,
                                          archm = FALSE),
                        start.pars = NULL,
                        distribution.model = distribution)
    
    for (i in 1:length(solvers.garch))
    {
      skip_to_next <- FALSE
      all.good <- FALSE
      
      tryCatch({
        all.good <- TRUE
        fit <- ugarchfit(spec = spec, data = data,
                         solver = solvers.garch[i],
                         solver.control = list(solver = solver[[i]]),
                         fit.control = list(trunclag = trunclag))},
        error=function(e){ skip_to_next <<- TRUE })
      
      if(skip_to_next)
      {
        next
      }
      if(!is.null(fit@fit$coef) & all.good )
      {
        break
      }
      gc()
    }
  }
  
  # forecasting series and standarnd deviation
  forecast <- ugarchforecast(fit, n.ahead = 1)

  out.garch <- list(fit@fit$coef,forecast@forecast$seriesFor[1],
                 forecast@forecast$sigmaFor[1])
  
  names(out.garch) <- c("","Mean_t_1","Sigma_t_1")
  
  # return
  return(out.garch)
}


# iterating through rollingwindow

rw.garch <- function(data = NULL,
                     family = "sGARCH",
                     garchorder = c(1,1),
                     armaorder = c(0,0),
                     submodel = NULL,
                     distribution = "norm",
                     wnd = 504)
{
  
  # fitting model for all data to get starting param for 
  # models on subdata
  
  fit <- fit.garch(data = data,
                   family = family,
                   garchorder = garchorder,
                   armaorder = armaorder,
                   submodel = submodel,
                   param = NULL,
                   distribution = distribution,
                   trunclag = wnd)
  
  # setting new param for opt
  param.all <- as.list(fit[[1]])
  
  # lenght of data
  stck.days <- length(data)
  
  # fitting model and calculating value at risk
  
  # creating table for storing  params
  rw.garch <-  c()
  
  fit.params <- c()
  
  # progress bar
  #pb <- txtProgressBar(min = 0, max = (stck.days - wnd + 1), style=3)
  
  param <- param.all
  
  for( days.garch in 1:(stck.days - wnd + 1))
  {

    fit <- fit.garch(data = data[days.garch:(days.garch + wnd - 1)],
                     family = family,
                     garchorder = garchorder,
                     armaorder = armaorder,
                     submodel = submodel,
                     #param = param,
                     distribution = distribution,
                     trunclag = wnd)
    
    # storing params with index of last day of data
    fit.params <- c((days.garch + wnd - 1), unlist(fit))
    rw.garch <- rbind(rw.garch,fit.params)
    rownames(rw.garch) <- 1:nrow(rw.garch)
    
    # moving average params
    ma.param <- rbind(unlist(param.all),
                      rw.garch[max(1,days.garch - 3):days.garch,
                          -c( 1 ,(ncol(rw.garch) - 1 ), ncol(rw.garch) )])
    if(days.garch == 1)
    {
      param <- ma.param
    }else
    {
      param <- as.list(apply(ma.param, 2, mean))
    }

    
    #param <- as.list(fit[[1]])
    
    # progress bar
    #setTxtProgressBar(pb, days.garch)

  }
  
  # setting col and row names
  colnames(rw.garch)[1] <- "Index_last_day"

  # output
  return(rw.garch)
}

# model selection 
#garch.models <- data.frame( model = c("apARCH",
#                                      "fGARCH",
#                                      "fGARCH",
#                                      "fGARCH",
#                                      "fGARCH"),
#                            submodels = I(list(NULL,
#                                              "AVGARCH", 
#                                              "NGARCH",
#                                              "NAGARCH",
#                                              "ALLGARCH")))
# model selection 
garch.models <- data.frame( model = c("sGARCH","gjrGARCH","eGARCH",
                                      "iGARCH",
                                      "csGARCH","fGARCH"),
                            submodels = I(list(NULL, NULL, NULL,
                                                NULL,
                                               NULL, "TGARCH")))

for( model.num in 1:nrow(garch.models))
{
  
  garches  <- list()
  
  pb <- txtProgressBar(min = 0, max = ncol(stocks.lr), style=3)
  
  for(stock.num in 1:ncol(stocks.lr))
  {
    skip_to_next <- FALSE
    # calculating value at risk for each stock 
    tryCatch({
      garches[[stock.num]] <- rw.garch(data = stocks.lr[,stock.num],
                                       family = garch.models[[model.num,1]],
                                       garchorder = c(1,1),
                                       armaorder = c(0,0),
                                       submodel = garch.models[[model.num,2]],
                                       distribution = "ghyp",
                                       wnd = 504)},
      error=function(e){ skip_to_next <<- TRUE })
    
    if(skip_to_next)
    {
      garches[[stock.num]] <- "..."
    }
    
    if(skip_to_next)
    {
      next
    }
    # update progress
    setTxtProgressBar(pb, stock.num)
  }
  
  # output file 
  output.file <- paste0("data/",garch.models[[model.num,1]],
                        c("_")[!is.null(garch.models[[model.num,2]])],
                        garch.models[[model.num,2]],"_spec_ghyp_504.Rdata")
  
  # output data
  save(garches, file = output.file)
  gc()
}


# "norm", already run"ged","sged","jsu","nig",
distrib <- c("ghyp")
for (j in distrib)
{
  ##### SPECIAL FIT FOR FIGARCH
  garches  <- list()
  
  pb <- txtProgressBar(min = 0, max = ncol(stocks.lr), style=3)
  
  for(stock.num in 1:ncol(stocks.lr))
  {
    skip_to_next <- FALSE
    # calculating value at risk for each stock 
    tryCatch({
      garches[[stock.num]] <- rw.garch(data = stocks.lr[,stock.num],
                                       family = "fiGARCH",
                                       garchorder = c(1,1),
                                       armaorder = c(0,0),
                                       submodel = NULL,
                                       distribution = j,
                                       wnd = 504)},
      error=function(e){ skip_to_next <<- TRUE })
    
    if(skip_to_next)
    {
      garches[[stock.num]] <- "..."
    }
    
    if(skip_to_next)
    {
      next
    }
    # update progress
    setTxtProgressBar(pb, stock.num)
  }
  
  # output file 
  output.file <- paste0("data/fiGARCH_spec_",j,"_504.Rdata")
  
  # output data
  save(garches, file = output.file)
  gc()
  
}
