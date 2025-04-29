# refiting models where forecasted sigma has been NaN

# looping through all models and distribution "ghyp"
distrib <- c("norm", "snorm", "std","sstd") # "norm", "snorm", "std","sstd""ged","sged","jsu","nig",
garch.models <- data.frame( model = c("sGARCH","gjrGARCH","eGARCH",
                                      "iGARCH","fiGARCH",
                                      "csGARCH","fGARCH"),
                            submodels = I(list(NULL, NULL, NULL,
                                               NULL,NULL,
                                               NULL, "TGARCH")))


for(i in 1:length(distrib)) #1:length(distrib)
{
  
  garch.var.list <- list()
  
  for( j in 1:nrow(garch.models))#1:nrow(garch.models)
  {
    print(paste0("*****",garch.models[[j,1]],
                 c("_")[!is.null(garch.models[[j,2]])],"******"))
    
    # load data for specific model 
    load.file <- paste0("data/",garch.models[[j,1]],
                        c("_")[!is.null(garch.models[[j,2]])],
                        garch.models[[j,2]],"_spec_",distrib[i],"_504.Rdata")
    
    load(load.file)
    
    # run VaR testing 
    for( mods in 1:length(garches))
    {

      # table with forecast
      tab.stock <- garches[[mods]]

      if(is.null(dim(tab.stock)))
      {
        next
      }
      # which sigma is nan
      nan.sigma <- is.nan(tab.stock[,"Sigma_t_1"]) | (tab.stock[,"Sigma_t_1"] == 0)

      if( sum(nan.sigma) > 0 )
      {
        # cycle to refit models
        for( cnt in which(nan.sigma))
        {

          
          spec <- ugarchspec( variance.model = list(model = garch.models[[j,1]],
                                                    garchOrder  = c(1,1),
                                                    submodel = garch.models[[j,2]]),
                              mean.model = list(armaOrder = c(0,0),
                                                include.mean = TRUE,
                                                archm = FALSE),
                              distribution.model = distrib[i])
          
          # which data to be fit 
          ind <- tab.stock[cnt,1]
          
          fit <-  ugarchfit(spec = spec, data = stocks.lr[(ind - 504 + 1):ind,mods],
                              fit.control = list(trunclag = 504), solver = "hybrid")

          forecast <- ugarchforecast(fit, n.ahead = 1)

          while(is.nan(forecast@forecast$sigmaFor[1]) | forecast@forecast$sigmaFor[1] == 0 )
          {
            tryCatch({
              fit <<-  ugarchfit(spec = spec, data = stocks.lr[(ind - 504 + 1):ind,mods],
                                fit.control = list(trunclag = 504), solver = "gosolnp")
              
              forecast <<- ugarchforecast(fit, n.ahead = 1)},
              error=function(e){ forecast@forecast$sigmaFor[1] <<- NaN })
          }
          
          
          out.new <- c(unlist(fit@fit$coef),forecast@forecast$seriesFor[1],
                            forecast@forecast$sigmaFor[1])
          
          tab.stock[cnt,2:ncol(tab.stock)] <- out.new
        }
        
        garches[[mods]] <- tab.stock
        
      }else
      {
        next
      }
      
    }
    
    # output corrected tables without NaN
    out.file <-  paste0("data/",garch.models[[j,1]],
                        c("_")[!is.null(garch.models[[j,2]])],
                        garch.models[[j,2]],"_spec_",distrib[i],"_504_corrected.Rdata")
    
    # output data
    save(garches, file = out.file)
    gc()
    
  }
  
}

