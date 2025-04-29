
de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = FALSE)
        },
        error=function(cond) {
            return(.Machine$double.xmax)
        },
        warning=function(cond) {
            return(.Machine$double.xmax)
        })   
    if(is.na(out) || is.nan(out)){
        out <- .Machine$double.xmax
    }   
    print(as.numeric(out))
    return(as.numeric(out))
}



library(doSNOW)

pbSapply <- function(cl, X, FUN, ...) {
  registerDoSNOW(cl)
  pb <- txtProgressBar(max=length(X), style = 3)
  on.exit(close(pb))
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  foreach(i=X, .options.snow=opts) %dopar% {
    FUN(i, ...)
  }
}


load(file = "Libraries_data/Data/Ger_aust_norm_111_Chol.R")

pop <- opt_ger_aust$member$pop

nCores <- detectCores() - 2
cl <- makeCluster(nCores)

#"rlist","extraDistr","Rfast","tidyr","dplyr","MASS",,"Rsolnp")
clusterEvalQ(cl = cl, library("mvnfast"))
clusterEvalQ(cl = cl, library("Rsolnp"))
clusterEvalQ(cl = cl, library("dplyr"))

clusterExport(cl = cl, list = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma","pop",
"de_func"))

gb_ger_aust <- pbSapply(cl = cl, X= 1:270, FUN = function(x){
    a_new <- solnp(pars = pop[x,] , fun = de_func, LB =  c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), UB =  c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)))
    return(c(a_new$pars,last(a_new$values)))
 
})


save(gb_ger_aust, file = "Libraries_data/Data/Ger_aust_M_norm_par_Chol_2.R")

llh <- c()

for( i in 1:270){
  llh <- c(llh, gb_ger_aust[[i]][28])
}



ger_aust_obj<- gas_M(static_param = gb_ger_aust[[28]][-28],
                  data = rbind(returns[,2],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_aust_obj, file = "Libraries_data/Data/Ger_aust_M_norm_obj_chol.R")
