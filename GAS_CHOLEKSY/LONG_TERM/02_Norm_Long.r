
de_func <- function(x){

    out <- tryCatch(
        {
            gas_M_long(static_param = x,
                  data = rbind(returns[,1],returns[,8]),
                  lag_order = 1,
                  dist = "norm")
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
    #print(as.numeric(out))
    return(as.numeric(out))
}


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

LT_fina2y <- DEoptim(fn = de_func, lower =  c(-1,-1,-10,-10,-5,-5,-5,-5,-5), 
                upper = c(1,1,0,0,5,5,5,5,5), control = DEoptim.control(
                CR = 0.8, 
                itermax = 1500, strategy = 6, p = 0.2, F = 1.2, 
                parallelType = "parallel",
                cluster = cl,
                packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
                "dplyr","MASS"),
                parVar = c("returns","grad_norm_M",
                "ginv_2","gas_M_long")))


save(LT_fina2y, file = "Libraries_data/Data/Yield2y_Fina_norm_LT_Chol.R")
