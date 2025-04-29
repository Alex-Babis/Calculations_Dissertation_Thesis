# LAST 52 will be used to evaluate

############################## TIME VARYING BETAS ONLY ##############################

#################### NS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(3,3),rep(0.9999999999999999,3),rep(10,3),3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NS_norm_inv <- de_opt_norm

# save(de_opt_NS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv.R")
# DONE 

# GRADIENT OPTIMIZATION - de_opt_NS_norm_inv

load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv.R")

opt_NS_norm_inv <- solnp(pars = de_opt_NS_norm_inv$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3, rep(0.1,9)))   

# FINAL LLH -> -6321.2755
save(opt_NS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,3),rep(0.9999999999999999,3),rep(10,3),3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NS_norm_sqrt <- de_opt_norm

# save(de_opt_NS_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt.R")
# DONE

load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt.R")

opt_NS_norm_sqrt <- solnp(pars = de_opt_NS_norm_sqrt$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3, rep(0.1,9)))   

# FINAL LLH -> -5731.981
save(opt_NS_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt.R")


#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_NSS_norm_inv <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(-0.9999999999999999,4),rep(-10,4),-1,-3, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(3,4),rep(0.9999999999999999,4),rep(10,4),1,-1, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 3000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))

# save(de_opt_NSS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv.R")

# opt_NSS_norm_inv <- solnp(pars = de_opt_NSS_norm_inv$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,4),-1,-3, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,4),1,-1, rep(0.1,9)))   

# # FINAL LLH -> -7116.388
# save(opt_NSS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(-0.9999999999999999,4),rep(-10,4),-10,-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,4),rep(0.9999999999999999,4),rep(10,4),3,3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NSS_norm_sqrt <- de_opt_norm

# save(de_opt_NSS_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt.R")
# DONE

load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt.R")

opt_NSS_norm_sqrt <- solnp(pars = de_opt_NSS_norm_sqrt$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,4),-10,-10, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,4),3,3, rep(0.1,9)))   

# FINAL LLH -> -6236.023
save(opt_NSS_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt.R")


#################### BL ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_BL_norm_inv <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-3,-3, rep(0.00000000000000001,9)), 
                upper =  c(rep(1.5,3),rep(0.9999999999999999,3),rep(10,3),1,1, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 2000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))


save(de_opt_BL_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv.R")


load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv.R")

opt_BL_norm_inv <- solnp(pars = de_opt_BL_norm_inv$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-3,-3, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),1,1, rep(0.1,9)))   

# FINAL LLH -> -6321.2776
save(opt_BL_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt.R")



########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,3),rep(0.9999999999999999,3),rep(10,3),3,3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_BL_norm_sqrt <- de_opt_norm

# save(de_opt_BL_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt.R")
# DONE 

load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt.R")

opt_BL_norm_sqrt <- solnp(pars = de_opt_BL_norm_sqrt$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-10, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3,3, rep(0.1,9)))   

# FINAL LLH -> -5814.2236
save(opt_BL_norm_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt.R")



############################## TIME VARYING BETAS/LAMBDAS ##############################

#################### NS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_NS_norm_inv_L <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-3, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1.5,4),rep(0.9999999999999999,4),rep(10,3),1, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))



# save(de_opt_NS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L.R")
# # DONE

# # GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L.R")

# opt_NS_norm_inv_L <- solnp(pars = de_opt_NS_norm_inv_L$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3, rep(0.1,9)))   

# # FINAL LLH -> -6404.587

# save(opt_NS_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,4),rep(0.9999999999999999,4),rep(10,3),3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

# de_opt_NS_norm_sqrt_L <- de_opt_norm

# save(de_opt_NS_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_L.R")
# DONE


# # GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_L.R")

# opt_NS_norm_sqrt_L <- solnp(pars = de_opt_NS_norm_sqrt_L$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3, rep(0.1,9)))   

# # FINAL LLH -> -5966.158

# save(opt_NS_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt_L.R")


#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_NSS_norm_inv_L <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,4),-1,-3, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(2,6),rep(0.9999999999999999,6),rep(10,4),1,-1, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, 
#                 itermax = 4000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))

# save(de_opt_NSS_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_L.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_L.R")

# opt_NSS_norm_inv_L <- solnp(pars = de_opt_NSS_norm_inv_L$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,4),-1,-3, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,4),1,-1, rep(0.1,9)))   
# # ZATIAL -7185.024
# # FINAL LLH -> -7343.36
# save(opt_NSS_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt_L.R")



########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,4),-10,-10, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,6),rep(0.9999999999999999,6),rep(10,4),3,3, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 3000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))

# de_opt_NSS_norm_sqrt_L <- de_opt_norm

# save(de_opt_NSS_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_L.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_L.R")

# opt_NSS_norm_sqrt_L <- solnp(pars = de_opt_NSS_norm_sqrt_L$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,4),-10,-10, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,4),3,3, rep(0.1,9)))   

# # FINAL LLH -> -6810.29
# save(opt_NSS_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt_L.R")


#################### BL ####################


########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)

}


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_BL_norm_inv_L <- DEoptim(fn = de_func, lower = c(rep(0.8,3),rep(0,2),rep(0.7,5),rep(-10,3),-1,-1, rep(0.00000000000000001,9)), 
                upper =  c(rep(1.3,3),rep(0.15,2),rep(0.9999999999999999,5),rep(10,3),0,-0.5, rep(0.1,9)), control = DEoptim.control( 
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 3000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))


save(de_opt_BL_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_L.R")


load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_L.R")

opt_BL_norm_inv_L <- solnp(pars = de_opt_BL_norm_inv_L$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(0,5),rep(-0.9999999999999999,5),rep(-10,3),-1,-1, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,3),rep(0.15,2),rep(0.9999999999999999,5),rep(10,3),0,-0.5, rep(0.1,9)))   

# FINAL LLH ->  -6976.042
save(opt_BL_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt_L.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_BL_norm_sqrt_L <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-1,-1, rep(0.00000000000000001,9)), 
                upper =  c(rep(1,5),rep(0.9999999999999999,5),rep(10,3),0,-0.5, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2, 
                itermax = 3000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))


save(de_opt_BL_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_L.R")


load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_L.R")

opt_BL_norm_sqrt_L <- solnp(pars = de_opt_BL_norm_sqrt_L$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-1,-1, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),0,-0.5, rep(0.1,9)))   

# FINAL LLH -> -5986.484
save(opt_BL_norm_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt_L.R")


##### DYNAMIC COVARIATION

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-10,-20, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(3,5),rep(0.9999999999999999,5),rep(10,3),3,0, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, initialpop = de_opt_NS_norm_inv_cov$member$pop,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

# de_opt_NS_norm_inv_cov <- de_opt_norm

# save(de_opt_NS_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_cov.R")
# DONE

# # GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_cov.R")

# opt_NS_norm_inv_cov <- solnp(pars = de_opt_NS_norm_inv_cov$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-10,-20, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),3,0, rep(0.1,9)))   
 
# save(opt_NS_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_cov.R")


# SQRT

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-10,-20, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,5),rep(0.9999999999999999,5),rep(10,3),3,-2, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, initialpop = de_opt_NS_norm_sqrt_cov$member$pop,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

# de_opt_NS_norm_sqrt_cov <- de_opt_norm

# save(de_opt_NS_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_cov.R")
# DONE

# # GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_cov.R")

# opt_NS_norm_sqrt_cov <- solnp(pars = de_opt_NS_norm_sqrt_cov$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-10,-20, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),3,0, rep(0.1,9)))   
 
# save(opt_NS_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt_cov.R")



#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,7),rep(0,7),rep(-10,4),-1,-3,-20, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1.5,7),rep(0.9999999999999999,7),rep(10,4),1,-1,-2, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, 
#                 itermax = 4000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))

# de_opt_NSS_norm_inv_cov <- de_opt_norm

# save(de_opt_NSS_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_cov.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_cov.R")

# opt_NSS_norm_inv_cov <- solnp(pars = de_opt_NSS_norm_inv_cov$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,7),rep(-0.9999999999999999,7),rep(-10,4),-1,-3,-20, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,7),rep(0.9999999999999999,7),rep(10,4),1,-1,0, rep(0.1,9)))   

# # FINAL LLH -> -8568.752
# save(opt_NSS_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt_cov.R")

########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,7),rep(0,7),rep(-10,4),-10,-10,-20, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,7),rep(0.9999999999999999,7),rep(10,4),3,3,-2, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, initialpop = de_opt_NSS_norm_sqrt_cov$member$pop,
#                 itermax = 2000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))

# de_opt_NSS_norm_sqrt_cov <- de_opt_norm

# save(de_opt_NSS_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_cov.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_cov.R")

# opt_NSS_norm_sqrt_cov <- solnp(pars = de_opt_NSS_norm_sqrt_cov$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,7),rep(-0.9999999999999999,7),rep(-10,4),-10,-10,-20, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,7),rep(0.9999999999999999,7),rep(10,4),3,3,0, rep(0.1,9)))   

# # FINAL LLH -> 
# save(opt_NSS_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt_cov.R")

#################### BL ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_BL_norm_inv_cov <- DEoptim(fn = de_func, lower = c(rep(0.8,3),rep(0,3),rep(0.7,6),rep(-10,3),-1,-1,-20, rep(0.00000000000000001,9)), 
                upper =  c(rep(1.3,3),rep(0.15,2),1.3,rep(0.9999999999999999,6),rep(10,3),0,-0.5,-2, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 4000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))

#de_opt_BL_norm_inv_cov <- de_opt_norm

save(de_opt_BL_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_cov2.R")


load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_cov2.R")

opt_BL_norm_inv_cov <- solnp(pars = de_opt_BL_norm_inv_cov$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(0.8,3),rep(-Inf,3),rep(-0.9999999999999999,6),rep(-10,3),-1,-1,-20, rep(0.00000000000000001,9)), 
                UB =  c(rep(1.3,3),rep(0.15,2),1.3,rep(0.9999999999999999,6),rep(10,3),0,-0.5,0, rep(0.1,9)))   

# FINAL LLH ->  
save(opt_BL_norm_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt_cov2.R")

########## SQRT ##########


de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = NULL)
        },
        error=function(cond) {
            return(Inf)
        },
        warning=function(cond) {
            return(Inf)
        })   
    if(is.na(out) || is.nan(out)){
        out <- Inf
    }   
    print(as.numeric(out))
    return(out)
}

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_cov.R")

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_norm <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,3),-1,-3,-20, rep(0.00000000000000001,9)), 
#                 upper =  c(rep(1,6),rep(0.9999999999999999,6),rep(10,3),1,0,-2, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, 
#                 itermax = 5000, strategy = 1, 
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))

# de_opt_BL_norm_sqrt_cov <- de_opt_norm

# save(de_opt_BL_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_cov.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_cov.R")

# opt_BL_norm_sqrt_cov <- solnp(pars = de_opt_BL_norm_sqrt_cov$optim$bestmem, 
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,3),-1,-3,-20, rep(0.00000000000000001,9)), 
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,3),1,0,0, rep(0.1,9)))   

# # FINAL LLH ->  
# save(opt_BL_norm_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt_cov.R")
