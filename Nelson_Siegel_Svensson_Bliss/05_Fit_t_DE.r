############################## TIME VARYING BETAS ONLY ##############################

#################### NS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 upper =  c(rep(3,3),rep(0.9999999999999999,3),rep(10,3),3,15, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NS_t_inv <- de_opt_t

# save(de_opt_NS_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv.R")
# DONE

# # GRADIENT OPTIMIZATION - de_opt_NS_t_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv.R")

# opt_NS_t_inv <- solnp(pars = de_opt_NS_t_inv$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3,15, rep(0.1,9)))

# # FINAL LLH -> -6388.878
# save(opt_NS_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt.R")

de_func(opt_NS_t_inv$pars)
########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,3),rep(0.9999999999999999,3),rep(10,3),3,15, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NS_t_sqrt <- de_opt_t

# save(de_opt_NS_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt.R")

# opt_NS_t_sqrt <- solnp(pars = de_opt_NS_t_sqrt$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3,15, rep(0.1,9)))

# # FINAL LLH -> -5880.031
# save(opt_NS_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt.R")


#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(-0.9999999999999999,4),rep(-10,4),-1,-3,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(3,4),rep(0.9999999999999999,4),rep(10,4),1,-1,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 3000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NSS_t_inv <- de_opt_t

# save(de_opt_NSS_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv.R")

# opt_NSS_t_inv <- solnp(pars = de_opt_NSS_t_inv$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,4),-1,-3,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,4),1,-1,15, rep(0.1,9)))

# # FINAL LLH ->  -7158.151
# save(opt_NSS_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(-0.9999999999999999,4),rep(-10,4),-10,-10,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,4),rep(0.9999999999999999,4),rep(10,4),3,3,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_NSS_t_sqrt <- de_opt_t

# save(de_opt_NSS_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt.R")

# opt_NSS_t_sqrt <- solnp(pars = de_opt_NSS_t_sqrt$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,4),-10,-10,0, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,4),3,3,9, rep(0.1,9)))

# # FINAL LLH -> -6378.886
# save(opt_NSS_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt.R")


#################### BL ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-3,-3,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(3,3),rep(0.9999999999999999,3),rep(10,3),0,0,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_BL_t_inv <- de_opt_t

# save(de_opt_BL_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv.R")

# opt_BL_t_inv <- solnp(pars = de_opt_BL_t_inv$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3,3,15, rep(0.1,9)))

# # FINAL LLH ->
# save(opt_BL_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt.R")



########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,3),rep(-0.9999999999999999,3),rep(-10,3),-3,-3,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,3),rep(0.9999999999999999,3),rep(10,3),0,0,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN")))

# de_opt_BL_t_sqrt <- de_opt_t

# save(de_opt_BL_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt.R")

# opt_BL_t_sqrt <- solnp(pars = de_opt_BL_t_sqrt$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,3),rep(-0.9999999999999999,3),rep(-10,3),-10,-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,3),rep(0.9999999999999999,3),rep(10,3),3,3,15, rep(0.1,9)))

# # FINAL LLH -> -5991.155
# save(opt_BL_t_sqrt, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt.R")


############################## TIME VARYING BETAS/LAMBDAS ##############################

#################### NS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-5,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(3,4),rep(0.9999999999999999,4),rep(10,3),3,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

# de_opt_NS_t_inv_L <- de_opt_t

# save(de_opt_NS_t_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_L.R")
# DONE

# # GRADIENT OPTIMIZATION - de_opt_NS_t_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_L.R")

# opt_NS_t_inv_L <- solnp(pars = de_opt_NS_t_inv_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3,15, rep(0.1,9)))

# # FINAL LLH -> -6473.203

# save(opt_NS_t_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt_L.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-5,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,4),rep(0.9999999999999999,4),rep(10,3),3,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

# de_opt_NS_t_sqrt_L <- de_opt_t

# save(de_opt_NS_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_L.R")



# # GRADIENT OPTIMIZATION - de_opt_NS_t_inv
# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_L.R")

# opt_NS_t_sqrt_L <- solnp(pars = de_opt_NS_t_sqrt_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3,15, rep(0.1,9)))

# # FINAL LLH -> -6105.989
# save(opt_NS_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt_L.R")


#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_NSS_t_inv_L <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,4),-1,-3,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(2,6),rep(0.9999999999999999,6),rep(10,4),1,-1,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, 
#                 itermax = 5000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))


# save(de_opt_NSS_t_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_L.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_L.R")

# opt_NSS_t_inv_L <- solnp(pars = de_opt_NSS_t_inv_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,4),-1,-3,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,4),1,-1,15, rep(0.1,9)))

# #FINAL LLH -> -7227.739
# save(opt_NSS_t_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt_L.R")



########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_NSS_t_sqrt_L <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,4),-1,-3,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,6),rep(0.9999999999999999,6),rep(10,4),1,-1,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))


# save(de_opt_NSS_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_L.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_L.R")

# opt_NSS_t_sqrt_L <- solnp(pars = de_opt_NSS_t_sqrt_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,4),-1,-3,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,4),1,-1,15, rep(0.1,9)))

# # FINAL LLH ->  -7117.325
# save(opt_NSS_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt_L.R")



#################### BL ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_BL_t_inv_L <- DEoptim(fn = de_func, lower = c(rep(0.8,3),rep(0,2),rep(0.7,5),rep(-10,3),-1,-1,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1.2,3),0.15,0.15,rep(0.9999999999999999,5),rep(10,3),0,-0.5,5, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2, 
#                 itermax = 3000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))


# save(de_opt_BL_t_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_L_2.R")

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_L_2.R")

# opt_BL_t_inv_L <- solnp(pars = de_opt_BL_t_inv_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(0.8,3),rep(0,2),rep(0.7,5),rep(-10,3),-1,-1,0, rep(0.00000000000000001,9)),
#                 UB =  c(rep(1.2,3),0.15,0.15,rep(0.9999999999999999,5),rep(10,3),0,-0.5,5, rep(0.1,9)))

# # FINAL LLH -> -6986.007
# save(opt_BL_t_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt_L_2.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_BL_t_sqrt_L <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-1,-1,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,5),rep(0.9999999999999999,5),rep(10,3),0,-0.5,6, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 3000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))


# save(de_opt_BL_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_L.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_L.R")

# opt_BL_t_sqrt_L <- solnp(pars = de_opt_BL_t_sqrt_L$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-1,-1,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),0,-0.5,15, rep(0.1,9)))

# # FINAL LLH -> -6061.037
# save(opt_BL_t_sqrt_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt_L.R")


##### DYNAMIC COVARIATION

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-5,-20,0, rep(0.00000000000000001,9)),
                upper =  c(rep(3,5),rep(0.9999999999999999,5),rep(10,3),0,0,9, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 4000, strategy = 1,
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

de_opt_NS_t_inv_cov <- de_opt_t

save(de_opt_NS_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_cov.R")


# GRADIENT OPTIMIZATION - de_opt_NS_t_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_cov.R")

opt_NS_t_inv_cov <- solnp(pars = de_opt_NS_t_inv_cov$optim$bestmem,
                fun = de_func,
                LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-5,-20,-20, rep(0.00000000000000001,9)),
                UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),0,0,15, rep(0.1,9)))

save(opt_NS_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt_cov.R")


# SQRT

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,5),rep(0,5),rep(-10,3),-10,-20,0, rep(0.00000000000000001,9)),
                upper =  c(rep(1,5),rep(0.9999999999999999,5),rep(10,3),3,-2,9, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 4000, strategy = 1,
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NS_L")))

de_opt_NS_t_sqrt_cov <- de_opt_t

save(de_opt_NS_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_cov.R")


# GRADIENT OPTIMIZATION - de_opt_NS_t_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_cov.R")

opt_NS_t_sqrt_cov <- solnp(pars = de_opt_NS_t_sqrt_cov$optim$bestmem,
                fun = de_func,
                LB =  c(rep(-Inf,5),rep(-0.9999999999999999,5),rep(-10,3),-10,-20,-20, rep(0.00000000000000001,9)),
                UB =  c(rep(Inf,5),rep(0.9999999999999999,5),rep(10,3),3,0,15, rep(0.1,9)))

save(opt_NS_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt_cov.R")



#################### NSS ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,7),rep(0,7),rep(-10,4),-1,-3,-20,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1.5,7),rep(0.9999999999999999,7),rep(10,4),1,-1,-2,9, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 5000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))

# de_opt_NSS_t_inv_cov <- de_opt_t

# save(de_opt_NSS_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_cov.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_cov.R")

# opt_NSS_t_inv_cov <- solnp(pars = de_opt_NSS_t_inv_cov$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,7),rep(-0.9999999999999999,7),rep(-10,4),-1,-3,-20,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,7),rep(0.9999999999999999,7),rep(10,4),1,-1,0,15, rep(0.1,9)))

# # FINAL LLH ->-8978.508
# save(opt_NSS_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt_cov.R")


########## SQRT ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

# de_opt_NSS_t_sqrt_cov <- DEoptim(fn = de_func, lower = c(rep(0,7),rep(0,7),rep(-10,4),-1,-3,-20,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,7),rep(0.9999999999999999,7),rep(10,4),1,-1,-2,9, rep(0.1,9)), control = DEoptim.control(initialpop = de_opt_NSS_t_sqrt_cov$member$pop,
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 1000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_NSS_L")))


# save(de_opt_NSS_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_cov.R")


# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_cov.R")

# opt_NSS_t_sqrt_cov <- solnp(pars = de_opt_NSS_t_sqrt_cov$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,7),rep(-0.9999999999999999,7),rep(-10,4),-1,-3,-20,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,7),rep(0.9999999999999999,7),rep(10,4),1,-1,0,15, rep(0.1,9)))


# save(opt_NSS_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt_cov.R")

#################### BL ####################

########## INV ##########

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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

de_opt_BL_t_inv_cov <- DEoptim(fn = de_func, lower = c(rep(0.8,3),rep(0,3),rep(0.7,6),rep(-10,3),-1,-1,-20,0, rep(0.00000000000000001,9)),
                upper =  c(rep(1.3,3),rep(0.10,2),1.3,rep(0.9999999999999999,6),rep(10,3),0,-0.5,-2,5, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2, 
                itermax = 4000, strategy = 1,
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))


save(de_opt_BL_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_cov.R")


load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_cov.R")

opt_BL_t_inv_cov <- solnp(pars = de_opt_BL_t_inv_cov$optim$bestmem,
                fun = de_func,
                LB =  c(rep(0.8,3),rep(0,3),rep(0,6),rep(-10,3),-1,-1,-20,0, rep(0.00000000000000001,9)),
                UB =   c(rep(1.3,3),rep(0.10,2),1.3,rep(0.9999999999999999,6),rep(10,3),0,-0.5,-2,5, rep(0.1,9)))

# FINAL LLH ->
save(opt_BL_t_inv_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt_cov.R")


########## SQRT ##########


de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,
                    data = ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = FALSE,
                    distr = "t",
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


# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# de_opt_t <- DEoptim(fn = de_func, lower = c(rep(0,6),rep(0,6),rep(-10,3),-1,-1,-20,0, rep(0.00000000000000001,9)),
#                 upper =  c(rep(1,6),rep(0.9999999999999999,6),rep(10,3),0,-0.5,-2,5, rep(0.1,9)), control = DEoptim.control(
#                 CR = 0.8,F = 1.2, c = 0.2,
#                 itermax = 4000, strategy = 1,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
#                 parVar = c("ger_yield","score_t", "mean_yield" ,"ginv_2","gas_cMVN","start_BL_L")))

# de_opt_BL_t_sqrt_cov <- de_opt_t

# save(de_opt_BL_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_cov.R")
# DONE

# load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_cov.R")

# opt_BL_t_sqrt_cov <- solnp(pars = de_opt_BL_t_sqrt_cov$optim$bestmem,
#                 fun = de_func,
#                 LB =  c(rep(-Inf,6),rep(-0.9999999999999999,6),rep(-10,3),-1,-1,-20,-20, rep(0.00000000000000001,9)),
#                 UB =  c(rep(Inf,6),rep(0.9999999999999999,6),rep(10,3),0,-0.5,0,5, rep(0.1,9)))

# # FINAL LLH ->
# save(opt_BL_t_sqrt_cov, file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt_cov.R")



