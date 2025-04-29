# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               SLOVINSKO   8               #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

# CECH

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_cech <- DEoptim(fn = de_func, lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_cech, file = "Libraries_data/Data/Slo_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_cech_norm_111_Chol.R")

gb_slo_cech <-  solnp(pars = opt_slo_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_cech, file = "Libraries_data/Data/Slo_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_cech_M_norm_par_Chol.R")

slo_cech_obj<- gas_M(static_param = gb_slo_cech$pars,
                  data = rbind(returns[,1],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)


save(slo_cech_obj, file = "Libraries_data/Data/Slo_cech_M_norm_obj_chol.R")

# AUST

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_aust <- DEoptim(fn = de_func, lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_aust, file = "Libraries_data/Data/Slo_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_aust_norm_111_Chol.R")

gb_slo_aust <-  solnp(pars = opt_slo_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_aust, file = "Libraries_data/Data/Slo_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_aust_M_norm_par_Chol.R")

slo_aust_obj<- gas_M(static_param = gb_slo_aust$pars,
                  data = rbind(returns[,2],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_aust_obj, file = "Libraries_data/Data/Slo_aust_M_norm_obj_chol.R")


### SLOVAKIA


de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,3],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_svk <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_svk, file = "Libraries_data/Data/Slo_svk_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_svk_norm_111_Chol.R")

gb_slo_svk <-  solnp(pars = opt_slo_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_svk, file = "Libraries_data/Data/Slo_svk_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_svk_M_norm_par_Chol.R")

slo_svk_obj<- gas_M(static_param = gb_slo_svk$pars,
                  data = rbind(returns[,3],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_svk_obj, file = "Libraries_data/Data/Slo_svk_M_norm_obj_chol.R")


# POLAND

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,4],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_pol <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_pol, file = "Libraries_data/Data/Slo_pol_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_pol_norm_111_Chol.R")

gb_slo_pol <-  solnp(pars = opt_slo_pol$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_pol, file = "Libraries_data/Data/Slo_pol_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_pol_M_norm_par_Chol.R")

slo_pol_obj<- gas_M(static_param = gb_slo_pol$pars,
                  data = rbind(returns[,4],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_pol_obj, file = "Libraries_data/Data/Slo_pol_M_norm_obj_chol.R")


# HUNGARY
de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,5],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_hun <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_hun, file = "Libraries_data/Data/Slo_hun_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_hun_norm_111_Chol.R")

gb_slo_hun <-  solnp(pars = opt_slo_hun$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_hun, file = "Libraries_data/Data/Slo_hun_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_hun_M_norm_par_Chol.R")

slo_hun_obj<- gas_M(static_param = gb_slo_hun$pars,
                  data = rbind(returns[,5],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_hun_obj, file = "Libraries_data/Data/Slo_hun_M_norm_obj_chol.R")

# SWI


de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,6],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_swi <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_swi, file = "Libraries_data/Data/Slo_swi_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_swi_norm_111_Chol.R")

gb_slo_swi <-  solnp(pars = opt_slo_swi$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_swi, file = "Libraries_data/Data/Slo_swi_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_swi_M_norm_par_Chol.R")

slo_swi_obj<- gas_M(static_param = gb_slo_swi$pars,
                  data = rbind(returns[,6],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_swi_obj, file = "Libraries_data/Data/Slo_swi_M_norm_obj_chol.R")


# GER

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,7],returns[,8]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_slo_ger <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_slo_ger, file = "Libraries_data/Data/Slo_ger_norm_111_Chol.R")

load(file = "Libraries_data/Data/Slo_ger_norm_111_Chol.R")

gb_slo_ger <-  solnp(pars = opt_slo_ger$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_slo_ger, file = "Libraries_data/Data/Slo_ger_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Slo_ger_M_norm_par_Chol.R")

slo_ger_obj<- gas_M(static_param = gb_slo_ger$pars,
                  data = rbind(returns[,7],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(slo_ger_obj, file = "Libraries_data/Data/Slo_ger_M_norm_obj_chol.R")



# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               NEMECKO    7               #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,7]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_cech, file = "Libraries_data/Data/Ger_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_cech_norm_111_Chol.R")

gb_ger_cech <-  solnp(pars = opt_ger_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_cech, file = "Libraries_data/Data/Ger_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_cech_M_norm_par_Chol.R")

ger_cech_obj<- gas_M(static_param = gb_ger_cech$pars,
                  data = rbind(returns[,1],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_cech_obj, file = "Libraries_data/Data/Ger_cech_M_norm_obj_chol.R")


#AUST

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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_aust <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_aust, file = "Libraries_data/Data/Ger_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_aust_norm_111_Chol.R")

gb_ger_aust <-  solnp(pars = opt_ger_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_aust, file = "Libraries_data/Data/Ger_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_aust_M_norm_par_Chol.R")

ger_aust_obj<- gas_M(static_param = gb_ger_aust$pars,
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




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,3],returns[,7]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_svk <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_svk, file = "Libraries_data/Data/Ger_svk_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_svk_norm_111_Chol.R")

gb_ger_svk <-  solnp(pars = opt_ger_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_svk, file = "Libraries_data/Data/Ger_svk_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_svk_M_norm_par_Chol.R")

ger_svk_obj<- gas_M(static_param = gb_ger_svk$pars,
                  data = rbind(returns[,3],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_svk_obj, file = "Libraries_data/Data/Ger_svk_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,4],returns[,7]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_pol <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_pol, file = "Libraries_data/Data/Ger_pol_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_pol_norm_111_Chol.R")

gb_ger_pol <-  solnp(pars = opt_ger_pol$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_pol, file = "Libraries_data/Data/Ger_pol_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_pol_M_norm_par_Chol.R")

ger_pol_obj<- gas_M(static_param = gb_ger_pol$pars,
                  data = rbind(returns[,4],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_pol_obj, file = "Libraries_data/Data/Ger_pol_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,5],returns[,7]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_hung <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_hung, file = "Libraries_data/Data/Ger_hung_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_hung_norm_111_Chol.R")

gb_ger_hung <-  solnp(pars = opt_ger_hung$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_hung, file = "Libraries_data/Data/Ger_hung_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_hung_M_norm_par_Chol.R")

ger_hung_obj<- gas_M(static_param = gb_ger_hung$pars,
                  data = rbind(returns[,5],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_hung_obj, file = "Libraries_data/Data/Ger_hung_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,6],returns[,7]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_ger_swis <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_ger_swis, file = "Libraries_data/Data/Ger_swis_norm_111_Chol.R")

load(file = "Libraries_data/Data/Ger_swis_norm_111_Chol.R")

gb_ger_swis <-  solnp(pars = opt_ger_swis$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_ger_swis, file = "Libraries_data/Data/Ger_swis_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Ger_swis_M_norm_par_Chol.R")

ger_swis_obj<- gas_M(static_param = gb_ger_swis$pars,
                  data = rbind(returns[,6],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(ger_swis_obj, file = "Libraries_data/Data/Ger_swis_M_norm_obj_chol.R")



# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               SVAJCIARSKO   6             #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,6]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_swis_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_swis_cech, file = "Libraries_data/Data/Swis_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Swis_cech_norm_111_Chol.R")

gb_swis_cech <-  solnp(pars = opt_swis_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_swis_cech, file = "Libraries_data/Data/Swis_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Swis_cech_M_norm_par_Chol.R")

swis_cech_obj<- gas_M(static_param = gb_swis_cech$pars,
                  data = rbind(returns[,1],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(swis_cech_obj, file = "Libraries_data/Data/Swis_cech_M_norm_obj_chol.R")


#AUST

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,6]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_swis_aust <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_swis_aust, file = "Libraries_data/Data/Swis_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Swis_aust_norm_111_Chol.R")

gb_swis_aust <-  solnp(pars = opt_swis_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_swis_aust, file = "Libraries_data/Data/Swis_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Swis_aust_M_norm_par_Chol.R")

swis_aust_obj<- gas_M(static_param = gb_swis_aust$pars,
                  data = rbind(returns[,2],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(swis_aust_obj, file = "Libraries_data/Data/Swis_aust_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,3],returns[,6]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_swis_svk <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_swis_svk, file = "Libraries_data/Data/Swis_svk_norm_111_Chol.R")

load(file = "Libraries_data/Data/Swis_svk_norm_111_Chol.R")

gb_swis_svk <-  solnp(pars = opt_swis_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_swis_svk, file = "Libraries_data/Data/Swis_svk_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Swis_svk_M_norm_par_Chol.R")

swis_svk_obj<- gas_M(static_param = gb_swis_svk$pars,
                  data = rbind(returns[,3],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(swis_svk_obj, file = "Libraries_data/Data/Swis_svk_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,4],returns[,6]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_swis_pol <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_swis_pol, file = "Libraries_data/Data/Swis_pol_norm_111_Chol.R")

load(file = "Libraries_data/Data/Swis_pol_norm_111_Chol.R")

gb_swis_pol <-  solnp(pars = opt_swis_pol$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_swis_pol, file = "Libraries_data/Data/Swis_pol_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Swis_pol_M_norm_par_Chol.R")

swis_pol_obj<- gas_M(static_param = gb_swis_pol$pars,
                  data = rbind(returns[,4],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(swis_pol_obj, file = "Libraries_data/Data/Swis_pol_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,5],returns[,6]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_swis_hung <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_swis_hung, file = "Libraries_data/Data/Swis_hung_norm_111_Chol.R")

load(file = "Libraries_data/Data/Swis_hung_norm_111_Chol.R")

gb_swis_hung <-  solnp(pars = opt_swis_hung$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_swis_hung, file = "Libraries_data/Data/Swis_hung_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Swis_hung_M_norm_par_Chol.R")

swis_hung_obj<- gas_M(static_param = gb_swis_hung$pars,
                  data = rbind(returns[,5],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(swis_hung_obj, file = "Libraries_data/Data/Swis_hung_M_norm_obj_chol.R")



# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               Madarsko      5             #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,5]),
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

nCores <- detectCores() - 2
cl <- makeCluster(nCores)

opt_hung_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
                upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-3,-3,3,3,3,3,3)), control = DEoptim.control(
                CR = 0.8,c = 0.1,
                itermax = 700, strategy = 6, p = 0.2,
                parallelType = "parallel",
                cluster = cl,
                packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
                "dplyr","MASS"),
                parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

save(opt_hung_cech, file = "Libraries_data/Data/Hung_cech_norm_111_Chol.R")

# x_new <- c(0.041624,    0.028260,    0.056200,    0.061619,    0.010618,
#   0.017979,    0.015753,    0.003147,    0.064973,    0.654671,
#   0.142303,    0.989608,    0.992111,    0.473826,    0.690092,
#   0.790172,    0.696652,    0.757459,    0.002491,    0.004107,
#  -5.440152,   -4.367489,   -0.080012,   -0.161189,   -0.186122,
#  -0.060903,   -0.655862)

load(file = "Libraries_data/Data/Hung_cech_norm_111_Chol.R")

gb_hung_cech <-  solnp(pars = opt_hung_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_hung_cech, file = "Libraries_data/Data/Hung_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Hung_cech_M_norm_par_Chol.R")

hung_cech_obj<- gas_M(static_param = gb_hung_cech$pars,
                  data = rbind(returns[,1],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -2852.188


save(hung_cech_obj, file = "Libraries_data/Data/Hung_cech_M_norm_obj_chol.R")


#AUST

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,5]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_hung_aust <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-3,-3,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_hung_aust, file = "Libraries_data/Data/Hung_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Hung_aust_norm_111_Chol.R")

gb_hung_aust <-  solnp(pars = opt_hung_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_hung_aust, file = "Libraries_data/Data/Hung_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Hung_aust_M_norm_par_Chol.R")

hung_aust_obj<- gas_M(static_param = gb_hung_aust$pars,
                  data = rbind(returns[,2],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(hung_aust_obj, file = "Libraries_data/Data/Hung_aust_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,3],returns[,5]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_hung_svk <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-3,-3,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_hung_svk, file = "Libraries_data/Data/Hung_svk_norm_111_Chol.R")

load(file = "Libraries_data/Data/Hung_svk_norm_111_Chol.R")

gb_hung_svk <-  solnp(pars = opt_hung_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_hung_svk, file = "Libraries_data/Data/Hung_svk_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Hung_svk_M_norm_par_Chol.R")

hung_svk_obj<- gas_M(static_param = gb_hung_svk$pars,
                  data = rbind(returns[,3],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(hung_svk_obj, file = "Libraries_data/Data/Hung_svk_M_norm_obj_chol.R")



de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,4],returns[,5]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_hung_pol <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-3,-3,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 800, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_hung_pol, file = "Libraries_data/Data/Hung_pol_norm_111_Chol.R")

load(file = "Libraries_data/Data/Hung_pol_norm_111_Chol.R")

gb_hung_pol <-  solnp(pars = opt_hung_pol$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_hung_pol, file = "Libraries_data/Data/Hung_pol_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Hung_pol_M_norm_par_Chol.R")

hung_pol_obj<- gas_M(static_param = gb_hung_pol$pars,
                  data = rbind(returns[,4],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(hung_pol_obj, file = "Libraries_data/Data/Hung_pol_M_norm_obj_chol.R")


# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               POLSKO        5             #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,4]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_pol_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_pol_cech, file = "Libraries_data/Data/Pol_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Pol_cech_norm_111_Chol.R")

gb_pol_cech <-  solnp(pars = opt_pol_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_pol_cech, file = "Libraries_data/Data/Pol_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Pol_cech_M_norm_par_Chol.R")

pol_cech_obj<- gas_M(static_param = gb_pol_cech$pars,
                  data = rbind(returns[,1],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(pol_cech_obj, file = "Libraries_data/Data/Pol_cech_M_norm_obj_chol.R")


#AUST

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,4]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_pol_aust <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_pol_aust, file = "Libraries_data/Data/Pol_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Pol_aust_norm_111_Chol.R")

gb_pol_aust <-  solnp(pars = opt_pol_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_pol_aust, file = "Libraries_data/Data/Pol_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Pol_aust_M_norm_par_Chol.R")

pol_aust_obj<- gas_M(static_param = gb_pol_aust$pars,
                  data = rbind(returns[,2],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819
plot(returns[,2],type ='l')

save(pol_aust_obj, file = "Libraries_data/Data/Pol_aust_M_norm_obj_chol.R")




de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,3],returns[,4]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_pol_svk <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_pol_svk, file = "Libraries_data/Data/Pol_svk_norm_111_Chol.R")

load(file = "Libraries_data/Data/Pol_svk_norm_111_Chol.R")

gb_pol_svk <-  solnp(pars = opt_pol_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_pol_svk, file = "Libraries_data/Data/Pol_svk_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Pol_svk_M_norm_par_Chol.R")

pol_svk_obj<- gas_M(static_param = gb_pol_svk$pars,
                  data = rbind(returns[,3],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(pol_svk_obj, file = "Libraries_data/Data/Pol_svk_M_norm_obj_chol.R")


# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               Slovensko     3             #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,3]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_svk_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-8.5,-8.5,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_svk_cech, file = "Libraries_data/Data/Svk_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Svk_cech_norm_111_Chol.R")

gb_svk_cech <-  solnp(pars = opt_svk_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_svk_cech, file = "Libraries_data/Data/Svk_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Svk_cech_M_norm_par_Chol.R")

svk_cech_obj<- gas_M(static_param = gb_svk_cech$pars,
                  data = rbind(returns[,1],returns[,3]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4394.819


save(svk_cech_obj, file = "Libraries_data/Data/Svk_cech_M_norm_obj_chol.R")


#AUST

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,2],returns[,3]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_svk_aust <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-9,-9,-1.5,-1.5,-1.5,-1.5,-1.5)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-6,-6,1.5,1.5,1.5,1.5,1.5)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 600, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_svk_aust, file = "Libraries_data/Data/Svk_aust_norm_111_Chol.R")

load(file = "Libraries_data/Data/Svk_aust_norm_111_Chol.R")

gb_svk_aust <-  solnp(pars = opt_svk_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_svk_aust, file = "Libraries_data/Data/Svk_aust_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Svk_aust_M_norm_par_Chol.R")

svk_aust_obj<- gas_M(static_param = gb_svk_aust$pars,
                  data = rbind(returns[,2],returns[,3]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -4813.41


save(svk_aust_obj, file = "Libraries_data/Data/Svk_aust_M_norm_obj_chol.R")

plot(svk_aust_obj$cor_out[6,], type ='l')

# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  
#               Rakusko     2               #  
# # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # #  

de_func <- function(x){

    out <- tryCatch(
        {
            gas_M(static_param = x,
                  data = rbind(returns[,1],returns[,2]),
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

# nCores <- detectCores() - 2
# cl <- makeCluster(nCores)

# opt_aust_cech <- DEoptim(fn = de_func,  lower =  c(rep(0,9),rep(0,9),c(-0.1,-0.1,-7,-7,-3,-3,-3,-3,-3)), 
#                 upper = c(rep(0.1,9), rep(1,9),c(0.1,0.1,-4,-4,3,3,3,3,3)), control = DEoptim.control(
#                 CR = 0.8,c = 0.1,
#                 itermax = 700, strategy = 6, p = 0.2,
#                 parallelType = "parallel",
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_M","ginv_2","gas_M","cond_mu_sigma")))

# save(opt_aust_cech, file = "Libraries_data/Data/Aust_cech_norm_111_Chol.R")

load(file = "Libraries_data/Data/Aust_cech_norm_111_Chol.R")

gb_aust_cech <-  solnp(pars = opt_aust_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-1,-1,-13,-13,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(1,1,-1,-1,3,3,3,3,3)) )   



save(gb_aust_cech, file = "Libraries_data/Data/Aust_cech_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Aust_cech_M_norm_par_Chol.R")

aust_cech_obj<- gas_M(static_param = gb_aust_cech$pars,
                  data = rbind(returns[,1],returns[,2]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
# -3828.281


save(aust_cech_obj, file = "Libraries_data/Data/Aust_cech_M_norm_obj_chol.R")
