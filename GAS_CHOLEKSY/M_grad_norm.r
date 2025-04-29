##############
# long-term values of parameters

#############
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

load(file = "Libraries_data/Data/Yield2y_util_norm_111_Chol2.R")

gb_util2y <-  solnp(pars = opt_util2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3)) )   



save(gb_util2y, file = "Libraries_data/Data/util_2y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/util_2y_M_norm_par_Chol.R")

util2y_obj <- gas_M(static_param = gb_util2y$pars,
                  data = rbind(returns[,1],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

# -5549.702
save(util2y_obj, file = "Libraries_data/Data/util_2y_M_norm_obj_chol.R")



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

load(file = "Libraries_data/Data/Yield10y_util_norm_111_Chol2.R")

# parameters are stored in >> opt_util10y


gb_util10y <-  solnp(pars = opt_util10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-10,-10,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3192.671 Target

save(gb_util10y, file = "Libraries_data/Data/util_10y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/util_10y_M_norm_par_Chol.R")


util10y_obj <- gas_M(static_param = gb_util10y$pars,
                  data = rbind(returns[,2],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(util10y_obj, file = "Libraries_data/Data/util_10y_M_norm_obj_Chol.R")

# util YIELD 30y

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
 

load(file = "Libraries_data/Data/Yield30y_util_norm_111_Chol2.R")

# parameters are stored in >> opt_util30y

gb_util30y <-  solnp(pars = opt_util30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3)) )   

# LLH -2880.7889

save(gb_util30y, file = "Libraries_data/Data/util_30y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/util_30y_M_norm_par_Chol.R")

util30y_obj <- gas_M(static_param = gb_util30y$pars,
                  data = rbind(returns[,3],returns[,4]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
save(util30y_obj,file = "Libraries_data/Data/util_30y_M_norm_obj_Chol.R")

#################################################################
#################################################################
#########################  cogo  ################################
#################################################################
#################################################################

# cogo util 2y

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

load( file = "Libraries_data/Data/Yield2y_cogo_norm_111_Chol.R")

# parameters are stored in >> opt_cogo2y

gb_cogo2y <-  solnp(pars = opt_cogo2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -4030.2968

save(gb_cogo2y, file = "Libraries_data/Data/cogo_2y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/cogo_2y_M_norm_par_Chol.R")
cogo2y_obj <- gas_M(static_param = gb_cogo2y$pars,
                  data = rbind(returns[,1],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(cogo2y_obj, file = "Libraries_data/Data/cogo_2y_M_norm_obj_Chol.R")



# cogo util 10y

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

load( file = "Libraries_data/Data/Yield10y_cogo_norm_111_Chol.R")

# parameters are stored in >> opt_cogo10y

gb_cogo10y <-  solnp(pars = opt_cogo10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3616.178


save(gb_cogo10y, file = "Libraries_data/Data/cogo_10y_M_norm_par_Chol.R")

load( file = "Libraries_data/Data/cogo_10y_M_norm_par_Chol.R")

cogo10y_obj <- gas_M(static_param = gb_cogo10y$pars,
                  data = rbind(returns[,2],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(cogo10y_obj,  file = "Libraries_data/Data/cogo_10y_M_norm_obj_Chol.R")



# cogo util 30y

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
 
load( file = "Libraries_data/Data/Yield30y_cogo_norm_111_Chol.R")

# parameters are stored in >> opt_cogo30y

gb_cogo30y <-  solnp(pars = opt_cogo30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3223.5804

save(gb_cogo30y, file = "Libraries_data/Data/cogo_30y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/cogo_30y_M_norm_par_Chol.R")

cogo30y_obj <- gas_M(static_param = gb_cogo30y$pars,
                  data = rbind(returns[,3],returns[,5]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(cogo30y_obj, file = "Libraries_data/Data/cogo_30y_M_norm_obj_Chol.R")

#################################################################
#################################################################
#########################  INDY  ################################
#################################################################
#################################################################

# INDY util 2y

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

load(file = "Libraries_data/Data/Yield2y_Indy_norm_111_Chol.R")

# parameters are stored in >> opt_indy2y

gb_indy2y <-  solnp(pars = opt_indy2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3959.716

save(gb_indy2y, file = "Libraries_data/Data/Indy_2y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Indy_2y_M_norm_par_Chol.R")
indy2y_obj <- gas_M(static_param = gb_indy2y$pars,
                  data = rbind(returns[,1],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
save(indy2y_obj , file = "Libraries_data/Data/Indy_2y_M_norm_obj_Chol.R")

# INDY util 10y

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


load( file = "Libraries_data/Data/Yield10y_Indy_norm_111_Chol.R")

# parameters are stored in >> opt_indy10y

gb_indy10y <-  solnp(pars = opt_indy10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3508.8131

save(gb_indy10y, file = "Libraries_data/Data/Indy_10y_M_norm_par_Chol.R")

load( file = "Libraries_data/Data/Indy_10y_M_norm_par_Chol.R")
indy10y_obj <- gas_M(static_param = gb_indy10y$pars,
                  data = rbind(returns[,2],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)
save(indy10y_obj,  file = "Libraries_data/Data/Indy_10y_M_norm_obj_Chol.R")

# INDY util 30y

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
 
load( file = "Libraries_data/Data/Yield30y_Indy_norm_111_Chol.R")

# parameters are stored in >> opt_indy30y

gb_indy30y <-  solnp(pars = opt_indy30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3181.3423

save(gb_indy30y, file = "Libraries_data/Data/Indy_30y_M_norm_par_Chol.R")

load( file = "Libraries_data/Data/Indy_30y_M_norm_par_Chol.R")

indy30y_obj <- gas_M(static_param = gb_indy30y$pars,
                  data = rbind(returns[,3],returns[,6]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(indy30y_obj,  file = "Libraries_data/Data/Indy_30y_M_norm_obj_Chol.R")

#################################################################
#################################################################
#########################  TECH  ################################
#################################################################
#################################################################

# TECH util 2y
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

load( file = "Libraries_data/Data/Yield2y_Tech_norm_111_Chol.R")

# parameters are stored in >> opt_tech2y

gb_tech2y <-  solnp(pars = opt_tech2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3884.7374

save(gb_tech2y, file = "Libraries_data/Data/Tech_2y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Tech_2y_M_norm_par_Chol.R")
tech2y_obj <- gas_M(static_param = gb_tech2y$pars,
                  data = rbind(returns[,1],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(tech2y_obj,file = "Libraries_data/Data/Tech_2y_M_norm_obj_Chol.R")
# TECH util 10y

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

load( file = "Libraries_data/Data/Yield10y_Tech_norm_111_Chol.R")

# parameters are stored in >> opt_tech10y

gb_tech10y <-  solnp(pars = opt_tech10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3417.233

save(gb_tech10y, file = "Libraries_data/Data/Tech_10y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Tech_10y_M_norm_par_Chol.R")
tech10y_obj <- gas_M(static_param = gb_tech10y$pars,
                  data = rbind(returns[,2],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(tech10y_obj,file = "Libraries_data/Data/Tech_10y_M_norm_obj_Chol.R")

# TECH util 30y

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
 
load( file = "Libraries_data/Data/Yield30y_Tech_norm_111_Chol.R")

# parameters are stored in >> opt_tech30y

gb_tech30y <-  solnp(pars = opt_tech30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3100.7387

save(gb_tech30y, file = "Libraries_data/Data/Tech_30y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Tech_30y_M_norm_par_Chol.R")
tech30y_obj <- gas_M(static_param = gb_tech30y$pars,
                  data = rbind(returns[,3],returns[,7]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(tech30y_obj, file = "Libraries_data/Data/Tech_30y_M_norm_obj_Chol.R")

#################################################################
#################################################################
#########################  FINA  ################################
#################################################################
#################################################################

# FINA util 2y

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

load( file = "Libraries_data/Data/Yield2y_Fina_norm_111_Chol.R")

# parameters are stored in >> opt_fina2y


gb_fina2y <-  solnp(pars = opt_fina2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -10390.11

save(gb_fina2y, file = "Libraries_data/Data/Fina_2y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Fina_2y_M_norm_par_Chol.R")
fina2y_obj <- gas_M(static_param = gb_fina2y$pars,
                  data = rbind(returns[,1],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(fina2y_obj, file = "Libraries_data/Data/Fina_2y_M_norm_obj_Chol.R")

# FINA util 10y
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

load( file = "Libraries_data/Data/Yield10y_Fina_norm_111_Chol.R")

# parameters are stored in >> opt_fina10y

gb_fina10y <-  solnp(pars = opt_fina10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -3355.7790

save(gb_fina10y, file = "Libraries_data/Data/Fina_10y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Fina_10y_M_norm_par_Chol.R")

fina10y_obj <- gas_M(static_param = gb_fina10y$pars,
                  data = rbind(returns[,2],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)


save(fina10y_obj, file = "Libraries_data/Data/Fina_10y_M_norm_obj_Chol.R")

# FINA util 30y

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

load( file = "Libraries_data/Data/Yield30y_Fina_norm_111_Chol.R")

# parameters are stored in >> opt_fina30y


gb_fina30y <-  solnp(pars = opt_fina30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,9),rep(0,9),c(-8,-8,-3,-3,-3,-3,-3)), 
                    UB =   c(rep(1,9), rep(1,9),c(-3,-3,3,3,3,3,3))  )   

# LLH -2981.4633

save(gb_fina30y, file = "Libraries_data/Data/Fina_30y_M_norm_par_Chol.R")

load(file = "Libraries_data/Data/Fina_30y_M_norm_par_Chol.R")

fina30y_obj <- gas_M(static_param = gb_fina30y$pars,
                  data = rbind(returns[,3],returns[,8]),
                  score_fun = grad_norm_M,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 9,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

save(fina30y_obj, file = "Libraries_data/Data/Fina_30y_M_norm_obj_Chol.R")

