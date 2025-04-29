#################################################################
#################################################################
#########################  Yield2y  #############################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,1],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load(file = "Libraries_data/Data/Y2y_t_111_Inv_Chol2.R")

gb_yield2y <-  solnp(pars = opt_yield2y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# -2119.2021
# save(gb_yield2y, file = "Libraries_data/Data/Yield_2y_U_t_par.R")
save(gb_yield2y, file = "Libraries_data/Data/Yield_2y_U_t_par_Chol.R")


load(file = "Libraries_data/Data/Yield_2y_U_t_par_Chol.R")

yield2y_obj <- gas_U(static_param = gb_yield2y$pars,                
                  data = returns[,1],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)



save(yield2y_obj, file = "Libraries_data/Data/Yield_2y_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  YIELD10y  #############################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,2],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load( file = "Libraries_data/Data/Y10y_t_111_Inv_Chol2.R")

gb_yield10y <-  solnp(pars = opt_yield10y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# -2098.701
# save(gb_yield10y, file = "Libraries_data/Data/Yield_10y_U_t_par.R")
save(gb_yield10y, file = "Libraries_data/Data/Yield_10y_U_t_par_Chol.R")

load(file = "Libraries_data/Data/Yield_10y_U_t_par_Chol.R")
yield10y_obj <- gas_U(static_param = gb_yield10y$pars,                
                  data = returns[,2],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)
#save(yield10y_obj, file = "Libraries_data/Data/Yield_10y_U_t_obj.R")
save(yield10y_obj, file = "Libraries_data/Data/Yield_10y_U_t_obj_Chol.R")


#################################################################
#################################################################
######################### YIELD30y  #############################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,3],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load( file = "Libraries_data/Data/Y30y_t_111_Inv_Chol2.R")

gb_yield30y <-  solnp(pars = yield30y$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -1009.58

# save(gb_yield30y, file = "Libraries_data/Data/Yield_30y_U_t_par.R")
save(gb_yield30y, file = "Libraries_data/Data/Yield_30y_U_t_par_Chol.R")

load( file = "Libraries_data/Data/Yield_30y_U_t_par_Chol.R")

yield30y_obj <-  gas_U(static_param = gb_yield30y$pars,                
                  data = returns[,3],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)

#save(yield30y_obj, file = "Libraries_data/Data/Yield_30y_U_t_obj.R")
save(yield30y_obj, file = "Libraries_data/Data/Yield_30y_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  util  ################################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,4],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  long_mu = Start_par$util,
                  start_param = Start_par$util,
                  n_tv_param = 3,
                  dist = "t",
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


load( file = "Libraries_data/Data/util_t_111_Inv_Chol2.R")

gb_util <-  solnp(pars = opt_util$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -1791.463

# save(gb_util, file = "Libraries_data/Data/util_U_t_par.R")
save(gb_util, file = "Libraries_data/Data/util_U_t_par_Chol.R")


load(file = "Libraries_data/Data/util_U_t_par_Chol.R")

util_obj <- gas_U(static_param = gb_util$pars,                
                  data = returns[,4],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)

#save(util_obj, file = "Libraries_data/Data/util_U_t_obj.R")
save(util_obj, file = "Libraries_data/Data/util_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  cogo  ################################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,5],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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


load( file = "Libraries_data/Data/cogo_t_111_Inv_Chol2.R")

gb_cogo <-  solnp(pars = opt_cogo$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -2202.194

# save(gb_cogo, file = "Libraries_data/Data/cogo_U_t_par.R")
 save(gb_cogo, file = "Libraries_data/Data/cogo_U_t_par_Chol.R")

load(file = "Libraries_data/Data/cogo_U_t_par_Chol.R")

cogo_obj <- gas_U(static_param = gb_cogo$pars,                
                  data = returns[,5],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)

#save(cogo_obj, file = "Libraries_data/Data/cogo_U_t_obj.R")
save(cogo_obj, file = "Libraries_data/Data/cogo_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  INDY  ################################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,6],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load( file = "Libraries_data/Data/Indy_t_111_Inv_Chol2.R")

gb_indy <-  solnp(pars = opt_indy$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -2148.403

# save(gb_indy, file = "Libraries_data/Data/Indy_U_t_par.R")
 save(gb_indy, file = "Libraries_data/Data/Indy_U_t_par_Chol.R")

load(file = "Libraries_data/Data/Indy_U_t_par_Chol.R")

indy_obj <- gas_U(static_param = gb_indy$pars,                
                  data = returns[,6],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)

#save(indy_obj, file = "Libraries_data/Data/Indy_U_t_obj.R")
save(indy_obj, file = "Libraries_data/Data/Indy_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  TECH  ################################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,7],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load(file = "Libraries_data/Data/Tech_t_111_Inv_Chol2.R")

gb_tech <-  solnp(pars = opt_tech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -2068.325

#save(gb_tech, file = "Libraries_data/Data/Tech_U_t_par.R")
save(gb_tech, file = "Libraries_data/Data/Tech_U_t_par_Chol.R")

load(file = "Libraries_data/Data/Tech_U_t_par_Chol.R")
tech_obj <- gas_U(static_param = gb_tech$pars,                
                  data = returns[,7],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)

#save(tech_obj, file = "Libraries_data/Data/Tech_U_t_obj.R")

save(tech_obj, file = "Libraries_data/Data/Tech_U_t_obj_Chol.R")

#################################################################
#################################################################
#########################  FINA  ################################
#################################################################
#################################################################

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                
                  data = returns[,8],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
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

load( file = "Libraries_data/Data/Fina_t_111_Inv_Chol2.R")

gb_fina <-  solnp(pars = opt_fina$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-10,2)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,2)) )   

# LLH -1951.867

# save(gb_fina, file = "Libraries_data/Data/Fina_U_t_par.R")
save(gb_fina, file = "Libraries_data/Data/Fina_U_t_par_Chol.R")


load(file = "Libraries_data/Data/Fina_U_t_par_Chol.R")

fina_obj <- gas_U(static_param = gb_fina$pars,                
                  data = returns[,8],
                  score_fun = grad_t_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "t",
                  Inv_h = -1,
                  out = TRUE)
#save(fina_obj, file = "Libraries_data/Data/Fina_U_t_obj.R")\
save(fina_obj, file = "Libraries_data/Data/Fina_U_t_obj_Chol.R")




# load(file = "Libraries_data/Data/Yield_2y_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Yield_10y_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Yield_30y_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Util_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Cogo_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Indy_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Tech_U_t_par_Chol.R")
# load(file = "Libraries_data/Data/Fina_U_t_par_Chol.R")


# nu_u <- c(gb_yield2y$pars[7],
#   gb_yield10y$pars[7],
#   gb_yield30y$pars[7],
#   gb_util$pars[7],
#   gb_cogo$pars[7],
#   gb_indy$pars[7],
#   gb_tech$pars[7],
#   gb_fina$pars[7])



save(nu_u, file = "Libraries_data/Data/Nu_U_t_obj_Chol.R")
