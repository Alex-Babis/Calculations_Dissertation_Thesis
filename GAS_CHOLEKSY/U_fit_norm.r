### CZECH 
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,1],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_cech <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_cech, file = "Libraries_data/Data/Cech_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Cech_norm_111_Inv_Chol.R")

gb_opt_cech <-  solnp(pars = opt_cech$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3)) )   

# 
save(gb_opt_cech, file = "Libraries_data/Data/Cech_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Cech_U_norm_par_Chol.R")

cech_obj <- gas_U(static_param = gb_opt_cech$pars,                
                  data = returns[,1],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)

# -1076.894

save(cech_obj, file = "Libraries_data/Data/Cech_U_norm_obj_Chol.R")

#### 

### AUSTRIA 
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,2],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_aust <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_aust, file = "Libraries_data/Data/Aust_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Aust_norm_111_Inv_Chol.R")

gb_opt_aust <-  solnp(pars = opt_aust$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3))  )   

 
save(gb_opt_aust, file = "Libraries_data/Data/Aust_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Aust_U_norm_par_Chol.R")

aust_obj <- gas_U(static_param = gb_opt_aust$pars,                
                  data = returns[,2],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(aust_obj, file = "Libraries_data/Data/Aust_U_norm_obj_Chol.R")


# SLOVAKIA
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,3],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_svk <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_svk, file = "Libraries_data/Data/Svk_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Svk_norm_111_Inv_Chol.R")

gb_opt_svk <-  solnp(pars = opt_svk$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3))  )   

 
save(gb_opt_svk, file = "Libraries_data/Data/Svk_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Svk_U_norm_par_Chol.R")

svk_obj <- gas_U(static_param = gb_opt_svk$pars,                
                  data = returns[,3],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(svk_obj, file = "Libraries_data/Data/Svk_U_norm_obj_Chol.R")


# Poland
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,4],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_pol <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_pol, file = "Libraries_data/Data/Pol_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Pol_norm_111_Inv_Chol.R")

gb_opt_pol <-  solnp(pars = opt_pol$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3)) )   

 
save(gb_opt_pol, file = "Libraries_data/Data/Pol_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Pol_U_norm_par_Chol.R")

pol_obj <- gas_U(static_param = gb_opt_pol$pars,                
                  data = returns[,4],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(pol_obj, file = "Libraries_data/Data/Pol_U_norm_obj_Chol.R")


# Hungary
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,5],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_hung <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_hung, file = "Libraries_data/Data/Hung_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Hung_norm_111_Inv_Chol.R")

gb_opt_hung <-  solnp(pars = opt_hung$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3)) )   

 
save(gb_opt_hung, file = "Libraries_data/Data/Hung_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Hung_U_norm_par_Chol.R")

hung_obj <- gas_U(static_param = gb_opt_hung$pars,                
                  data = returns[,5],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(hung_obj, file = "Libraries_data/Data/Hung_U_norm_obj_Chol.R")

# SWITZERLAND
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,6],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_swis <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_swis, file = "Libraries_data/Data/Swis_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Swis_norm_111_Inv_Chol.R")

gb_opt_swis <-  solnp(pars = opt_swis$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3))  )   

 
save(gb_opt_swis, file = "Libraries_data/Data/Swis_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Swis_U_norm_par_Chol.R")

swis_obj <- gas_U(static_param = gb_opt_swis$pars,                
                  data = returns[,6],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(swis_obj, file = "Libraries_data/Data/Swis_U_norm_obj_Chol.R")


# GERMANY 
de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,7],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_ger <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_ger, file = "Libraries_data/Data/Ger_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Ger_norm_111_Inv_Chol.R")

gb_opt_ger <-  solnp(pars = opt_ger$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3)) )   

 
save(gb_opt_ger, file = "Libraries_data/Data/Ger_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Ger_U_norm_par_Chol.R")

ger_obj <- gas_U(static_param = gb_opt_ger$pars,                
                  data = returns[,7],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(ger_obj, file = "Libraries_data/Data/Ger_U_norm_obj_Chol.R")


# SLOVENIA

de_func <- function(x){

    out <- tryCatch(
        {
            gas_U(static_param = x,                # parameter to be optimized
                  data = returns[,8],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
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

# opt_slo <- DEoptim(fn = de_func, lower =  c(rep(0,3),rep(0,3),-0.1,-10,-1.5), 
#                 upper = c(rep(0.2,3), rep(1,3),0.1,-3,1.5), control = DEoptim.control(
#                 CR = 0.8,
#                 itermax = 400, strategy = 6, c = 0.1, p = 0.2,
#                 parallelType = 1,
#                 cluster = cl,
#                 packages = c("mvnfast","rlist","extraDistr","Rfast","tidyr",
#                 "dplyr","MASS"),
#                 parVar = c("returns","grad_norm_U",
#                 "ginv_2","gas_U","cond_mu_sigma_uni")))



# save(opt_slo, file = "Libraries_data/Data/Slo_norm_111_Inv_Chol.R")

load(file = "Libraries_data/Data/Slo_norm_111_Inv_Chol.R")

gb_opt_slo <-  solnp(pars = opt_slo$optim$bestmem, 
                    fun = de_func,
                    LB =   c(rep(0,3),rep(0,3),rep(-13,3)), 
                    UB =   c(rep(1,3), rep(1,3),rep(8,3)) )   

 
save(gb_opt_slo, file = "Libraries_data/Data/Slo_U_norm_par_Chol.R")


load(file = "Libraries_data/Data/Slo_U_norm_par_Chol.R")

slo_obj <- gas_U(static_param = gb_opt_slo$pars,                
                  data = returns[,8],
                  score_fun = grad_norm_U,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  n_tv_param = 3,
                  dist = "norm",
                  Inv_h = -1,
                  out = TRUE)



save(slo_obj, file = "Libraries_data/Data/Slo_U_norm_obj_Chol.R")

###############################################################################

a <- ret_func(hung_obj$param_out)
b <- ret_func(slov_svk_obj_M$param_out, UNI = FALSE)

x <- a$sigma_c
y <- b$x_sigma_c
w_x <- w_y <- rep(1, length(x))
alpha <-2

entr <- alpha/(1-alpha) * (log( w_x%*%x^( (1-alpha)/(2*alpha) )) - log( w_y%*%y^( (1-alpha)/(2*alpha) )) )

test <- t.test(x =(x)^( (1-alpha)/(2*alpha)), y = (y)^( (1-alpha)/(2*alpha) ),  alternative = "two.sided")$p.value


plot(a$sigma_c, type ='l')
points(b$x_sigma_c, type ='l', col = 'red')


plot(a$sigma, type ='l')
points(a$sigma_c, type ='l', col = 'red')

plot(b$x_sigma, type ='l')
points(b$x_sigma_c, type ='l', col = 'red')


plot(x^( (1-alpha)/(2*alpha)), y = y^( (1-alpha)/(2*alpha) ))
abline(a = 0, b = 1, col = 'red')
hist(x^( (1-alpha)/(2*alpha)))
hist( y^( (1-alpha)/(2*alpha) ))

plot(b$x_sigma_c*0.98, type ='l')
points(b$x_sigma_c, type ='l', col = 'red')


plot(a$sigma, type ='l')
points(b$x_sigma, type ='l', col= 'red')

exp(-6)
plot(x,y)
abline(a = 0, b = 1, col = 'red')

log(min(x))
exp(-10)

plot(returns[,5],type = 'l')

