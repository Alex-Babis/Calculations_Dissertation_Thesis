set.seed(12345)

# 10% data will be missing
ind_del <- sample(1:(nrow(ger_yield)*ncol(ger_yield)),floor((nrow(ger_yield)*ncol(ger_yield))/10),replace = FALSE)
ger_yield_w <- ger_yield
ger_yield_w[ind_del] <- NA

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield_w,
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


nCores <- detectCores() - 2
cl <- makeCluster(nCores)

de_opt_NS_norm_inv_L <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-3, rep(0.00000000000000001,9)), 
                upper =  c(rep(1.5,4),rep(0.9999999999999999,4),rep(10,3),1, rep(0.1,9)), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 2000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield_w","score_norm", "mean_yield" ,"ginv_2","gas_cMVN")))
 


save(de_opt_NS_norm_inv, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L_miss.R")
# DONE

# GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L_miss.R")

opt_NS_norm_inv_L <- solnp(pars = de_opt_NS_norm_inv_L$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10, rep(0.00000000000000001,9)), 
                UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3, rep(0.1,9)))   

# FINAL LLH -> -6404.587

save(opt_NS_norm_inv_L, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L_miss.R")

# Optimization with focus to long maturities rather then short

# new weights for volatilities that will prefer long maturities
# will emd at 1 at zero, so the longer maturities have lower volatility
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L_miss.R")


NS_norm_inv_L_for <- gas_cMVN(static_param = opt_NS_norm_inv_L$pars,               
                    data = rbind(ger_yield_w,pred_ger_yield_w),
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_NS_L)


metrics_fun(NS_norm_inv_L_for$llh[,897:976], met = "llh", par_cnt = NS_norm_inv_L_for$par_cnt)

metrics_fun(NS_norm_inv_L_for$llh[,1:896], met = "llh",par_cnt = NS_norm_inv_L_for$par_cnt)

metrics_fun(dlhodobe$values[,1:896], met = "mse")

plot(dlhodobe$pars['tv_sigma',], type = 'l')

####################################################################################
# POVODNE LAMBDA
w_long <- exp(-c(1/4,1/2,1,3,5,7,10,20,30)/20)
w_long <- w_long/min(w_long)
#w_long <- c(rep(10,7),rep(1,2))

de_func <- function(x){

    out <- tryCatch(
        {
            gas_cMVN(static_param = x,               
                    data = ger_yield_w,
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
                    start_param = NULL,
                    sigma_weights = w_long)
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

de_opt_NS_norm_inv_L_w <- DEoptim(fn = de_func, lower = c(rep(0,4),rep(0,4),rep(-10,3),-3,0.00000000000000001), 
                upper =  c(rep(1.5,4),rep(0.9999999999999999,4),rep(10,3),1, 0.1), control = DEoptim.control(
                CR = 0.8,F = 1.2, c = 0.2,
                itermax = 2000, strategy = 1, 
                parallelType = 1,
                cluster = cl,
                packages = c("mvnfast","rlist","tidyr","tidyverse","reshape2"),
                parVar = c("ger_yield_w","score_norm", "mean_yield" ,"ginv_2","gas_cMVN","w_long")))
 


save(de_opt_NS_norm_inv_L_w, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L_w.R")
# DONE

# GRADIENT OPTIMIZATION - de_opt_NS_norm_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_L_w.R")

opt_NS_norm_inv_L_w <- solnp(pars = de_opt_NS_norm_inv_L_w$optim$bestmem, 
                fun = de_func,
                LB =  c(rep(-Inf,4),rep(-0.9999999999999999,4),rep(-10,3),-10, 0.00000000000000001), 
                UB =  c(rep(Inf,4),rep(0.9999999999999999,4),rep(10,3),3, 0.1))   

# FINAL LLH -> -6404.587

save(opt_NS_norm_inv_L_w, file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L_w.R")

# Optimization with focus to long maturities rather then short

# new weights for volatilities that will prefer long maturities
# will emd at 1 at zero, so the longer maturities have lower volatility
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L_w.R")


NS_norm_inv_L_for_w <- gas_cMVN(static_param = opt_NS_norm_inv_L_w$pars,               
                    data = rbind(ger_yield_w,pred_ger_yield_w),
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_NS_L,
                    sigma_weights = w_long)

metrics_fun(NS_norm_inv_L_for$llh[,1:896], met = "llh",par_cnt = NS_norm_inv_L_for$par_cnt)

metrics_fun(NS_norm_inv_L_for_w$llh[,1:896], met = "llh",par_cnt = NS_norm_inv_L_for_w$par_cnt)

metrics_fun(NS_norm_inv_L_for$values[,1:896], met = "mse")

metrics_fun(NS_norm_inv_L_for_w$values[,1:896], met = "mse")


metrics_fun(NS_norm_inv_L_for$llh[,897:976], met = "llh", par_cnt = NS_norm_inv_L_for$par_cnt)

metrics_fun(NS_norm_inv_L_for_w$llh[,897:976], met = "llh", par_cnt = NS_norm_inv_L_for_w$par_cnt)

val <- metrics_fun(NS_norm_inv_L_for$values[,897:976], met = "mse")

val2 <- metrics_fun(NS_norm_inv_L_for_w$values[,897:976], met = "mse")

df_pred_w <- rbind(val, val2)
colnames(df_pred_w) <- c("3M", "6M", "12M","36M","60M","84M","120M","240M","360M","All")
rownames(df_pred_w) <- c("Original","Weighted")

df_pred_w <- round(df_pred_w, digits = 5)
    
df_pred_w5 <- df_pred_w[,1:5]
df_pred_w10 <- df_pred_w[,6:10]

Hmisc::latex(df_pred_w5, file = '', caption = "Forecasted period")

Hmisc::latex(df_pred_w10, file = '', caption = "Forecasted period")
