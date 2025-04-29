##### Normal Distribution
#pred_ger_yield
# opt_NS_norm_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt.R")

NS_norm_inv_b_for <- gas_cMVN(static_param = opt_NS_norm_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
# -6321.276

# opt_NS_norm_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt.R")

NS_norm_sqrt_b_for <- gas_cMVN(static_param = opt_NS_norm_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
# -5731.981

# opt_NSS_norm_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt.R")

NSS_norm_inv_b_for <-  gas_cMVN(static_param = opt_NSS_norm_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
# -7116.394

# opt_NSS_norm_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt.R")

NSS_norm_sqrt_b_for <- gas_cMVN(static_param = opt_NSS_norm_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
# -6236.023

# opt_BL_norm_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt.R")

BL_norm_inv_b_for <- gas_cMVN(static_param = opt_BL_norm_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)


 #  -6321.278


# opt_BL_norm_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt.R")

BL_norm_sqrt_b_for <- gas_cMVN(static_param = opt_BL_norm_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)
# -5814.224

# opt_NS_norm_inv_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_L.R")

NS_norm_inv_L_for <- gas_cMVN(static_param = opt_NS_norm_inv_L$pars,               
                    data = pred_ger_yield,
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


# -6404.587

# opt_NS_norm_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt_L.R")

NS_norm_sqrt_L_for <- gas_cMVN(static_param = opt_NS_norm_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_NS_L)
# -5957.007

# opt_NSS_norm_inv_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt_L.R")

NSS_norm_inv_L_for <- gas_cMVN(static_param = opt_NSS_norm_inv_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)


# -7343.36

# opt_NSS_norm_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt_L.R")

NSS_norm_sqrt_L_for <- gas_cMVN(static_param = opt_NSS_norm_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)
# -6810.29

# opt_BL_norm_inv_L 
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt_L.R")

BL_norm_inv_L_for <- gas_cMVN(static_param =opt_BL_norm_inv_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)


# -6998.997


# opt_BL_norm_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt_L.R")

BL_norm_sqrt_L_for <- gas_cMVN(static_param = opt_BL_norm_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_BL_L)
#  -6054.352

# opt_NS_norm_inv_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_inv_opt_cov.R")

NS_norm_inv_cov_for <- gas_cMVN(static_param = opt_NS_norm_inv_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_NS_L)
# -7532.354


# opt_NS_norm_sqrt_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_norm_sqrt_opt_cov.R")
NS_norm_sqrt_cov_for <- gas_cMVN(static_param = opt_NS_norm_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_NS_L)
#  -6736.809

# opt_NSS_norm_inv_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_inv_opt_cov.R")
NSS_norm_inv_cov_for <- gas_cMVN(static_param = opt_NSS_norm_inv_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = NULL)
#  -8730.953

# opt_NSS_norm_sqrt_cov
load( file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_norm_sqrt_opt_cov.R")
NSS_norm_sqrt_cov_for <- gas_cMVN(static_param = opt_NSS_norm_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)
# -7729.65


# opt_BL_norm_inv_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_inv_opt_cov2.R")

BL_norm_inv_cov_for <- gas_cMVN(static_param = opt_BL_norm_inv_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_BL_L)



# -8470.193

# opt_BL_norm_sqrt_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_norm_sqrt_opt_cov.R")
BL_norm_sqrt_cov_for <- gas_cMVN(static_param = opt_BL_norm_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_norm,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "norm",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_BL_L)
# -7583.085

##### T Distribution

# opt_NS_t_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt.R")

NS_t_inv_b_for <- gas_cMVN(static_param = opt_NS_t_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
#  -6388.878

# opt_NS_t_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt.R")

NS_t_sqrt_b_for <- gas_cMVN(static_param = opt_NS_t_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,FALSE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0)
# -5880.031

# opt_NSS_t_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt.R")

NSS_t_inv_b_for <- gas_cMVN(static_param = opt_NSS_t_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
# -6740.329

# opt_NSS_t_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt.R")

NSS_t_sqrt_b_for <- gas_cMVN(static_param = opt_NSS_t_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5)
# -6378.886

# opt_BL_t_inv
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt.R")

BL_t_inv_b_for <- gas_cMVN(static_param = opt_BL_t_inv$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)
#-6389.025


# opt_BL_t_sqrt
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt.R")

BL_t_sqrt_b_for <- gas_cMVN(static_param = opt_BL_t_sqrt$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, FALSE, FALSE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0)
#  -5991.155

# opt_NS_t_inv_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt_L.R")

NS_t_inv_L_for <- gas_cMVN(static_param = opt_NS_t_inv_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_NS_L)
#  -6467.879

# opt_NS_t_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt_L.R")

NS_t_sqrt_L_for <-  gas_cMVN(static_param = opt_NS_t_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_NS_L)
# -6091.173

# opt_NSS_t_inv_L --- SPUSTENE --- stale zle
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt_L.R")

NSS_t_inv_L_for <- gas_cMVN(static_param = opt_NSS_t_inv_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)

# -7224.416

# opt_NSS_t_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt_L.R")

NSS_t_sqrt_L_for <- gas_cMVN(static_param = opt_NSS_t_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = FALSE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)
# -7118.979

# opt_BL_t_inv_L --- Bolo by dobre to este raz pozriet --- ale uz bolo optimalizovane nanovo
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt_L_2.R")


BL_t_inv_L_for <- gas_cMVN(static_param = opt_BL_t_inv_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = NULL)

plot(BL_t_inv_L_for$pars['lambda2',], type = 'l')

# -7089.349



# opt_BL_t_sqrt_L
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt_L.R")

BL_t_sqrt_L_for <- gas_cMVN(static_param = opt_BL_t_sqrt_L$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = FALSE,
                    delta_l = 0,
                    start_param = start_BL_L)
# -6770.139

# opt_NS_t_inv_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_inv_opt_cov.R")
NS_t_inv_cov_for <- gas_cMVN(static_param = opt_NS_t_inv_cov$pars,               
            data = pred_ger_yield,
            score_fun = score_t,
            gas_a_lag = 1,
            gas_b_lag = 1,
            tau = c(1/4,1/2,1,3,5,7,10,20,30),
            out = TRUE,
            distr = "t",
            tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
            h = -1,
            type = "NS",
            dcov = TRUE,
            delta_l = 0,
            start_param = start_NS_L)
# -7659.489
            

# opt_NS_t_sqrt_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NS_t_sqrt_opt_cov.R")
NS_t_sqrt_cov_for <- gas_cMVN(static_param = opt_NS_t_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE,TRUE),
                    h = -1/2,
                    type = "NS",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_NS_L)
# -6758.53


# opt_NSS_t_inv_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_inv_opt_cov.R")
NSS_t_inv_cov_for <- gas_cMVN(static_param = opt_NSS_t_inv_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)

score_mat = t(NSS_t_inv_cov_for$score_out)
cor(score_mat)

score_mat = as.data.frame(score_mat)
score_mat$x = 1:nrow(score_mat)

long_beta <- score_mat |> pivot_longer(-x, values_to = "Value", names_to = "Scores")

long_beta |> ggplot(aes( x = x, y = Value, group = Scores, color = Scores)) + geom_line()

# -8952.664

# opt_NSS_t_sqrt_cov
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/NSS_t_sqrt_opt_cov.R")
NSS_t_sqrt_cov_for <- gas_cMVN(static_param = opt_NSS_t_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "NSS",
                    dcov = TRUE,
                    delta_l = 0.5,
                    start_param = start_NSS_L)
# -7920.599

# opt_BL_t_inv_cov -TIETO DVE TREBA POZRIET --- zbehnute
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_inv_opt_cov.R")

BL_t_inv_cov_for <- gas_cMVN(static_param = opt_BL_t_inv_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_BL_L)

# 


# opt_BL_t_sqrt_cov - AJ TOTO TREBA POZRIET -0--zbehnute
load(file = "Nelson_Siegel_Svensson_Bliss/Output_data/BL_t_sqrt_opt_cov.R")
BL_t_sqrt_cov_for <- gas_cMVN(static_param = opt_BL_t_sqrt_cov$pars,               
                    data = pred_ger_yield,
                    score_fun = score_t,
                    gas_a_lag = 1,
                    gas_b_lag = 1,
                    tau = c(1/4,1/2,1,3,5,7,10,20,30),
                    out = TRUE,
                    distr = "t",
                    tv_par = c(TRUE,TRUE,TRUE, TRUE, TRUE,TRUE),
                    h = -1/2,
                    type = "BL",
                    dcov = TRUE,
                    delta_l = 0,
                    start_param = start_BL_L)

#  
