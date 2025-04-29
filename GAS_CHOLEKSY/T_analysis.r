# Object for normal distributions

### MULTIVARIATE

# objects slo_hun_obj
load(file = "Libraries_data/Data/Slo_hun_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_svk_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_ger_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_pol_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_swi_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Slo_aust_M_t_obj_chol.R")



# objects ger_hun_obj
load(file = "Libraries_data/Data/Ger_hung_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Ger_svk_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Ger_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Ger_pol_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Ger_swis_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Ger_aust_M_t_obj_chol.R")


# objects swis_hun_obj
load(file = "Libraries_data/Data/Swis_hung_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Swis_svk_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Swis_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Swis_pol_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Swis_aust_M_t_obj_chol.R")


# objects slo_hun_obj
load(file = "Libraries_data/Data/Hung_svk_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Hung_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Hung_pol_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Hung_aust_M_t_obj_chol.R")


# objects slo_hun_obj
load(file = "Libraries_data/Data/Pol_svk_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Pol_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Pol_aust_M_t_obj_chol.R")


# objects slo_hun_obj
load(file = "Libraries_data/Data/Svk_cech_M_t_obj_chol.R")
load(file = "Libraries_data/Data/Svk_aust_M_t_obj_chol.R")


# objects slo_hun_obj
load(file = "Libraries_data/Data/Aust_cech_M_t_obj_chol.R")


### UNIVARIATE

# yield2y_obj
load(file = "Libraries_data/Data/Cech_U_t_obj_Chol.R")
# yield10Y_obj
load(file = "Libraries_data/Data/Slo_U_t_obj_Chol.R")
# yield30Y_obj
load(file = "Libraries_data/Data/Svk_U_t_obj_Chol.R")
# Util_obj
load(file = "Libraries_data/Data/Ger_U_t_obj_Chol.R")
# Cogo_obj
load(file = "Libraries_data/Data/Hung_U_t_obj_Chol.R")
# indy_obj
load(file = "Libraries_data/Data/Pol_U_t_obj_Chol.R")
# tech_obj
load(file = "Libraries_data/Data/Swis_U_t_obj_Chol.R")
# fina_obj
load(file = "Libraries_data/Data/Aust_U_t_obj_Chol.R")

################################################################################
##################### SLOVINSKO

# CESKO - Sedi
slo_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_cech_obj$param_out, UNI= FALSE,nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_cech_ent[c("entr_x","entr_y","test_x","test_y")]


# RAKUSKO - Sedi
slo_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_aust_ent[c("entr_x","entr_y","test_x","test_y")]

# SLOVENSKO - Sedi
slo_svk_ent <- Entropy_fun_t(Obj_x = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_svk_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_svk_ent[c("entr_x","entr_y","test_x","test_y")]

# POLSKO - Sedi
slo_pol_ent <- Entropy_fun_t(Obj_x = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_pol_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_pol_ent[c("entr_x","entr_y","test_x","test_y")]


# MADARSKO - Sedi
slo_hung_ent <- Entropy_fun_t(Obj_x = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_hun_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_hung_ent[c("entr_x","entr_y","test_x","test_y")]

# SVAJCIARSKO - Sedi
slo_swis_ent <- Entropy_fun_t(Obj_x = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_swi_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_swis_ent[c("entr_x","entr_y","test_x","test_y")]

# NEMECKO - Sedi
slo_ger_ent <- Entropy_fun_t(Obj_x = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(slo_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(slo_ger_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

slo_ger_ent[c("entr_x","entr_y","test_x","test_y")]


##################### NEMECKO

# CESKO - Sedi - Mozno sa oplati este raz pozriet
ger_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_cech_ent[c("entr_x","entr_y","test_x","test_y")]

# RAKUSKO - Sedi
ger_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha =  c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_aust_ent[c("entr_x","entr_y","test_x","test_y")]

# SLOVENSKO - Sedi 
ger_svk_ent <- Entropy_fun_t(Obj_x = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_svk_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_svk_ent[c("entr_x","entr_y","test_x","test_y")]

# POLSKO - Sedi
ger_pol_ent <- Entropy_fun_t(Obj_x = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_pol_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_pol_ent[c("entr_x","entr_y","test_x","test_y")]

# MADARSKO - Sedi - TOTO by sa este tiez oplatilo pozriet mozno
ger_hung_ent <- Entropy_fun_t(Obj_x = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_hung_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_hung_ent[c("entr_x","entr_y","test_x","test_y")]

# SVAJCIARSKO - Sedi
ger_swis_ent <- Entropy_fun_t(Obj_x = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(ger_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(ger_swis_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

ger_swis_ent[c("entr_x","entr_y","test_x","test_y")]



##################### SVAJCIARSKO

# CESKO - Sedi
swis_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(swis_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

swis_cech_ent[c("entr_x","entr_y","test_x","test_y")]


# RAKUSKO - Sedi
swis_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(swis_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

swis_aust_ent[c("entr_x","entr_y","test_x","test_y")]

# SLOVENSKO - Sedi
swis_svk_ent <- Entropy_fun_t(Obj_x = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(swis_svk_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

swis_svk_ent[c("entr_x","entr_y","test_x","test_y")]

# POLSKO - Sedi
swis_pol_ent <- Entropy_fun_t(Obj_x = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(swis_pol_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

swis_pol_ent[c("entr_x","entr_y","test_x","test_y")]

# MADARSKO - Sedi
swis_hung_ent <- Entropy_fun_t(Obj_x = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(swis_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(swis_hung_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

swis_hung_ent[c("entr_x","entr_y","test_x","test_y")]

##################### MADARSKO

# CESKO - Sedi
hung_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(hung_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

hung_cech_ent[c("entr_x","entr_y","test_x","test_y")]

# RAKUSKO - Sedi
hung_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(hung_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

hung_aust_ent[c("entr_x","entr_y","test_x","test_y")]

# SLOVENSKO - Sedi
hung_svk_ent <- Entropy_fun_t(Obj_x = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(hung_svk_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

hung_svk_ent[c("entr_x","entr_y","test_x","test_y")]

# POLSKO - Sedi
hung_pol_ent <- Entropy_fun_t(Obj_x = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(hung_pol_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

hung_pol_ent[c("entr_x","entr_y","test_x","test_y")]

# Obj_x = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10)
# Obj_y = ret_func(hung_obj$param_out, UNI= TRUE, nu = 10)
# Obj_xy = ret_func(hung_pol_obj$param_out, UNI= FALSE, nu = 10)

# plot(Obj_x$sigma, type ='l')
# points(Obj_xy$x_sigma, type = 'l', col = 'red')

# plot(Obj_x$sigma_c, type ='l')
# points(Obj_xy$x_sigma_c, type = 'l', col = 'red')

# plot(Obj_y$sigma, type ='l')
# points(Obj_xy$y_sigma, type = 'l', col = 'red')

# plot(Obj_y$sigma_c, type ='l')
# points(Obj_xy$y_sigma_c, type = 'l', col = 'red')

##################### POLSKO

# CESKO - Sedi
pol_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(pol_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

pol_cech_ent[c("entr_x","entr_y","test_x","test_y")]
 

# RAKUSKO - Sedi
pol_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(pol_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

pol_aust_ent[c("entr_x","entr_y","test_x","test_y")]

# SLOVENSKO - Sedi
pol_svk_ent <- Entropy_fun_t(Obj_x = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(pol_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(pol_svk_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

pol_svk_ent[c("entr_x","entr_y","test_x","test_y")]


##################### SLOVENSKO

# CESKO - Sedi - POZRIET ESTE RAZ
svk_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(svk_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

svk_cech_ent[c("entr_x","entr_y","test_x","test_y")]


# RAKUSKO - Sedi
svk_aust_ent <- Entropy_fun_t(Obj_x = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(svk_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(svk_aust_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

svk_aust_ent[c("entr_x","entr_y","test_x","test_y")]


##################### RAKUSKO

# CESKO - Sedi 
aust_cech_ent <- Entropy_fun_t(Obj_x = ret_func(cech_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_y = ret_func(aust_obj$param_out, UNI= TRUE, nu = 10), 
                          Obj_xy = ret_func(aust_cech_obj$param_out, UNI= FALSE, nu = 10), 
                          alpha = c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5), wind = 252)

aust_cech_ent[c("entr_x","entr_y","test_x","test_y")]



table_all <- c()

test_all <- c()

# SLOVINSKO

table_all <- rbind(
slo_cech_ent$entr_y,
slo_aust_ent$entr_y,
slo_svk_ent$entr_y,
slo_pol_ent$entr_y,
slo_hung_ent$entr_y,
slo_swis_ent$entr_y,
slo_ger_ent$entr_y,

# NEMECKO
ger_cech_ent$entr_y,
ger_aust_ent$entr_y,
ger_svk_ent$entr_y,
ger_pol_ent$entr_y,
ger_hung_ent$entr_y,
ger_swis_ent$entr_y,
slo_ger_ent$entr_x,

# SVAJCIARKO
swis_cech_ent$entr_y,
swis_aust_ent$entr_y,
swis_svk_ent$entr_y,
swis_pol_ent$entr_y,
swis_hung_ent$entr_y,
ger_swis_ent$entr_x,
slo_swis_ent$entr_x,

# MADARSKO
hung_cech_ent$entr_y,
hung_aust_ent$entr_y,
hung_svk_ent$entr_y,
hung_pol_ent$entr_y,
swis_hung_ent$entr_x,
ger_hung_ent$entr_x,
slo_hung_ent$entr_x,

# POLSKO

pol_cech_ent$entr_y,
pol_aust_ent$entr_y,
pol_svk_ent$entr_y,
hung_pol_ent$entr_x,
swis_pol_ent$entr_x,
ger_pol_ent$entr_x,
slo_pol_ent$entr_x,

# SLOVENSKO

svk_cech_ent$entr_y,
svk_aust_ent$entr_y,
pol_svk_ent$entr_x,
hung_svk_ent$entr_x,
swis_svk_ent$entr_x,
ger_svk_ent$entr_x,
slo_svk_ent$entr_x,

# RAKUSKO

aust_cech_ent$entr_y,
svk_aust_ent$entr_x,
pol_aust_ent$entr_x,
hung_aust_ent$entr_x,
swis_aust_ent$entr_x,
ger_aust_ent$entr_x,
slo_aust_ent$entr_x,

# CESKOR

aust_cech_ent$entr_x,
svk_cech_ent$entr_x,
pol_cech_ent$entr_x,
hung_cech_ent$entr_x,
swis_cech_ent$entr_x,
ger_cech_ent$entr_x,
slo_cech_ent$entr_x)


test_all <- rbind(
slo_cech_ent$test_y,
slo_aust_ent$test_y,
slo_svk_ent$test_y,
slo_pol_ent$test_y,
slo_hung_ent$test_y,
slo_swis_ent$test_y,
slo_ger_ent$test_y,

# NEMECKO
ger_cech_ent$test_y,
ger_aust_ent$test_y,
ger_svk_ent$test_y,
ger_pol_ent$test_y,
ger_hung_ent$test_y,
ger_swis_ent$test_y,
slo_ger_ent$test_x,

# SVAJCIARKO
swis_cech_ent$test_y,
swis_aust_ent$test_y,
swis_svk_ent$test_y,
swis_pol_ent$test_y,
swis_hung_ent$test_y,
ger_swis_ent$test_x,
slo_swis_ent$test_x,

# MADARSKO
hung_cech_ent$test_y,
hung_aust_ent$test_y,
hung_svk_ent$test_y,
hung_pol_ent$test_y,
swis_hung_ent$test_x,
ger_hung_ent$test_x,
slo_hung_ent$test_x,

# POLSKO

pol_cech_ent$test_y,
pol_aust_ent$test_y,
pol_svk_ent$test_y,
hung_pol_ent$test_x,
swis_pol_ent$test_x,
ger_pol_ent$test_x,
slo_pol_ent$test_x,

# SLOVENSKO

svk_cech_ent$test_y,
svk_aust_ent$test_y,
pol_svk_ent$test_x,
hung_svk_ent$test_x,
swis_svk_ent$test_x,
ger_svk_ent$test_x,
slo_svk_ent$test_x,

# RAKUSKO

aust_cech_ent$test_y,
svk_aust_ent$test_x,
pol_aust_ent$test_x,
hung_aust_ent$test_x,
swis_aust_ent$test_x,
ger_aust_ent$test_x,
slo_aust_ent$test_x,

# CESKOR

aust_cech_ent$test_x,
svk_cech_ent$test_x,
pol_cech_ent$test_x,
hung_cech_ent$test_x,
swis_cech_ent$test_x,
ger_cech_ent$test_x,
slo_cech_ent$test_x)



colnames(table_all) <- c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5)

rownames(table_all) <- c( "CZE$\\rightarrow$SLO",
                          "AUS$\\rightarrow$SLO",
                          "SVK$\\rightarrow$SLO",
                          "POL$\\rightarrow$SLO",
                          "HUN$\\rightarrow$SLO",
                          "SWI$\\rightarrow$SLO",
                          "GER$\\rightarrow$SLO",

                          "CZE$\\rightarrow$GER",
                          "AUS$\\rightarrow$GER",
                          "SVK$\\rightarrow$GER",
                          "POL$\\rightarrow$GER",
                          "HUN$\\rightarrow$GER",
                          "SWI$\\rightarrow$GER",
                          "SLO$\\rightarrow$GER",

                          "CZE$\\rightarrow$SWI",
                          "AUS$\\rightarrow$SWI",
                          "SVK$\\rightarrow$SWI",
                          "POL$\\rightarrow$SWI",
                          "HUN$\\rightarrow$SWI",
                          "GER$\\rightarrow$SWI",
                          "SLO$\\rightarrow$SWI",

                          "CZE$\\rightarrow$HUN",
                          "AUS$\\rightarrow$HUN",
                          "SVK$\\rightarrow$HUN",
                          "POL$\\rightarrow$HUN",
                          "SWI$\\rightarrow$HUN",
                          "GER$\\rightarrow$HUN",
                          "SLO$\\rightarrow$HUN",

                          "CZE$\\rightarrow$POL",
                          "AUS$\\rightarrow$POL",
                          "SVK$\\rightarrow$POL",
                          "HUN$\\rightarrow$POL",
                          "SWI$\\rightarrow$POL",
                          "GER$\\rightarrow$POL",
                          "SLO$\\rightarrow$POL",

                          "CZE$\\rightarrow$SVK",
                          "AUS$\\rightarrow$SVK",
                          "POL$\\rightarrow$SVK",
                          "HUN$\\rightarrow$SVK",
                          "SWI$\\rightarrow$SVK",
                          "GER$\\rightarrow$SVK",
                          "SLO$\\rightarrow$SVK",

                          "CZE$\\rightarrow$AUS",
                          "SVK$\\rightarrow$AUS",
                          "POL$\\rightarrow$AUS",
                          "HUN$\\rightarrow$AUS",
                          "SWI$\\rightarrow$AUS",
                          "GER$\\rightarrow$AUS",
                          "SLO$\\rightarrow$AUS",

                          "AUS$\\rightarrow$CZE",
                          "SVK$\\rightarrow$CZE",
                          "POL$\\rightarrow$CZE",
                          "HUN$\\rightarrow$CZE",
                          "SWI$\\rightarrow$CZE",
                          "GER$\\rightarrow$CZE",
                          "SLO$\\rightarrow$CZE"
                          )


options(scipen = 999)
table_all <- trunc(table_all*10^4)/10^4

table_all <- as.data.frame(table_all)

table_all[test_all < 0.05] <- paste0("\\pmb{",table_all[test_all < 0.05],"}")

test_all <- trunc(test_all*10^4)/10^4
test_all <- as.data.frame(test_all)

rownames(test_all) <- rep("", nrow(test_all))

colnames(test_all) <- c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5)

test_all <- apply(test_all, 2, function(x) paste0("(", x, ")") )

both_all <- c()
for(i in 1:nrow(table_all)){
  both_all <- rbind(both_all, rbind(table_all[i,],test_all[i,]))
}


Hmisc::latex(both_all[1:56,], file = '', caption = "Forcasting period", title = " $\\alpha$", math.col.names=TRUE)

Hmisc::latex(both_all[57:112,], file = '', caption = "Forcasting period", title = " $\\alpha$", math.col.names=TRUE)

# Graphical representation

cc <- c(scales::seq_gradient_pal("#00897D", "#FF6B6B",  "Lab")(seq(0,0.7,length.out=4)),
        scales::seq_gradient_pal("#8531d3", "#FF6B6B",  "Lab")(seq(0,1,length.out=3)))

time_ind <- data_all$Date[data_all$Date >= "2019-06-01" & data_all$Date < "2024-06-01"]

graph_entr <- function(x, name_out){

  x <- t(x)
  colnames(x) <- c(0.2, 0.5, 0.8, 1, 1.5, 2, 2.5)

  x <- as.data.frame(x)
  time_ind <- time_ind[128:(length(time_ind)-127)]

  x$time <- time_ind

  x_long <- x |> pivot_longer(-time, values_to = "Entropy", names_to = "alpha-level")

  p <- ggplot(x_long, aes(x = time, y = Entropy, group = `alpha-level`, color = `alpha-level`))+geom_line()+
   geom_point(size =0)+
  scale_colour_manual(values=cc, name = "\u03b1 -level")+
    theme_light() + 
    theme(strip.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 25),
          axis.text.x = element_text(size = 25),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size =25),
          legend.title =  element_text(size =25),
          legend.direction = "horizontal",
          legend.position="bottom") + 
    guides(colour = guide_legend(nrow = 1,override.aes= list(size = 15,shape = 15)))

    ggplot2::ggsave(paste0("Graphs/",name_out,"_t.png"),
                    p,width = 2400,
                    height = 1200,units = "px",
                    scale = 2,
                    dpi = 300)

}


graph_entr(x = slo_aust_ent$ts_entr_y, name_out = "AUS_SLO")
graph_entr(x = slo_svk_ent$ts_entr_y, name_out = "SVK_SLO")
graph_entr(x = slo_swis_ent$ts_entr_y, name_out = "SWI_SLO")

graph_entr(x = hung_pol_ent$ts_entr_y, name_out = "POL_HUN")
graph_entr(x = pol_cech_ent$ts_entr_x, name_out = "POL_CZE")



Obj_x = ret_func(aust_obj$param_out, UNI= TRUE)
Obj_y = ret_func(slo_obj$param_out, UNI= TRUE)
Obj_xy = ret_func(slo_aust_obj$param_out, UNI= FALSE)

plot(Obj_x$sigma_c, type ='l')
points(Obj_xy$x_sigma_c, type ='l',col = 'red')

aus_df <- data.frame(`Cond Itself` = Obj_x$sigma_c,
                     `Cond Slo` = Obj_xy$x_sigma_c, 
                     time = time_ind[-c(1,2)])

aus_long <- aus_df |> pivot_longer(-time, values_to = "Sigma", names_to = "Orig_vs_cond")

# aus_long$Orig_vs_cond <- factor(aus_long$Orig_vs_cond, levels =c("Cond.Itself","Cond.Slo")  )

aus_long$Orig_vs_cond[aus_long$Orig_vs_cond == "Cond.Itself"] <- "Conditioned on itself"
aus_long$Orig_vs_cond[aus_long$Orig_vs_cond == "Cond.Slo"] <- "Conditioned on Slovenia"

p_new <- ggplot(aus_long, aes( x = time, y = Sigma, group = Orig_vs_cond, color = Orig_vs_cond))+geom_line() +geom_point(size =0)+
  scale_colour_manual(values=c("black","#66CDAA"), name = "")+
    theme_light() + 
    theme(strip.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 25),
          axis.text.x = element_text(size = 25),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text = element_text(size =25),
          legend.title =  element_text(size =25),
          legend.direction = "horizontal",
          legend.position="bottom") + 
    guides(colour = guide_legend(nrow = 1,override.aes= list(size = 15,shape = 15)))

p_new <- p_new +
geom_ellipse(aes(
    x0 = 19300,     # x-coordinate of the center
    y0 = 0.0085,     # y-coordinate of the center
    a = 400,      # Semi-major axis length
    b = 0.0025,      # Semi-minor axis length
    angle = 0   # Rotation angle in radians
  ), fill = NA, color = "red")+
geom_ellipse(aes(
    x0 = 18358,     # x-coordinate of the center
    y0 = 0.0123,     # y-coordinate of the center
    a = 100,      # Semi-major axis length
    b = 0.0055,      # Semi-minor axis length
    angle = 0   # Rotation angle in radians
  ), fill = NA, color = "red")


ggplot2::ggsave(paste0("Graphs/AUS_SIGMA.png"),
                    p_new,width = 2400,
                    height = 1200,units = "px",
                    scale = 2,
                    dpi = 300)