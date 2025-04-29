cir_el = np.copy(data_el)

cir_gas = np.copy(data_gas)

date_new = np.copy(data['Date'])

dt = 1/252
# | ( (data['Date'] >= '2021-06-01') & (data['Date'] < '2023-01-01') )
ind = np.where( (cir_el < 0) | (cir_gas < 0) )[0] 

Nsteps = len(cir_el)

cir_el[ ind] = np.nan

cir_gas[ ind] = np.nan

# define variables

# print(jump_cnt)
rs_el = cir_el[:Nsteps - 1]  
rt_el = cir_el[1:Nsteps]

# print(jump_cnt)
rs_gas = cir_gas[:Nsteps - 1]  
rt_gas = cir_gas[1:Nsteps]

# feature engineering to fit the theoretical model for electricity
y_el = (rt_el - rs_el) / np.sqrt(rs_el)

# righ side
z1 = (rs_gas - 1)*dt/np.sqrt(rs_el)
z2 = (1- rs_el)*dt/np.sqrt(rs_el)

x_el = np.column_stack((z1, z2))

# fitting model by OLS
# model_el = sm.RLM(y_el, x_el, M=sm.robust.norms.HuberT(), missing = 'drop')
model_el = sm.OLS(y_el, x_el, missing = 'drop')
fit_mod_el = model_el.fit()

# Parameters estimate of CIR process
kappa_el = fit_mod_el.params[1]
alpha_el = fit_mod_el.params[0]/kappa_el

sigma_el = np.std(fit_mod_el.resid)/np.sqrt(dt)

# feature engineering to fit the theoretical model for natural gas
y_gas = (rt_gas - rs_gas) / np.sqrt(rs_gas)
# right side
# z1 = (rs_el - 1)*dt/np.sqrt(rs_gas)
# z2 = (1- rs_gas)*dt/np.sqrt(rs_gas)

# x_gas = np.column_stack((z1, z2))

x_gas = (1- rs_gas)*dt/np.sqrt(rs_gas)
x_gas = x_gas.reshape(-1,1)
# fitting model by OLS
# model_gas = sm.OLS(y_gas, x_gas, missing = 'drop')
model_gas = sm.OLS(y_gas, x_gas, missing = 'drop')
fit_mod_gas = model_gas.fit()
fit_mod_gas.params

# # Parameters estimates of CIR process
# kappa_gas = fit_mod_gas.params[1]
# alpha_gas = fit_mod_gas.params[0]/kappa_gas
kappa_gas = fit_mod_gas.params[0]
sigma_gas = np.std(fit_mod_gas.resid)/np.sqrt(dt)
alpha_gas = 0

pars2 = [kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas]


# #### UPRAVA SIGIEM
# cir_el = np.copy(data_el)

# cir_gas = np.copy(data_gas)

# date_new = np.copy(data['Date'])

# dt = 1/252

# ind = np.where( (cir_el < 0) | (cir_gas < 0) )[0] 

# Nsteps = len(cir_el)

# cir_el[ ind] = np.nan

# cir_gas[ ind] = np.nan

# # define variables

# # print(jump_cnt)
# rs_el = cir_el[:Nsteps - 1]  
# rt_el = cir_el[1:Nsteps]

# # print(jump_cnt)
# rs_gas = cir_gas[:Nsteps - 1]  
# rt_gas = cir_gas[1:Nsteps]

# # feature engineering to fit the theoretical model for electricity
# y_el = (rt_el - rs_el) / np.sqrt(rs_el)

# # righ side
# z1 = (rs_gas - 1)*dt/np.sqrt(rs_el)
# z2 = (1- rs_el)*dt/np.sqrt(rs_el)

# resid_el = (y_el - fit_mod_el.params[0]*z1 - fit_mod_el.params[1]*z2)

# sigma_el = np.nanstd(resid_el)/np.sqrt(dt)

# # feature engineering to fit the theoretical model for natural gas
# y_gas = (rt_gas - rs_gas) / np.sqrt(rs_gas)
# # right side
# z1 = (rs_el - 1)*dt/np.sqrt(rs_gas)
# z2 = (1- rs_gas)*dt/np.sqrt(rs_gas)

# resid_gas = (y_gas - fit_mod_gas.params[0]*z1 - fit_mod_gas.params[1]*z2)

# sigma_gas = np.nanstd(resid_gas)/np.sqrt(dt)

# pars2 = [kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas]

# fig_out = go.Figure(
#     data = go.Scatter(x = x_gas[:,0], y =y_gas, name="Orig to other",   mode='markers',)
# )

# fig_out.add_trace(go.Scatter(
#     x = x_gas[:,1], y =y_gas, name="Orig to orig",  mode='markers',
# ))

# fig_out.show()


# fig_out = go.Figure(
#     data = go.Scatter(x = x_el[:,0], y =y_el, name="Orig to other",   mode='markers',)
# )

# fig_out.add_trace(go.Scatter(
#     x = x_el[:,1], y =y_el, name="Orig to orig",  mode='markers',
# ))

# fig_out.show()

# fig_out = go.Figure(
#     data = go.Scatter(y = x_el[:,0], name="z1",   mode='markers',)
# )

# fig_out.add_trace(go.Scatter(
#     y = x_el[:,1], name="z2",  mode='markers',
# ))

# fig_out.add_trace(go.Scatter(
#     y =y_el, name="el",  mode='markers',
# ))

# fig_out.show()


# fig_out = go.Figure(
#     data = go.Scatter(y = x_gas[:,0], name="z1",   mode='markers',)
# )

# fig_out.add_trace(go.Scatter(
#     y = x_gas[:,1], name="z2",  mode='markers',
# ))

# fig_out.add_trace(go.Scatter(
#     y =y_gas, name="el",  mode='markers',
# ))

# fig_out.show()

cir_el = np.copy(data_el)

cir_gas = np.copy(data_gas)

date_new = np.copy(data['Date'])

dt = 1/252

ind = np.where( (cir_el < 0) | (cir_gas < 0) )[0] 

Nsteps = len(cir_el)

cir_el[ ind] = np.nan

cir_gas[ ind] = np.nan

# define variables

# print(jump_cnt)
rs_el = cir_el[:Nsteps - 1]  
rt_el = cir_el[1:Nsteps]

# print(jump_cnt)
rs_gas = cir_gas[:Nsteps - 1]  
rt_gas = cir_gas[1:Nsteps]

# feature engineering to fit the theoretical model for electricity
y_el = (rt_el - rs_el) / np.sqrt(rs_el)

# righ side
z1 = (rs_gas - 1)*dt/np.sqrt(rs_el)
z2 = (1- rs_el)*dt/np.sqrt(rs_el)

x_el = np.column_stack((z1, z2))

# fitting model by OLS
model_el = sm.RLM(y_el, x_el, M=sm.robust.norms.HuberT(), missing = 'drop')
# model_el = sm.OLS(y_el, x_el, missing = 'drop')
fit_mod_el = model_el.fit()

# Parameters estimate of CIR process
kappa_el = fit_mod_el.params[1]
alpha_el = fit_mod_el.params[0]/kappa_el

sigma_el = np.std(fit_mod_el.resid)/np.sqrt(dt)

# feature engineering to fit the theoretical model for natural gas
y_gas = (rt_gas - rs_gas) / np.sqrt(rs_gas)
# right side
z1 = (rs_el - 1)*dt/np.sqrt(rs_gas)
z2 = (1- rs_gas)*dt/np.sqrt(rs_gas)

x_gas = np.column_stack((z1, z2))

# fitting model by OLS
# model_gas = sm.OLS(y_gas, x_gas, missing = 'drop')
model_gas = sm.OLS(y_gas, x_gas, missing = 'drop')
fit_mod_gas = model_gas.fit()
fit_mod_gas.params

# Parameters estimates of CIR process
kappa_gas = fit_mod_gas.params[1]
alpha_gas = fit_mod_gas.params[0]/kappa_gas

sigma_gas = np.std(fit_mod_gas.resid)/np.sqrt(dt)

pars2 = [kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas]

fit_mod_gas.resid

fit_mod_el.resid

correlation = np.corrcoef(fit_mod_gas.resid, fit_mod_el.resid)
