##### FILTERING OUT JUMPS OF COUPLED CIR PROCESS
# Define a custom callback function
def print_progress(xk, convergence):
    print(f"Iteration: {print_progress.iteration}")
    print(f"Best Parameters: {xk}")
    print(f"Convergence Metric: {convergence}")
    print("-" * 30)
    print_progress.iteration += 1
    return False  # Return True to stop optimization early

print_progress.iteration = 1  # Initialize iteration counter

num_workers = multiprocessing.cpu_count()-2

# BOUNDS ON ESTIMATED PARAMETERS
bounds = [ (50,500),(50,500),(0.60,0.99),(0.60,0.99) ]

# FITTING DATA BY DIFFERENTIAL EVOLUTION
opt_coupled_cir = differential_evolution(func = coupled_cir, bounds = bounds, args = (data_gas, data_el, 1/252, True, pars2, 0.85), callback=print_progress, disp=True,  updating='deferred',  maxiter=400, tol = 0.0000001, popsize = 60,workers = num_workers,  mutation=(0.5, 1.5), recombination=0.7, strategy = 'best1exp', polish=False)

# PARAMETERS FOR THE FINAL VALUES OF COUPLED CIR PROCESS ARE ESTIMATED USING THE DATA WHERE NO JUMP HAVE BEEN DETECTED

# SAVING OPTIMAL OBJECT
# save_object(opt_coupled_cir, 'DATA_OUT/fitted_parameters.pkl')


# LOADING OPTIMAL VALUES OF PARAMETERS
# opt_coupled_cir = load_object('DATA_OUT/fitted_parameters.pkl')

fig_out, cir_el , cir_gas, jump_el, jump_gas, kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas,fig_out2 = coupled_cir(pars = opt_coupled_cir.x, el = data_el, gas = data_gas, dt = 1/252, opt = False, pars2= pars2)

fig_out.show()
fig_out2.show()

fig_jumps = go.Figure( go.Histogram( x = jump_el[ jump_el != 0 ]))
fig_jumps.add_trace( go.Histogram( x = jump_gas[ jump_gas != 0 ]))
fig_jumps.update_layout(barmode='overlay')
fig_jumps.update_traces(opacity=0.75)

fig_jumps.show()

####################################################################################################
# FILTERING OUT JUMPS OF COUPLED CIR PROCESS
####################################################################################################
##### FILTERING OUT JUMPS OF COUPLED CIR PROCESS
# Define a custom callback function
def print_progress(xk, convergence):
    print(f"Iteration: {print_progress.iteration}")
    print(f"Best Parameters: {xk}")
    print(f"Convergence Metric: {convergence}")
    print("-" * 30)
    # print_progress.iteration += 1
    return False  # Return True to stop optimization early

print_progress.iteration = 1  # Initialize iteration counter

num_workers = multiprocessing.cpu_count()-2

# BOUNDS ON ESTIMATED PARAMETERS
bounds = [ (60,500),(60,500),(0.60,0.99),(0.60,0.99) ]

percentiles = [0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,0.95]
percentiles= np.arange(0.05,1,0.01)
params_out = np.zeros((30,4))

for i in range(0,30):
    
    print(i)
    print_progress.iteration = i  # Initialize iteration counter
    opt_coupled_cir = differential_evolution(func = coupled_cir, bounds = bounds, args = (data_gas, data_el, 1/252, True, pars2, 0.05), callback=print_progress, disp=True,  updating='deferred',  maxiter=500, tol = 0.0000001, popsize = 60,workers = num_workers,  mutation=(0.5, 1.5), recombination=0.7, strategy = 'best1exp', polish=True)

    params_out[i,:] = opt_coupled_cir.x

# save_object(params_out, 'DATA_OUT/params_out_JB2.pkl')

params_out = load_object('DATA_OUT/params_out_JB2.pkl')

fig_out, cir_el , cir_gas, jump_el, jump_gas, kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas,fig_out2 = coupled_cir(pars = params_out[0,:], el = data_el, gas = data_gas, dt = 1/252, opt = False, pars2= pars2, perc = 0.85)

fig_out.show()
fig_out2.show()

out_pars = np.zeros((len(percentiles),6))
out_pars[:,0:4] = params_out
out_pars[:,4] = percentiles

for i in range(0,len(percentiles)):
  
    print(i)
    out_pars[i,5] = coupled_cir(pars = out_pars[i,0:4], el = data_el, gas = data_gas, dt = 1/252, opt = True, pars2= pars2, perc = out_pars[i,4])


fig_out = go.Figure(
    data = go.Scatter(x = out_pars[:,4], y = out_pars[:,5], name="Percentile vs Distance",)
)

fig_out.show()


####################################################################################################
# FITTING CIR PROCESSES ON DATA WITHOUT JUMPS
####################################################################################################

fig_out, cir_el , cir_gas, jump_el, jump_gas, kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas,fig_out2 = coupled_cir(pars = params_out[0,:], el = data_el.copy(), gas = data_gas.copy(), dt = 1/252, opt = False, pars2= pars2)

dt = 1/252
# | ( (data['Date'] >= '2021-06-01') & (data['Date'] < '2023-01-01') )
ind = np.where( (jump_el != 0) | (jump_gas != 0)  | ( (data['Date'] >= '2020-01-01') & (data['Date'] < '2023-01-01') ) )[0] 

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

correlation = np.corrcoef(fit_mod_gas.resid, fit_mod_el.resid)

num_workers = multiprocessing.cpu_count()-2

# FITTING JUMP DISTRIBUTIONS
bounds = [ (0,6),  (-4, 0), (0,5), (0.001, 5), (0.001,1) ]

jumps_el = jump_el[ jump_el != 0 ]
jumps_gas = jump_gas[ jump_gas != 0 ]

fig_jumps = go.Figure( go.Histogram( x = jump_el[ jump_el != 0 ]))
fig_jumps.add_trace( go.Histogram( x = jump_gas[ jump_gas != 0 ]))
fig_jumps.update_layout(barmode='overlay')
fig_jumps.update_traces(opacity=0.75)
fig_jumps.show()

# FITTING DATA BY DIFFERENTIAL EVOLUTION
opt_el_jumps = differential_evolution(
  func = mle_jump_distribution, 
  bounds = bounds,
  args = (jumps_el,), 
  disp = True,  
  updating='deferred',  
  maxiter=800, 
  tol = 0.00000001, 
  popsize = 45,
  workers = num_workers, 
  mutation  = (0.8,1.2), 
  recombination = 0.4, 
  strategy = 'best1exp')

opt_gas_jumps = differential_evolution(
  func = mle_jump_distribution, 
  bounds = bounds,
  args = (jumps_gas,), 
  disp = True,  
  updating='deferred',  
  maxiter=800, 
  tol = 0.00000001, 
  popsize = 45,
  workers = num_workers, 
  mutation  = (0.8,1.2), 
  recombination = 0.4, 
  strategy = 'best1exp')


# FITTING HAWKESS PROCESS DYNAMICS
bounds = [ (0,100),  (0, 100), (0,100), (0,100)]

opt_el_int = differential_evolution(
  func = MLE_Hawkess_int, 
  bounds = bounds,
  args = (jump_el[0:2150], jump_gas[0:2150], 1/252), 
  disp = True,  
  updating='deferred',  
  maxiter=800, 
  tol = 0.00000001, 
  popsize = 30,
  workers = num_workers, 
  mutation  = (0.8,1.2), 
  recombination = 0.4, 
  strategy = 'best1exp')

# 
opt_gas_int = differential_evolution(
  func = MLE_Hawkess_int, 
  bounds = bounds,
  args = (jump_gas[0:2150], jump_el[0:2150],  1/252), 
  disp = True,  
  updating='deferred',  
  maxiter=800, 
  tol = 0.00000001, 
  popsize = 30,
  workers = num_workers, 
  mutation  = (0.8,1.2), 
  recombination = 0.4, 
  strategy = 'best1exp')

pars_e = [kappa_el, alpha_el, sigma_el]

pars_g = [kappa_gas, alpha_gas, sigma_gas]

pars_le = np.append(params_out[0,0], opt_el_int.x) 
pars_lg = np.append(params_out[0,1],opt_gas_int.x) 

pars_jump_el = opt_el_jumps.x
pars_jump_gas = opt_gas_jumps.x

init_values = [cir_el[-1], cir_gas[-1], jump_el[-1], jump_gas[-1],8.983073543442092,8.243466778449351]

# save_object(pars_e, 'DATA_OUT/pars_e.pkl')

# save_object(pars_g, 'DATA_OUT/pars_g.pkl')

# save_object(pars_jump_el, 'DATA_OUT/opt_el_jumps.pkl')

# save_object(pars_jump_gas, 'DATA_OUT/opt_gas_jumps.pkl')

# save_object(pars_le, 'DATA_OUT/opt_el_int.pkl')

# save_object(pars_lg, 'DATA_OUT/opt_gas_int.pkl')

# save_object(init_values, 'DATA_OUT/init_values.pkl')


# GRAPHS 

plt.rcParams.update({
    "font.family": "serif",
    "font.size": 12,
    "axes.labelsize": 12,
    "legend.fontsize": 11,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "figure.figsize": (7, 5),
    "axes.grid": True,
    "grid.alpha": 0.3,
})

fig, axs = plt.subplots(2, 1, sharex=True)

# Electricity plot
axs[0].plot(data["Date"], cir_el, label="Continous part of the Electricity price", 
           color="#00897D",linestyle='-',  linewidth=1)
axs[0].plot(data["Date"], data_el - cir_el, label="Jump part of the Electricity price", 
            color="#FF6B6B", linestyle='-', linewidth=1)

axs[0].set_ylabel("Electricity [EUR/MWh]")
axs[0].legend(loc="upper left")

# Gas plot
axs[1].plot(data["Date"], cir_gas, label="Continous part of the Gas price", 
            color="#00897D",linestyle='-',  linewidth=1)
axs[1].plot(data["Date"], data_gas - cir_gas, label="Jump part of the Gas price", 
            color="#FF6B6B", linestyle='-', linewidth=1)

axs[1].set_ylabel("Gas [EUR/MWh]")
axs[1].legend(loc="upper left")

# Common X label
axs[1].set_xlabel("Date")
fig.autofmt_xdate()
plt.tight_layout()

# Save as figure
plt.savefig("GRAPHS/Factored_processes.png", dpi=300)

plt.show()


# Apply scientific style
plt.rcParams.update({
    "font.family": "serif",
    "font.size": 12,
    "axes.labelsize": 12,
    "legend.fontsize": 11,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "figure.figsize": (7, 5),
    "axes.grid": True,
    "grid.alpha": 0.3,
})

# Filter non-zero jumps
jump_el_nonzero = jump_el[jump_el != 0]
jump_gas_nonzero = jump_gas[jump_gas != 0]

# Create histogram
fig, ax = plt.subplots()
ax.hist(jump_el_nonzero, bins=50, alpha=0.6, label="Electricity", color="#00897D", edgecolor="black")
ax.hist(jump_gas_nonzero, bins=50, alpha=0.6, label="Gas", color="#FF6B6B", edgecolor="black")

# Labels and legend
ax.set_xlabel("Jump Size")
ax.set_ylabel("Frequency")
ax.legend()
ax.grid(True, linestyle='--', alpha=0.5)


plt.tight_layout()

plt.savefig("GRAPHS/Jump_histograms.png", dpi=300)
plt.show()
