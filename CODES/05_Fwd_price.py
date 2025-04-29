####################################################################################################
# ORIGINAL FWD PRICES
####################################################################################################
gas_fwd = gas_fwd[['Date','2025-01-06']]
gas_fwd.index = gas_fwd['Date']
gas_fwd = gas_fwd.drop(['Date'],axis = 1)
gas_fwd = gas_fwd[gas_fwd.index >= '2025-01-01']
gas_fwd = gas_fwd[gas_fwd.index < '2028-01-01']
gas_fwd.columns = ['Price']

el_fwd = el_fwd[['Date','2025-01-06']]
el_fwd.index = el_fwd['Date']
el_fwd = el_fwd.drop(['Date'],axis = 1)
el_fwd = el_fwd[el_fwd.index >= '2025-01-01']
el_fwd = el_fwd[el_fwd.index  < '2028-01-01']
el_fwd['2025-01-01'] = el_fwd.groupby(el_fwd.index.date)['2025-01-06'].transform('mean')
el_fwd = el_fwd.resample('D').first()  
el_fwd = el_fwd.drop(columns=['2025-01-06'])
el_fwd.columns = ['Price']

fwd_df = pd.merge(gas_fwd,el_fwd, left_index=True, right_index=True, suffixes = ['_gas','_el'])
fwd_df = fwd_df[fwd_df.index.dayofweek < 5]

fwd_df['year'] = fwd_df.index.year - 2015


fwd_df['weekday_index'] = fwd_df.index.to_series().apply(lambda x: np.busday_count(f'{x.year}-01-01', x.strftime('%Y-%m-%d')) + 1)

fwd_df['total_weekdays'] = fwd_df.index.year.map(lambda y: np.busday_count(f'{y}-01-01', f'{y}-12-31') + 1)

fwd_df['Time'] = fwd_df['weekday_index'] / fwd_df['total_weekdays'] + fwd_df['year']

fwd_df['Ind'] = np.arange(0,len(fwd_df['Time']))

# 
start_end_ind = np.zeros((16,2))
# Get the first business day of 2025
start_2025_week = fwd_df.index[fwd_df.index >= '2025-01-06'][0]

ind_row = 0
# For weekly contract weights
for i in range(1, 6):
    ind = fwd_df.loc[start_2025_week + pd.DateOffset(weeks=i-1): start_2025_week + pd.DateOffset(weeks=i) - pd.DateOffset(days=1)]['Ind']
    start_end_ind[ind_row,0] = ind.min()
    start_end_ind[ind_row,1] = ind.max()
    ind_row += 1

# For monthly contract weights
start_2025 = fwd_df.index[fwd_df.index >= '2025-01-01'][0]
for i in range(1, 7):
    ind = fwd_df.loc[start_2025 + pd.DateOffset(months=i-1): start_2025 + pd.DateOffset(months=i) - pd.DateOffset(days=1)]['Ind']
    start_end_ind[ind_row,0] = ind.min()
    start_end_ind[ind_row,1] = ind.max()
    ind_row += 1


# For quarterly contract weights
for i in range(1, 5):
    ind = fwd_df.loc[start_2025 + pd.DateOffset(months=3*(i-1)): start_2025 + pd.DateOffset(months=3*i) - pd.DateOffset(days=1)]['Ind']
    start_end_ind[ind_row,0] = ind.min()
    start_end_ind[ind_row,1] = ind.max()
    ind_row += 1


# For yearly contract weights
for i in range(1, 2):
    ind = fwd_df.loc[start_2025 + pd.DateOffset(years=i-1): start_2025 + pd.DateOffset(years=i) - pd.DateOffset(days=1)]['Ind']
    start_end_ind[ind_row,0] = ind.min()
    start_end_ind[ind_row,1] = ind.max()
    ind_row += 1

fwd_df = fwd_df.reset_index(drop=False)

start_end_ind = start_end_ind.astype(int)

price_el_fwd = np.zeros(start_end_ind.shape[0])
price_gas_fwd = np.zeros(start_end_ind.shape[0])

for i in range(0,len(price_el_fwd)):
    price_el_fwd[i] = fwd_df.iloc[np.arange(start_end_ind[i,0],start_end_ind[i,1]+1)]['Price_el'].mean()
    price_gas_fwd[i]= fwd_df.iloc[np.arange(start_end_ind[i,0],start_end_ind[i,1]+1)]['Price_gas'].mean()


####################################################################################################
# SEASONAL VALUE FOR THOSE DATES AND TIME FOR DAILY FORWARDS
####################################################################################################
# adding seasonal function to original data with all variables
fwd_df['Seas_el'] = seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = np.array(fwd_df['Time']) ) 

fwd_df['Seas_gas'] =seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = np.array(fwd_df['Time']) )  

fwd_df['Time'] = fwd_df['Time'] - 10

fwd_df = fwd_df.loc[fwd_df['Date'] < '2026-01-01']




####################################################################################################
# OPTIMIZATION OF PARAMETERS
#################################################################################################### 

def print_progress(xk, convergence):
    print(f"Iteration: {print_progress.iteration}")
    print(f"Best Parameters: {xk}")
    print(f"Convergence Metric: {convergence}")
    print("-" * 30)
    # print_progress.iteration += 1
    return False  # Return True to stop optimization early


num_workers = multiprocessing.cpu_count()-2

# BOUNDS ON ESTIMATED PARAMETERS
bounds = [ (-122,30),(-1.7,30),(-30,30),(-30,30) ]


pars_theta_Q = np.zeros((30,5))

for i in range(0,30):
  print_progress.iteration = i
  opt_risk_pars = differential_evolution(func = risk_neutral_fwd_price, 
                                      bounds = bounds, 
                                      args =(pars_e.copy(), 
                                            pars_g.copy(), 
                                            pars_le.copy(), 
                                            pars_lg.copy(), 
                                            pars_jump_el.copy(), 
                                            pars_jump_gas.copy(),
                                            fwd_df['Seas_el'].copy(), 
                                            fwd_df['Seas_gas'].copy(), 
                                            init_values.copy(), 
                                            fwd_df['Time'].copy(), 
                                            price_el_fwd.copy(), 
                                            price_gas_fwd.copy(),                                     
                                            start_end_ind.copy(),
                                            True), 
                                        callback=print_progress, 
                                        disp=True,  
                                        updating='deferred',  
                                        maxiter=500, 
                                        tol = 0.0000001, 
                                        popsize = 60,
                                        workers = num_workers,  
                                        mutation=(0.5, 1.5), 
                                        recombination=0.7, 
                                        strategy = 'best1exp', 
                                        polish=True)
  pars_theta_Q[i,0] = opt_risk_pars.fun
  pars_theta_Q[i,1:5] = opt_risk_pars.x

# save_object(pars_theta_Q, 'DATA_OUT/params_theta.pkl')
pars_theta_Q = load_object('DATA_OUT/params_theta.pkl')

# minimal function
pars_theta_Q = pars_theta_Q[pars_theta_Q[:, 0] == pars_theta_Q[:, 0].min(), :]

out_fwd, cir_pars_e_Q, cir_pars_g_Q, pars_e_Q, pars_g_Q, jump_pars_e_Q, jump_pars_g_Q, initial_val_Q = risk_neutral_fwd_price(pars= pars_theta_Q[0,1:5], 
                                cir_pars_e = pars_e.copy(), 
                                cir_pars_g = pars_g.copy(), 
                                me_pars_e = pars_le.copy(), 
                                me_pars_g =  pars_lg.copy(), 
                                jump_pars_e = pars_jump_el.copy(), 
                                jump_pars_g = pars_jump_gas.copy(),
                                seasonal_e = fwd_df['Seas_el'].copy(), 
                                seasonal_g = fwd_df['Seas_gas'].copy(), 
                                init_values = init_values.copy(), 
                                time = fwd_df['Time'].copy(), 
                                fwd_e =  price_el_fwd.copy(), 
                                fwd_g =  price_gas_fwd.copy(), 
                                opt = False,
                                start_end= start_end_ind.copy())


swap_el = np.zeros(start_end_ind.shape[0])
swap_gas = np.zeros(start_end_ind.shape[0])

for i in range(0,len(swap_el)):
    swap_el[i] = out_fwd[np.arange(start_end_ind[i,0],start_end_ind[i,1]+1),0].mean()
    swap_gas[i]= out_fwd[np.arange(start_end_ind[i,0],start_end_ind[i,1]+1),1].mean()


# Electricity 
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime

# Delivery periods for electricity (from previous table)
elec_contracts = [
    ("2025-01-01", "2025-01-07", 120.05),
    ("2025-01-08", "2025-01-14", 116.82),
    ("2025-01-15", "2025-01-21", 116.66),
    ("2025-01-22", "2025-01-28", 120.99),
    ("2025-01-29", "2025-02-04", 153.12),
    ("2025-01-01", "2025-01-31", 116.70),
    ("2025-02-01", "2025-02-28", 148.99),
    ("2025-03-01", "2025-03-31", 111.34),
    ("2025-04-01", "2025-04-30", 96.05),
    ("2025-05-01", "2025-05-31", 86.12),
    ("2025-06-01", "2025-06-30", 98.17),
    ("2025-01-01", "2025-03-31", 125.03),
    ("2025-04-01", "2025-06-30", 93.37),
    ("2025-07-01", "2025-09-30", 120.05),
    ("2025-10-01", "2025-12-31", 128.07),
    ("2025-01-01", "2025-12-31", 116.66)
]

# Model-generated electricity prices (same order as contracts)
model_prices = np.array([
    123.98471495, 127.71999251, 126.60092469, 124.80231969,
    122.96590951, 122.9959702, 120.38340567, 114.17629604,
    109.43899343, 106.71572476, 106.16542805, 119.2855882,
    107.4596583, 110.15837569, 116.56005551, 113.34318733
])


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

fig, ax = plt.subplots()

# Plot: Electricity Market vs Model Prices
# fig, ax = plt.subplots(figsize=(12, 6))

for idx, (start_str, end_str, market_price) in enumerate(elec_contracts):
    start = datetime.strptime(start_str, "%Y-%m-%d")
    end = datetime.strptime(end_str, "%Y-%m-%d")
    # Market price line
    ax.hlines(market_price, xmin=start, xmax=end, colors="#00897D", linewidth=1, label='Market Price' if idx == 0 else "")
    # Model price line
    ax.hlines(model_prices[idx], xmin=start, xmax=end, colors="#FF6B6B", linewidth=1, linestyles='dashed', label='Model Price' if idx == 0 else "")

# Format x-axis as dates
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.xticks(rotation=45)

# Labels and legend
ax.set_ylabel("Electricity Price [EUR/MWh]")
ax.legend(loc="upper right")
ax.grid(True, linestyle=':', alpha=0.6)

plt.tight_layout()

plt.savefig("GRAPHS/Elec_market_vs_model.png", dpi=300)
plt.show()


# GAS

# Delivery periods for electricity (from previous table)
elec_contracts = [
    ("2025-01-01", "2025-01-07", 47.06879178),
    ("2025-01-08", "2025-01-14", 45.58279589),
    ("2025-01-15", "2025-01-21", 46.3155143),
    ("2025-01-22", "2025-01-28", 47.09742293),
    ("2025-01-29", "2025-02-04", 47.45798684),
    ("2025-01-01", "2025-01-31", 46.81863587),
    ("2025-02-01", "2025-02-28", 47.43476924),
    ("2025-03-01", "2025-03-31", 47.46894772),
    ("2025-04-01", "2025-04-30", 47.25288342),
    ("2025-05-01", "2025-05-31", 47.11709096),
    ("2025-06-01", "2025-06-30", 47.0008501),
    ("2025-01-01", "2025-03-31", 47.22456112),
    ("2025-04-01", "2025-06-30", 47.12549675),
    ("2025-07-01", "2025-09-30", 46.86291863),
    ("2025-10-01", "2025-12-31", 44.99533429),
    ("2025-01-01", "2025-12-31", 46.54472756)
]

# Model-generated electricity prices (same order as contracts)
model_prices = swap_gas

# Plot: Electricity Market vs Model Prices
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

fig, ax = plt.subplots()

for idx, (start_str, end_str, market_price) in enumerate(elec_contracts):
    start = datetime.strptime(start_str, "%Y-%m-%d")
    end = datetime.strptime(end_str, "%Y-%m-%d")
    # Market price line
    ax.hlines(market_price, xmin=start, xmax=end, colors="#00897D", linewidth=1, label='Market Price' if idx == 0 else "")
    # Model price line
    ax.hlines(model_prices[idx], xmin=start, xmax=end, colors="#FF6B6B", linewidth=1, linestyles='dashed', label='Model Price' if idx == 0 else "")

# Format x-axis as dates
ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.xticks(rotation=45)

# Labels and legend
ax.set_ylabel("Gas Price [EUR/MWh]")
ax.legend(loc="upper left")
ax.grid(True, linestyle=':', alpha=0.6)

plt.tight_layout()

plt.savefig("GRAPHS/Gas_market_vs_model.png", dpi=300)
plt.show()
