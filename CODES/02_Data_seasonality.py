############################################################################################ #
#                               IMPORTING DATA AND DATA                                      #
############################################################################################ #

def days_in_year(date):
    year = date.year
    return 366 if calendar.isleap(year) else 365

# Import of electricity data
ee_data = pd.read_excel('DATA_IN/data.xlsx', index_col=0)
ee_data = ee_data['EEXRTRB']

# Import of gas data
gas_data = pd.read_excel('DATA_IN/GAS-EOD-DA_THE_TTF_CEGH.xlsx', index_col=0)
gas_data = gas_data['TTF']

# Filtering just slovakian data for electricity price, where we have some ccgt 
# ee_data = ee_data[["SLOVAKIA"]].groupby(ee_data.index.date ).mean()

# Merging data together
data = pd.merge(ee_data, gas_data, left_index=True, right_index=True, how='outer')

# Drop rows with any NaN values
data = data.dropna()

data = data.reset_index(drop=False)

data.columns = ['Date','Elec', 'Gas']

# Deleting weekends, because the gas is not traded on those days
data = data.loc[ data['Date'].dt.dayofweek.lt(5) ]

data = data[ (data['Date'] >= '2016-01-01') ]

# creating changes in values / increments of procesesses
data['LG_elec'] = np.log(data['Elec'])

data['LG_gas'] = np.log(data['Gas'])

data['year'] = data['Date'].dt.year - 2015

# Calculate the weekday index within the year
data['weekday_index'] = data['Date'].apply(lambda x: np.busday_count(f'{x.year}-01-01', x.strftime('%Y-%m-%d')) + 1)

# Calculate the number of weekdays in the year
data['total_weekdays'] = data['Date'].dt.year.apply(lambda y: np.busday_count(f'{y}-01-01', f'{y}-12-31') + 1)

# Compute the time value
data['Time'] = data['weekday_index'] / data['total_weekdays'] + data['year']


# # Data manipulation to create time index in years
# base_date = date(data['Date'].min().date().year, 1, 1)

# data['Time'] = data['Date'].apply(lambda x: (x.date() - base_date).days)

# data['Time'] = data.apply(lambda row: row['Time'] / days_in_year(row['Date']), axis=1)

# Ploting graphs to see how they behave 

fig = go.Figure()

fig.add_trace(go.Scatter(
    name="Price of el",
    mode="lines", x=data["Date"], y=data["Elec"],
    line = dict(color='green')
))
fig.add_trace(go.Scatter(
    name="Price of gas",
    mode="lines", x=data["Date"], y=data["Gas"],
    line = dict(color='red')
))

fig.show()


# Set matplotlib to use a serif font, no LaTeX
plt.rcParams.update({
    "font.family": "serif",      # Use serif font (like Times New Roman)
    "font.size": 12,             # Default font size
    "axes.labelsize": 12,        # Axis label font size
    "legend.fontsize": 11,       # Legend font size
    "xtick.labelsize": 10,       # X-axis tick label size
    "ytick.labelsize": 10,       # Y-axis tick label size
    "figure.figsize": (6, 4),    # Figure size for single-column paper
    "axes.grid": True,           # Show grid for clarity
    "grid.alpha": 0.3,           # Grid transparency
})

# Create the figure and axis
fig, ax = plt.subplots(figsize=(7, 5))  # Suitable for single-column papers

# Plot electricity price
ax.plot(data["Date"], data["Elec"], label="Electricity price", color="#00897D",linestyle='-',  linewidth=1)

# Plot gas price
ax.plot(data["Date"], data["Gas"], label="Gas price", color="#FF6B6B", linestyle='--', linewidth=1)

# Label axes
ax.set_xlabel("Date")
ax.set_ylabel("Price [EUR/MWh]")

# Add legend
ax.legend(loc="upper left")

# Improve layout and formatting
fig.autofmt_xdate()  # Rotate date labels for readability
plt.tight_layout()

# Save as vector and high-res image
plt.savefig("GRAPHS/Spot_price_orig.png", dpi=300)    
plt.show()
           # for presentation use

# # 
# fig_inc = go.Figure()

# fig_inc.add_trace(go.Scatter(
#     name="Increments of el",
#     mode="lines", x=data["Date"], y=data["LG_elec"],
#     line = dict(color='green')
# ))

# fig_inc.add_trace(go.Scatter(
#     name="Increments of gas",
#     mode="lines", x=data["Date"], y=data["LG_gas"],
#     line = dict(color='red')
# ))

# fig_inc.show()


# #  mode="markers+lines", x=data["Date"], y=data["LG_gas"],
# #     marker_symbol="star",



############################################################################################ #
# Filtering seasonal and jump component from prices where diffusion is driven by CIR process #
############################################################################################ #

seas_data = data.loc[(data['Date'] < '2021-06-01') | (data['Date'] >= '2023-01-01')]

# # data.loc[(data['Date'] <= '2021-01-01')].shape
# # data.loc[(data['Date'] >= '2023-06-01')].shape

# # intercept1 = np.ones(1474 + 409, dtype=int)
# # intercept2 = np.zeros(1474 + 409, dtype=int)

# # intercept1[:1474] = 1
# # intercept2[1475:] = 1

# # creating matrix of Fourier terms
def fourier_mat( period, cnt_terms, time):

  if cnt_terms > period/2:
    print("PREKROCENIE MAXIMALNEHO POCTU CLENOV")

  terms = (np.arange(cnt_terms) + 1)
  terms = terms.reshape(1,-1)

  time = time.reshape(-1,1)

  # time to days
  time = time * 252

  const = np.ones(time.shape[0]).reshape(-1,1)

  cos_mat = np.cos(2*np.pi / period * time @ terms )
  sin_mat = np.sin(2*np.pi / period * time @ terms)

  ft_mat = np.concatenate((const,time, cos_mat, sin_mat), axis = 1)

  return ft_mat


# function that for given coeficient will create seasonal patern with linear trend and intercept
def seas_func( coef, period, cnt_terms, time):
  
  seas_mat = fourier_mat(period = period, cnt_terms = cnt_terms, time =time)

  seas = np.dot(seas_mat,coef)

  return seas 

# seasonal regressors, this will be comon for both electricity price and gas prices
reg_mat = fourier_mat(period = 252, cnt_terms = 1, time = np.array(seas_data['Time']) )

# Seasonal function for Electricity prices
# el_model = sm.RLM(endog  = np.array(seas_data['Elec']), exog = reg_mat, M= sm.robust.norms.HuberT())
el_model = sm.OLS(endog  = np.array(seas_data['Elec']), exog = reg_mat, missing='drop')
reg_el = el_model.fit()

# gas_model = sm.RLM(endog  = np.array(seas_data['Gas']), exog = reg_mat, M= sm.robust.norms.HuberT())
gas_model = sm.OLS(endog  = np.array(seas_data['Gas']), exog = reg_mat, missing='drop')
reg_gas = gas_model.fit()

# adding seasonal function to original data with all variables
data['Seas_el'] = seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = np.array(data['Time']) )

data['Seas_gas'] = seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = np.array(data['Time']) )


# fig = go.Figure()

# fig.add_trace(go.Scatter(
#     name="Price of Electricity",
#     x = data['Date'], y =  np.exp(data['Seas_el']),
#     line = dict(color='green')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= np.exp(data['Seas_gas']),
#     line = dict(color='red')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= np.exp(data['Seas_el']-data['Seas_gas']),
#     line = dict(color='black')
# ))

# fig.show()


# el_fig = go.Figure(
#   data = go.Scatter(x = data['Date'], y = data['Seas_el'] )
# )

# el_fig.add_trace(go.Scatter(
#     name="Price of Ele",
#     x=data['Date'], y= np.array(data['Elec']),
#     line = dict(color='red')
# ))

# el_fig.show()


# gas_fig = go.Figure(
#   data = go.Scatter(x = data['Date'], y = data['Seas_gas'] )
# )

# gas_fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= np.array(data['Gas']),
#     line = dict(color='red')
# ))

# gas_fig.show()

# # graph of deseasonalised time series without linear trend

# fig = go.Figure()

# fig.add_trace(go.Scatter(
#     name="Price of Electricity",
#     x = data['Time'], y = data['Elec']/ data['Seas_el'],
#     line = dict(color='green')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Time'], y= data['Gas']/data['Seas_gas'],
#     line = dict(color='red')
# ))

# fig.show()




# # DESEASONALISED TIME SERIES FOR ELECTRICITY AND GAS PRICES
# data_gas = np.copy(np.array( data['Gas']/data['Seas_gas'] ))
# data_el = np.copy(np.array(data['Elec']/data['Seas_el']))


############################################################################################ #
#                                Filtering param of CIR process                              #
############################################################################################ #
# seas_data = data.loc[(data['Date'] < '2021-06-01') | (data['Date'] >= '2023-01-01')]

# def seas_MLE(x, elec_data, gas_data, time, reg_mat):
  
#   elec_data = elec_data - x - np.log(time)
#   gas_data = gas_data - x*np.log(time)

#   # el_model = sm.OLS(endog  = elec_data, exog = reg_mat, missing='drop')
#   el_model = sm.RLM(endog  = elec_data, exog = reg_mat, missing='drop',M= sm.robust.norms.HuberT())
#   reg_el = el_model.fit()

#   # gas_model = sm.OLS(endog  = gas_data, exog = reg_mat, missing='drop')
#   gas_model = sm.RLM(endog  = gas_data, exog = reg_mat, missing='drop',M= sm.robust.norms.HuberT())
#   reg_gas = gas_model.fit()


#   return sum(reg_el.resid**2) + sum( reg_gas.resid**2)

# # seasonal regressors, this will be comon for both electricity price and gas prices
# reg_mat = fourier_mat(period = 252, cnt_terms = 1, time = np.array(seas_data['Time']) )

# opt_seas = minimize(seas_MLE, x0 = 3 , args=(np.copy(np.array(seas_data['LG_elec'])),np.copy(np.array(seas_data['LG_gas'])), np.copy(np.array(seas_data['Time'])), reg_mat ))

# lt = opt_seas.x

# # Seasonal function for Electricity prices
# # el_model = sm.OLS(endog  = np.array(seas_data['LG_elec']) - lt*np.array(seas_data['Time']), exog = reg_mat, missing='drop')
# el_model = sm.RLM(endog  = np.array(seas_data['LG_elec']) - lt*np.array(seas_data['Time']), exog = reg_mat, missing='drop',M= sm.robust.norms.HuberT())
# reg_el = el_model.fit()

# #gas_model = sm.OLS(endog  = np.array(seas_data['LG_gas']) - lt*np.array(seas_data['Time']), exog = reg_mat, missing='drop')
# gas_model = sm.RLM(endog  = np.array(seas_data['LG_gas']) - lt*np.array(seas_data['Time']), exog = reg_mat, missing='drop',M= sm.robust.norms.HuberT())
# reg_gas = gas_model.fit()


# # adding seasonal function to original data with all variables
# data['Seas_el'] = np.exp(seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = np.array(data['Time']) ) + lt*np.array(data['Time']))

# data['Seas_gas'] = np.exp(seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = np.array(data['Time']) )  + lt*np.array(data['Time']))


# DESEASONALISED TIME SERIES FOR ELECTRICITY AND GAS PRICES
data_gas = np.copy(np.array( data['Gas']/data['Seas_gas'] ))
data_el = np.copy(np.array(data['Elec']/data['Seas_el']))


# el_fig = go.Figure(
#   data = go.Scatter(x = data['Date'], y = data['Seas_el'] )
# )

# el_fig.add_trace(go.Scatter(
#     name="Price of Ele",
#     x=data['Date'], y= np.array(data['Elec']),
#     line = dict(color='red')
# ))

# el_fig.show()


# gas_fig = go.Figure(
#   data = go.Scatter(x = data['Date'], y = data['Seas_gas'] )
# )

# gas_fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= np.array(data['Gas']),
#     line = dict(color='red')
# ))

# gas_fig.show()

# fig = go.Figure()

# fig.add_trace(go.Scatter(
#     name="Price of Electricity",
#     x = data['Date'], y = data_el,
#     line = dict(color='green')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y=data_gas,
#     line = dict(color='red')
# ))

# fig.show()



# fig = go.Figure()

# fig.add_trace(go.Scatter(
#     name="Price of Electricity",
#     x = data['Date'], y =  (data['Seas_el']),
#     line = dict(color='green')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= (data['Seas_gas']),
#     line = dict(color='red')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= (data['Seas_el']/data['Seas_gas']),
#     line = dict(color='black')
# ))

# fig.add_trace(go.Scatter(
#     name="Price of Gas",
#     x=data['Date'], y= data['Seas_gas']/data['Seas_el'],
#     line = dict(color='black')
# ))

# fig.show()


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
axs[0].plot(data["Date"], data["Elec"], label="Electricity price", 
           color="#00897D",linestyle='-',  linewidth=1)
axs[0].plot(data["Date"], data["Seas_el"], label="Seasonal Elec", 
            color="#FF6B6B", linestyle='--', linewidth=1)

axs[0].set_ylabel("Electricity [EUR/MWh]")
axs[0].legend(loc="upper left")

# Gas plot
axs[1].plot(data["Date"], data["Gas"], label="Gas price", 
            color="#00897D",linestyle='-',  linewidth=1)
axs[1].plot(data["Date"], data["Seas_gas"], label="Seasonal Gas", 
            color="#FF6B6B", linestyle='--', linewidth=1)

axs[1].set_ylabel("Gas [EUR/MWh]")
axs[1].legend(loc="upper left")

# Common X label
axs[1].set_xlabel("Date")
fig.autofmt_xdate()
plt.tight_layout()

# Save as figure
plt.savefig("GRAPHS/Seasonal_orig.png", dpi=300)

plt.show()



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

# Detrended Electricity price (Original price / Seasonal component)
ax.plot(data["Date"], data["Elec"] / data["Seas_el"], label="Deseasonalised Electricity",color="#00897D",linestyle='-',  linewidth=1)

# Detrended Gas price (Original price / Seasonal component)
ax.plot(data["Date"], data["Gas"] / data["Seas_gas"], label="Deseasonalised Gas",
        color="#FF6B6B", linestyle='--', linewidth=1)

ax.set_xlabel("Date")
ax.set_ylabel("Detrended Price [EUR/MWh]")
ax.legend(loc="upper left")

fig.autofmt_xdate()
plt.tight_layout()

# Save the plot
plt.savefig("GRAPHS/Deseasonalised_data.png", dpi=300)

plt.show()