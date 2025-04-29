import numpy as np
from scipy.stats import jarque_bera, norm, chi2, multivariate_normal,shapiro, anderson
import statsmodels.api as sm
import plotly.graph_objects as go
from scipy.optimize import minimize, NonlinearConstraint
from scipy import stats


# function to estimate parameters
def coupled_cir(pars, gas, el, dt, opt = None, pars2 = None, perc = None):

  cir_el = np.copy(el)

  cir_gas = np.copy(gas)

  # Mean-reversion of Jumps
  a1 = pars[0]

  a2 = pars[1]

  # Upper quantile
  q11 = pars[2]

  # Lower quantile
  q12 = 1 - pars[2]

  q21 = pars[3]

  q22 = 1 - pars[3]

  el_up = norm.ppf(q11)

  el_down = norm.ppf(q12)

  gas_up = norm.ppf(q21)

  gas_down = norm.ppf(q22)
  
  kappa_el = pars2[0]
  alpha_el = pars2[1]
  sigma_el = pars2[2]

  kappa_gas = pars2[3]
  alpha_gas = pars2[4]
  sigma_gas = pars2[5]
  
  # FILTERING OUT CIR PROCESS  
  # Initializing jump process and creating vector of jumps

  # residuals2 = residuals/np.std(residuals)

  cir_el = np.copy(el)
  cir_gas = np.copy(gas)

  residuals1 = np.zeros(len(cir_el)-1)
  residuals2 = np.zeros(len(cir_gas)-1)

  jump_el = np.zeros(len(cir_el))
  jump_gas = np.zeros(len(cir_gas))

  of_set = 0

  residuals1[0] =(cir_el[1] - cir_el[0] - kappa_el*(1 - alpha_el + alpha_el*cir_gas[0] - cir_el[0])*dt)/(sigma_el*np.sqrt(dt*cir_el[0]))

  residuals2[0] = (cir_gas[1] - cir_gas[0] - kappa_gas*(1-alpha_gas + alpha_gas*cir_el[0] - cir_gas[0])*dt)/(sigma_gas*np.sqrt(dt*cir_gas[0]))


  for i in range(0, cir_el.size - 2):
  
    if (residuals1[i] > el_up) | (residuals1[i] < el_down) | (residuals2[i] > gas_up) | (residuals2[i] < gas_down):
       of_set += 0
    else:
       of_set = 0
  
    if residuals1[i] > el_up:
      # print("JUMP UP EL")
      residuals1[i] = 0

      cir_el_new = cir_el[i] + kappa_el*(1 - alpha_el + alpha_el*cir_gas[i] - cir_el[i])*dt + sigma_el*np.sqrt(dt*np.maximum(cir_el[i],0))*residuals1[i]

      jump_el[i+1] = cir_el[i+1] - cir_el_new

      cir_el[i+1] = cir_el_new

      inds = np.arange(i + 2, cir_el.size) - i - 1

      cir_el[i+2:cir_el.size]  = cir_el[i+2:cir_el.size] - jump_el[i+1]*np.exp(-a1*inds*dt)


    elif residuals1[i] < el_down:
      # print("JUMP DOWN EL")
      residuals1[i] = 0

      cir_el_new = cir_el[i] + kappa_el*(1 - alpha_el + alpha_el*cir_gas[i] - cir_el[i])*dt + sigma_el*np.sqrt(dt*np.maximum(cir_el[i],0))*residuals1[i]

      jump_el[i+1] = cir_el[i+1] - cir_el_new

      cir_el[i+1] = cir_el_new

      inds = np.arange(i + 2, cir_el.size) - i - 1

      cir_el[i+2:cir_el.size]  = cir_el[i+2:cir_el.size] - jump_el[i+1]*np.exp(-a1*inds*dt)


    # gas updating

    if residuals2[i] > gas_up:
      # print("JUMP UP GAS")
      residuals2[i] = 0

      cir_gas_new = cir_gas[i] + kappa_gas*(1-alpha_gas + alpha_gas*cir_el[i] - cir_gas[i])*dt + sigma_gas*np.sqrt(dt*np.maximum(cir_gas[i],0))*residuals2[i]

      jump_gas[i+1] = cir_gas[i+1] - cir_gas_new

      cir_gas[i+1] = cir_gas_new

      inds = np.arange(i + 2, cir_el.size) - i - 1

      cir_gas[i+2:cir_gas.size]  = cir_gas[i+2:cir_gas.size] - jump_gas[i+1]*np.exp(-a2*inds*dt)

    elif residuals2[i] < gas_down:
      # print("JUMP DOWN GAS")
      residuals2[i] = 0

      cir_gas_new = cir_gas[i] + kappa_gas*(1-alpha_gas + alpha_gas*cir_el[i] - cir_gas[i])*dt + sigma_gas*np.sqrt(dt*np.maximum(cir_gas[i],0))*residuals2[i]

      jump_gas[i+1] = cir_gas[i+1] - cir_gas_new

      cir_gas[i+1] = cir_gas_new

      inds = np.arange(i + 2, cir_el.size) - i - 1

      cir_gas[i+2:cir_el.size]  = cir_gas[i+2:cir_el.size] - jump_gas[i+1]*np.exp(-a2*inds*dt)

    residuals1[i+1] = (cir_el[i+2] - cir_el[i+1-of_set] - kappa_el*(1 - alpha_el + alpha_el*cir_gas[i+1-of_set] - cir_el[i+1-of_set])*dt*(1 + of_set))/(sigma_el*np.sqrt(dt*(1 + of_set)*np.maximum(cir_el[i+1-of_set],0.0000000000000001)))

    residuals2[i+1] = (cir_gas[i+2] - cir_gas[i+1] - kappa_gas*(1-alpha_gas + alpha_gas*cir_el[i+1-of_set] - cir_gas[i+1-of_set])*dt*(1 + of_set))/(sigma_gas*np.sqrt(dt*(1 + of_set)*np.maximum(cir_gas[i+1-of_set],0.0000000000000001)))

  Nsteps = len(cir_el)

  x1 = residuals1[ np.where( jump_el[1:Nsteps] == 0 )[0] ]
  x2 = residuals2[ np.where( jump_gas[1:Nsteps] == 0 )[0] ]
  
  x1 = (x1 - np.mean(x1))/np.std(x1)
  x2 = (x2 - np.mean(x2))/np.std(x2)

  p_val1 = jarque_bera(x1)
  p_val2 = jarque_bera(x2)

  # p_val1 = stats.kstest(x1,"norm")
  # p_val2 = stats.kstest(x1,"norm")

  test_stat = -2*( np.log( np.maximum(p_val1[1],0.000000001)) +  np.log( np.maximum(p_val2[1],0.000000001)) )
  p_val = 1 - chi2.cdf(test_stat, 4)

  if (opt == True):
    # | (p_val < perc)
    if (p_val1[1] < perc) | (p_val2[1] < perc) :
      barrier = 100000*(p_val1[0] + p_val2[0])

    else:
      barrier = 0

    return barrier + sum( abs(cir_el - el)**2) + sum( abs(cir_gas - gas)**2)


  fig_out = go.Figure(
      data = go.Scatter(y =el, name="Orig El",)
  )

  fig_out.add_trace(go.Scatter(
      name="New El",
      y= cir_el
  ))

  fig_out.add_trace(go.Scatter(
      name="Orig Gas",
      y= gas
  ))

  fig_out.add_trace(go.Scatter(
      name="New Gas",
      y= cir_gas
  ))
  fig_out.add_trace(go.Bar(
      name="Jump El",
      y= jump_el,
      marker_color='black'
  ))

  fig_out.add_trace(go.Bar(
      name="Jump Gas",
      y= jump_gas,
      marker_color='red'
  ))

  fig_out2 = go.Figure(
      data = go.Scatter(y =el, name="Orig El",)
  )

  fig_out2 = go.Figure( go.Histogram( x = residuals1[ np.where( jump_el[1:Nsteps] == 0 )[0]]))
  fig_out2.add_trace( go.Histogram( x = residuals2[ np.where( jump_gas[1:Nsteps] == 0 )[0]]))
  fig_out2.update_layout(barmode='overlay')
  fig_out2.update_traces(opacity=0.75)

  print([p_val1,p_val2])
  return fig_out, cir_el , cir_gas, jump_el, jump_gas, kappa_el, alpha_el, sigma_el, kappa_gas, alpha_gas, sigma_gas, fig_out2



#######################################################################################################################
# OPTIMALIZACIA PARAMETROV, STACI AJ OLS
#######################################################################################################################

# function to estimate parameters
def uni_cir(pars, ts, dt, opt = None, weight = None):

  cir_ts = np.copy(ts)

  negat_ind1 = np.where( cir_ts <= 0 )

  cir_ts[negat_ind1] = np.nan
  # define variables
  Nsteps = len(cir_ts)
  
  # Mean-reversion of Jumps
  a1 = pars[0]

  # Upper quantile
  q11 = pars[1]

  # Lower quantile
  q12 = 1 - pars[1]

  up = norm.ppf(q11)

  down = norm.ppf(q12)

  ###### FITTING MODEL
  
  # print(jump_cnt)
  rs = cir_ts[:Nsteps - 1]  
  rt = cir_ts[1:Nsteps]

  # feature engineering to fit the theoretical model for electricity
  y_ts = (rt - rs) / np.sqrt(rs)

  x_ts = (1-rs)*dt/np.sqrt(rs)
  x_ts = x_ts.reshape(-1,1)

  # fitting model by OLS
  model_ts = sm.OLS(y_ts, x_ts, missing = 'drop')

  fit_mod_ts = model_ts.fit()

  # Parameters estimate of CIR process

  kappa_ts = fit_mod_ts.params[0]
  alpha_ts = 1
  sigma_ts = np.std(fit_mod_ts.resid)/np.sqrt(dt)
  
  
  # FILTERING OUT CIR PROCESS  
  # Initializing jump process and creating vector of jumps

  # residuals2 = residuals/np.std(residuals)

  cir_ts = np.copy(ts)

  residuals = np.zeros(len(cir_ts)-1)

  jump_ts = np.zeros(len(cir_ts))

  of_set = 0

  for i in range(0, cir_ts.size - 2):

    if residuals[i] > up:

      residuals[i] = 0

      cir_ts_new = cir_ts[i] + kappa_ts*(alpha_ts  - cir_ts[i])*dt + sigma_ts*np.sqrt(dt*np.maximum(cir_ts[i],0))*residuals[i]

      jump_ts[i+1] = cir_ts[i+1] - cir_ts_new

      cir_ts[i+1] = cir_ts_new

      inds = np.arange(i + 2, cir_ts.size) - i - 1

      cir_ts[i+2:cir_ts.size]  = cir_ts[i+2:cir_ts.size] - jump_ts[i+1]*np.exp(-a1*inds*dt)

      of_set += 0


    elif residuals[i] < down:

      residuals[i] = 0

      cir_ts_new = cir_ts[i] + kappa_ts*(alpha_ts  - cir_ts[i])*dt + sigma_ts*np.sqrt(dt*np.maximum(cir_ts[i],0))*residuals[i]

      jump_ts[i+1] = cir_ts[i+1] - cir_ts_new

      cir_ts[i+1] = cir_ts_new

      inds = np.arange(i + 2, cir_ts.size) - i - 1

      cir_ts[i+2:cir_ts.size]  = cir_ts[i+2:cir_ts.size] - jump_ts[i+1]*np.exp(-a1*inds*dt)

      of_set += 0

    else:
       of_set = 0

    residuals[i+1] = (cir_ts[i+2] - cir_ts[i+1 - of_set] - kappa_ts*( alpha_ts  - cir_ts[i+1- of_set])*dt*(1 + of_set))/(sigma_ts*np.sqrt(dt*(1 + of_set)*np.maximum(cir_ts[i+1- of_set],0.0000000000000001)))

  Nsteps = len(cir_ts)

  x1 = residuals[ np.where( jump_ts[1:Nsteps] == 0 )[0]]

  x1 = (x1 - np.mean(x1))/np.std(x1)

  # p_val1 = stats.kstest(x1,"norm")
  
  p_val = jarque_bera(x1)

  if (opt == True):
    
    if (p_val[1] < 0.05)  :
      barrier = 100000*(p_val[0] )
    else:
      barrier = p_val[0] 

    return barrier + sum( ( ts- cir_ts)**2 ) 
    # + sum(abs(el_jchanges)**2) + sum(abs(gas_jchanges)**2)
    #+ sum(abs(jump_el)**2) + sum(abs(jump_gas)**2) + sum( abs(cir_el[jump_el == 0] - el[jump_el == 0])**2)+ sum( abs(cir_gas[jump_gas == 0] - gas[jump_gas == 0])**2)
    # + sum(abs(el_jchanges)) + sum(abs(gas_jchanges))
    # + sum( el_changes**2) + sum( gas_changes**2 ) + sum(np.diff(cir_el)**2) + sum(np.diff(cir_gas)**2)
  

  fig_out = go.Figure(
      data = go.Scatter(y =ts, name="Orig El",)
  )

  fig_out.add_trace(go.Scatter(
      name="New El",
      y= cir_ts
  ))


  fig_out.add_trace(go.Bar(
      name="Jump El",
      y= jump_ts,
      marker_color='black'
  ))


  print([p_val])
  return fig_out, cir_ts, jump_ts

