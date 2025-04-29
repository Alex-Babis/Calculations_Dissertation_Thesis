####################################################################################################
# Function that will evaluate forward prices based on the values of 
# parameters from bivariate electricity-gas process
####################################################################################################
import numpy as np 
from CODES.Trunc_norm_dist import  char_trunc_norm, mean_trunc_norm
from scipy.optimize import root
from scipy.stats import norm

import warnings
# Suppress the FutureWarning for Series.__getitem__
warnings.simplefilter(action='ignore', category=FutureWarning)

####################################################################################################
# Expected values of Jump components - Mean-reverting driven by Hawkess processes
####################################################################################################
def fwd_jump(tau, pars_e, pars_g, init_value):

  # parameters of jump processes with mean value of absolute jumps
  ae, kappa_le, alpha_le, delta_ee, delta_eg, mu1, mu1_abs = pars_e

  ag, kappa_lg, alpha_lg, delta_gg, delta_ge, mu2, mu2_abs = pars_g

  # 
  init_value_l = init_value[2:]
  init_value_jump = init_value[0:2]

  A = np.array([ [delta_ee*mu1_abs - kappa_le, delta_eg*mu2_abs] , [delta_ge*mu1_abs, delta_gg*mu2_abs - kappa_lg ]])

  chi1 = (A[0,0] + A[1,1] + np.sqrt((A[0,0] - A[1,1])**2 + 4*A[0,1]*A[1,0]))/2
  chi2 = (A[0,0] + A[1,1] - np.sqrt((A[0,0] - A[1,1])**2 + 4*A[0,1]*A[1,0]))/2

  V = np.array([ [-delta_eg*mu2_abs, -delta_eg*mu2_abs] , [delta_ee*mu1_abs - kappa_le - chi1, delta_ee*mu1_abs - kappa_le - chi2]])

  # calculation of constant coeficient of expected values of intensities
  ce_0, cg_0 = -V @ np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_le*alpha_le, kappa_lg*alpha_lg])

  ce , cg  = V @ np.diag(np.linalg.inv(V) @ init_value_l + np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_le*alpha_le, kappa_lg*alpha_lg])) 

  fwd_e = np.exp(-ae*tau)*init_value_jump[0] + mu1*( ce_0/ae *(1 - np.exp(-ae*tau)) + ce[0]/(ae + chi1)*(np.exp(chi1*tau) - np.exp(-ae*tau)) + ce[1]/(ae + chi2)*(np.exp(chi2*tau) - np.exp(-ae*tau)))
  
  fwd_g = np.exp(-ag*tau)*init_value_jump[1] + mu2*( cg_0/ag *(1 - np.exp(-ag*tau)) + cg[0]/(ag + chi1)*(np.exp(chi1*tau) - np.exp(-ag*tau)) + cg[1]/(ag + chi2)*(np.exp(chi2*tau) - np.exp(-ag*tau)))

  return np.array([fwd_e, fwd_g])


####################################################################################################
# Expected values of Coupler CIR components
####################################################################################################
def fwd_cir(tau, pars_e, pars_g, init_value):

  kappa_el, beta_el, alpha_el = pars_e
  kappa_gas, beta_gas, alpha_gas = pars_g

  chi1 = (-(kappa_el + kappa_gas) + np.sqrt( (kappa_el + kappa_gas)**2 + 4*kappa_el*kappa_gas*(alpha_el*alpha_gas - 1) ))/2
  chi2 = (-(kappa_el + kappa_gas) - np.sqrt( (kappa_el + kappa_gas)**2 + 4*kappa_el*kappa_gas*(alpha_el*alpha_gas - 1) ))/2

  V = np.array([ [kappa_el*alpha_el, kappa_el*alpha_el] , [kappa_el + chi1, kappa_el + chi2]])

  fwd = V @ np.diag([( np.exp(chi1*tau) - 1)/chi1 , (np.exp(chi2*tau) - 1)/chi2 ]) @ np.linalg.inv(V) @ np.array([kappa_el*beta_el, kappa_gas*beta_gas]) + V @  np.diag([ np.exp(chi1*tau) , np.exp(chi2*tau) ]) @ np.linalg.inv(V) @ init_value

  return fwd


####################################################################################################
# prenasobit sezonnou castou a nascitat to cez vsetky dni v danom forwarde
# najst potom parametre pre thety aby to co najviac sedelo s pozorovanymi hodnotami
####################################################################################################
def jump_martingale(pars, fixed_pars,pars_jump_el, pars_jump_gas):
  u1, u2 = pars
  theta1, theta2, kappa_le, delta_ee, delta_eg, kappa_lg, delta_ge, delta_gg = fixed_pars
  eq1 = u1*kappa_le - char_trunc_norm(a = 0, b = u1*delta_ee + u2*delta_ge + theta1, pars = pars_jump_el) + 1
  eq2 = u2*kappa_lg - char_trunc_norm(a = 0, b = u1*delta_eg + u2*delta_gg + theta2, pars = pars_jump_gas) + 1

  return np.array([eq1,eq2])


def risk_neutral_fwd_price(pars, cir_pars_e, cir_pars_g, me_pars_e, me_pars_g, jump_pars_e, jump_pars_g, seasonal_e, seasonal_g, init_values, time, fwd_e, fwd_g, start_end, opt = None):

  # initial value for cir process 
  init_val_cir = init_values[0:2]
  init_value_me = init_values[2:4]
  init_value_lambda = init_values[4:]

  ####################################################################################################
  # Part of forward price related to COUPLED CIR PROCESS
  ####################################################################################################

  # price of risk
  theta_e_cir, theta_g_cir, theta_e_jump, theta_g_jump = pars 

  # parameters of CIR under historical probability
  kappa_e, alpha_e, sigma_e = cir_pars_e 
  beta_e = 1 - alpha_e

  kappa_g, alpha_g, sigma_g = cir_pars_g
  beta_g = 1 - alpha_g

  # risk-neutral parameters of CIR
  kappa_e_Q = kappa_e + theta_e_cir
  beta_e_Q = (1 - alpha_e)*kappa_e/(kappa_e + theta_e_cir)
  alpha_e_Q = alpha_e*kappa_e/(kappa_e + theta_e_cir)

  cir_pars_e_Q = [kappa_e_Q, beta_e_Q, alpha_e_Q, sigma_e]

  kappa_g_Q = kappa_g + theta_g_cir
  beta_g_Q = (1 - alpha_g)*kappa_g/(kappa_g + theta_g_cir)
  alpha_g_Q = alpha_g*kappa_g/(kappa_g + theta_g_cir)

  cir_pars_g_Q = [kappa_g_Q, beta_g_Q, alpha_g_Q, sigma_g]

  fwd_cir_prices = np.zeros(( len(time),2))

  for i in range(0,len(time)):
    fwd_cir_prices[i,:] = fwd_cir(tau = time[i], pars_e = [kappa_e_Q, beta_e_Q, alpha_e_Q], pars_g = [kappa_g_Q, beta_g_Q, alpha_g_Q], init_value = init_val_cir)


  ####################################################################################################
  # Part of forward price related to MUTUALLY EXCITING JUMP PROCESS
  ####################################################################################################

  # jump parameters under historical probability
  ae, kappa_le, alpha_le, delta_ee, delta_eg = me_pars_e

  ag, kappa_lg, alpha_lg, delta_gg, delta_ge = me_pars_g

  # risk-neutral parameters of jumps u1, u2, a u3 
  u1, u2 = root(fun = jump_martingale, x0 = [0,0], args = ([theta_e_jump, theta_g_jump,kappa_le, delta_ee, delta_eg, kappa_lg, delta_ge, delta_gg], jump_pars_e, jump_pars_g), method='hybr').x

  u3 = kappa_le*alpha_le*u1 + kappa_lg*alpha_lg*u2

  g1 = char_trunc_norm(a = 0, b = u1*delta_ee + u2*delta_ge + theta_e_jump, pars = jump_pars_e)
  g2 = char_trunc_norm(a = 0, b = u1*delta_eg + u2*delta_gg + theta_g_jump, pars = jump_pars_g)

  alpha_le_Q = alpha_le*g1
  delta_ee_Q = delta_ee*g1
  delta_eg_Q = delta_eg*g1

  alpha_lg_Q = alpha_lg*g2
  delta_gg_Q = delta_gg*g2
  delta_ge_Q = delta_ge*g2

  # also the initial value of lambda will change....
  init_value_lambda = init_value_lambda * np.array([g1, g2])

  # distribution of jumps under Q, where only probs of positive and negative jumps are altered?
  jump_pars_e_Q = jump_pars_e.copy()
  jump_pars_g_Q = jump_pars_g.copy()

  #
  be = (u1*delta_ee + u2*delta_ge + theta_e_jump)
  bg = (u1*delta_eg + u2*delta_gg + theta_g_jump)

  e_pp = np.exp( norm.logcdf((jump_pars_e[0] + jump_pars_e[2]**2 * be/2)/jump_pars_e[2]) + norm.logcdf(jump_pars_e[0]/jump_pars_e[2]) + np.log(jump_pars_e[4]) + be*jump_pars_e[0] + be**2 * jump_pars_e[2]**2/2)

  e_pn =  np.exp( norm.logcdf((-jump_pars_e[1] + jump_pars_e[3]**2 * be/2)/jump_pars_e[3]) + norm.logcdf(-jump_pars_e[1]/jump_pars_e[3]) + np.log(jump_pars_e[4]) - be*jump_pars_e[1] + be**2 * jump_pars_e[3]**2/2)

  g_pp = np.exp( norm.logcdf((jump_pars_g[0] + jump_pars_g[2]**2 * bg/2)/jump_pars_g[2]) + norm.logcdf(jump_pars_g[0]/jump_pars_g[2]) + np.log(jump_pars_g[4]) + bg*jump_pars_g[0] + bg**2 * jump_pars_g[2]**2/2)

  g_pn =  np.exp( norm.logcdf((-jump_pars_g[1] + jump_pars_g[3]**2 * bg/2)/jump_pars_g[3]) + norm.logcdf(-jump_pars_g[1]/jump_pars_g[3]) + np.log(jump_pars_g[4]) - bg*jump_pars_g[1] + bg**2 * jump_pars_g[3]**2/2)

  # mean value of positive jumps - electricity
  jump_pars_e_Q[0] = jump_pars_e[0] + be*jump_pars_e[1]

  # mean value of negative jumps - electricity
  jump_pars_e_Q[2] = jump_pars_e[2] - be*jump_pars_e[3]

  # probability of positive jump 
  jump_pars_e_Q[4] = e_pp/(e_pp + e_pn)

  # mean value of positive jumps - electricity
  jump_pars_g_Q[0] = jump_pars_g[0] + bg*jump_pars_g[1]

  # mean value of negative jumps - electricity
  jump_pars_g_Q[2] = jump_pars_g[2] - bg*jump_pars_g[3]

  # probability of positive jump 
  jump_pars_g_Q[4] = g_pp/(g_pp + g_pn)

  mu1_Q = mean_trunc_norm(jump_pars_e_Q, abs_val = False)
  mu1_abs_Q = mean_trunc_norm(jump_pars_e_Q, abs_val = True)

  mu2_Q = mean_trunc_norm(jump_pars_g_Q, abs_val = False)
  mu2_abs_Q = mean_trunc_norm(jump_pars_g_Q, abs_val = True)

  # mu1 and mu1_abs will be recalculated
  pars_e_Q = [ae, kappa_le, alpha_le_Q, delta_ee_Q, delta_eg_Q, mu1_Q, mu1_abs_Q ]
  pars_g_Q = [ag, kappa_lg, alpha_lg_Q, delta_gg_Q, delta_ge_Q, mu2_Q, mu2_abs_Q ]

  fwd_me_prices = np.zeros((len(time),2))
  for i in range(0,len(time)):
    fwd_me_prices[i,:] = fwd_jump(tau = time[i], pars_e = pars_e_Q, pars_g = pars_g_Q, init_value = np.concatenate((init_value_me,init_value_lambda)))

  fwd_price = fwd_cir_prices + fwd_me_prices

  fwd_price[:,0] = fwd_price[:,0]*seasonal_e
  fwd_price[:,1] = fwd_price[:,1]*seasonal_g

  swap_el = np.zeros(start_end.shape[0])
  swap_gas = np.zeros(start_end.shape[0])

  for i in range(0,len(swap_el)):
      swap_el[i] = fwd_price[np.arange(start_end[i,0],start_end[i,1]+1),0].mean()
      swap_gas[i]= fwd_price[np.arange(start_end[i,0],start_end[i,1]+1),1].mean()

  e_dist = (fwd_e - swap_el)**2
  g_dist = (fwd_g - swap_gas)**2

  if opt == True:
    if np.isnan(sum(e_dist) + sum(g_dist)):
          return np.inf 
    return sum(e_dist) + sum(g_dist)
  
  pars_e_Q = [ae, kappa_le, alpha_le_Q, delta_ee_Q, delta_eg_Q ]
  pars_g_Q = [ag, kappa_lg, alpha_lg_Q, delta_gg_Q, delta_ge_Q ]

  initial_val_Q = np.concatenate((init_val_cir,np.concatenate((init_value_me,init_value_lambda))))
  
  return fwd_price, cir_pars_e_Q, cir_pars_g_Q, pars_e_Q, pars_g_Q, jump_pars_e_Q, jump_pars_g_Q, initial_val_Q
