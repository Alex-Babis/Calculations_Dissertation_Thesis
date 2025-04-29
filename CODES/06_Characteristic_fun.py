####################################################################################################
# SOLVING THE SYSTEM OF ODES FOR CHARACTERISTIC FUNCTION OF COUPLED CIR PROCESSES
####################################################################################################

####################################################################################################
# prerobit na tvar pre beta a teda kedy tam nebude hodnota rovna 1
#################################################################################################### 
def cir_ode(t, y, pars_e, pars_g):

  kappa_e, beta_e, alpha_e, sigma_e = pars_e
  kappa_g, beta_g, alpha_g, sigma_g = pars_g

  A = np.array([ [0, kappa_e*beta_e, kappa_g*beta_g ], [0, -kappa_e, kappa_g*alpha_g], [0, kappa_e*alpha_e, -kappa_g]  ])

  B = np.array([[0,0,0], [0, sigma_e**2,0], [0,0, sigma_g**2]])

  rhs = A @ y + 1/2 * B @ y**2

  return rhs

def cir_char(u, v, t, T, pars_e, pars_g):

  sol = solve_ivp(cir_ode, [t, T], [0*1j,u*1j,v*1j], method='RK45', args =(pars_e, pars_g))

  beta0, beta1, beta2 = sol.y[:,-1]

  cir_pars = np.array([beta0, beta1, beta2])

  # cir_char_val =  beta0 + cir_pars @ initial_val

  return cir_pars



####################################################################################################
# SOLVING SYSTEM OF ODES FOR CHARACTERISTIC FUNCTION OF MUTUALLY EXCITED HAWKESS PROCESS
####################################################################################################

def meh_odes(t, y, pars_le, pars_lg,  pars_jump_el, pars_jump_gas,w1,w2):

  a1, kappa_le, alpha_le, delta_ee, delta_eg= pars_le

  a2, kappa_lg, alpha_lg, delta_gg, delta_ge = pars_lg

  beta0 = y[0]
  beta1 = np.exp(-a1*t)*w1
  beta2 = np.exp(-a2*t)*w2
  beta3 = y[1]
  beta4 = y[2]
  

  rhs0 = + beta3*kappa_le*alpha_le + beta4*kappa_lg*alpha_lg
  rhs3 = -beta3*kappa_le + char_trunc_norm(beta1, (beta3*delta_ee + beta4*delta_ge), pars = pars_jump_el) - 1
  rhs4 = -beta4*kappa_lg + char_trunc_norm(beta2, (beta3*delta_eg + beta4*delta_gg), pars = pars_jump_gas) - 1

  rhs = np.array([rhs0, rhs3, rhs4])

  return rhs

def meh_char(u, v, t,  T,  pars_le, pars_lg,  pars_jump_el, pars_jump_gas):


  sol = solve_ivp(meh_odes, [t, T], [0j, 0j, 0j],method='RK45', args =( pars_le, pars_lg,  pars_jump_el, pars_jump_gas, u*1j, v*1j))

  # values at the last value of t (beta0, beta3, beta4)
  beta0, beta3, beta4 = sol.y[:,-1]

  # # Beta1 and beta2 ahve analytic forms
  beta1 = np.exp(pars_le[0]*(t - T))*u*1j

  beta2 = np.exp(pars_lg[0]*(t - T))*v*1j
  meh_pars = np.array([beta0, beta1, beta2, beta3, beta4])

  # meh_char_val =  beta0 + meh_pars @ initial_val
  return meh_pars


####################################################################################################
# Expected value and Variance of Stochastic processes 
####################################################################################################
def var_meh_ode(t, y, pars_le, pars_lg,  means, means_squared, mean_lambda_pars):
  
  ae, kappa_le, alpha_le, delta_ee, delta_eg = pars_le
  ag, kappa_lg, alpha_lg, delta_gg, delta_ge = pars_lg

  z_mean, y_mean, z_abs_mean, y_abs_mean = means
  z2, y2, z_abs2, y_abs2 = means_squared

  chi1, chi2, ce_0, cg_0, ce, cg = mean_lambda_pars

  mean_lambda_e = ce_0 + ce[0]*np.exp(chi1 * t) + ce[1]*np.exp(chi2 * t)
  mean_lambda_g = cg_0 + cg[0]*np.exp(chi1 * t) + cg[1]*np.exp(chi2 * t)

  mean_lambda = np.array([ mean_lambda_e, mean_lambda_g])

  B = np.array([ [z2, 0, y_mean, delta_ee*z_abs2, delta_ge * z_abs2, 0, 0,delta_ee**2 * z2, delta_ge**2 * z2 , delta_ee * delta_ge * z2],
                 [0, y2, z_mean, 0, 0, delta_eg*y_abs2, delta_gg * y_abs2, delta_eg**2 * y2, delta_gg**2 * y2, delta_eg * delta_gg * y2]])
  
  A = np.array([ [-2*ae, 0, 0, 2*z_mean, 0, 0, 0, 0, 0, 0],
                 [0, -2*ag, 0, 0, 0, 0, 2*y_mean, 0, 0, 0],
                 [0, 0, -(ae + ag), 0, y_mean, z_mean, 0, 0, 0, 0],
                 [0, 0, 0, -(ae + kappa_le - delta_ee * z_abs_mean), delta_eg * y_abs_mean, 0, 0, z_mean, 0, 0],
                 [0, 0, 0, delta_ge * z_abs_mean, -(ae + kappa_lg - delta_gg*y_abs_mean), 0, 0, 0, 0, z_mean],
                 [0, 0, 0, 0, 0, -(ag + kappa_le - delta_ee * z_abs_mean), delta_eg *y_abs_mean, 0, 0, y_mean],
                 [0, 0, 0, 0, 0, delta_ge * z_abs_mean, -(ag + kappa_lg - delta_gg * y_abs_mean), 0 ,y_mean, 0],
                 [0, 0, 0, 0, 0, 0, 0, -2*(kappa_le - delta_ee * z_abs_mean), 0, 2*delta_eg * y_abs_mean],
                 [0, 0, 0, 0, 0, 0, 0, 0, -2*(kappa_lg - delta_gg * y_abs_mean), 2*delta_ge * z_abs_mean],
                 [0, 0, 0, 0, 0, 0, 0, delta_ge *z_abs_mean, delta_eg * y_abs_mean, -(kappa_le - delta_ee * z_abs_mean + kappa_lg - delta_gg * y_abs_mean)]])

  rhs = B.T @ mean_lambda + A @ y

  return rhs


def moments_meh( time,  pars_le, pars_lg,  pars_jump_el, pars_jump_gas,initial_val):

  ae, kappa_le, alpha_le, delta_ee, delta_eg= pars_le
  ag, kappa_lg, alpha_lg, delta_gg, delta_ge = pars_lg

  # exected value of X^2
  z2 = var_trunc_norm(pars_jump_el, abs_val = False)
  y2 = var_trunc_norm(pars_jump_gas, abs_val = False)

  # expected values of X|X|
  z_abs2 = var_trunc_norm(pars_jump_el, abs_val = True)
  y_abs2 = var_trunc_norm(pars_jump_gas, abs_val = True)

  # expected value of X
  z_mean = mean_trunc_norm(pars_jump_el, abs_val = False)
  y_mean = mean_trunc_norm(pars_jump_gas, abs_val = False)

  # expected values of |X|
  z_abs_mean = mean_trunc_norm(pars_jump_el, abs_val = True)
  y_abs_mean = mean_trunc_norm(pars_jump_gas, abs_val = True)

  means = [z_mean, y_mean, z_abs_mean, y_abs_mean]
  means_squared = [z2, y2, z_abs2, y_abs2]

  # mean value of lambda 
  A = np.array([ [delta_ee*z_abs_mean - kappa_le, delta_eg*y_abs_mean] , [delta_ge*z_abs_mean, delta_gg*y_abs_mean - kappa_lg ]])

  chi1 = (A[0,0] + A[1,1] + np.sqrt((A[0,0] - A[1,1])**2 + 4*A[0,1]*A[1,0]))/2
  chi2 = (A[0,0] + A[1,1] - np.sqrt((A[0,0] - A[1,1])**2 + 4*A[0,1]*A[1,0]))/2

  V = np.array([ [-delta_eg*y_abs_mean, -delta_eg*y_abs_mean] , [delta_ee*z_abs_mean - kappa_le - chi1, delta_ee*z_abs_mean - kappa_le - chi2]])

  # calculation of constant coeficient of expected values of intensities
  ce_0, cg_0 = -V @ np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_le*alpha_le, kappa_lg*alpha_lg])

  ce , cg  = V @ np.diag(np.linalg.inv(V) @ initial_val[2:] + np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_le*alpha_le, kappa_lg*alpha_lg])) 

  # JUMP PART OF FORWARDS
  h0 = z_mean * (  -(kappa_le * alpha_le * (delta_ee * z_abs_mean - kappa_le - chi2) + kappa_lg * alpha_lg *delta_eg * y_abs_mean)/(chi1*(chi2 - chi1)) * ( np.exp(chi1 * time)/( ae + chi1) - 1/ae - np.exp(-ae*time)/(ae + chi1) + np.exp(-ae * time)/ae ) + (kappa_le * alpha_le * (delta_ee * z_abs_mean - kappa_le - chi1) + kappa_lg * alpha_lg *delta_eg * y_abs_mean)/(chi2*(chi2 - chi1)) * ( np.exp(chi2 *time)/(ae + chi2) - 1/ae - np.exp(-ae*time)/(ae + chi2 ) + np.exp(-ae*time)/ae ) )

  h3 = np.exp(-ae *time)

  h4 = np.zeros(len(time))

  h5 = z_mean * ( (delta_ee * z_abs_mean - kappa_le - chi1)/(ae + chi2) *( np.exp(chi2 *time) - np.exp(-ae *time)) - (delta_ee * z_abs_mean - kappa_le - chi2)/(ae + chi1) * (np.exp(chi1*time) - np.exp(-ae*time)) )/(chi2 - chi1)

  h6 = z_mean * delta_eg*y_abs_mean * ( (np.exp(chi2 * time) - np.exp(-ae*time))/(ae + chi2) - (np.exp(chi1*time) - np.exp(-ae*time))/(ae + chi1) )/(chi2 - chi1)

  g0 = y_mean * (  (delta_ee * z_abs_mean - kappa_le - chi1)*(kappa_le * alpha_le * (delta_ee * z_abs_mean - kappa_le - chi2) + kappa_lg * alpha_lg *delta_eg * y_abs_mean)/(delta_eg * y_abs_mean *chi1*(chi2 - chi1)) * ( np.exp(chi1 * time)/( ag + chi1) - 1/ag - np.exp(-ag*time)/(ag + chi1) + np.exp(-ag * time)/ag ) - (delta_ee * z_abs_mean - kappa_le - chi2) * (kappa_le * alpha_le * (delta_ee * z_abs_mean - kappa_le - chi1) + kappa_lg * alpha_lg *delta_eg * y_abs_mean)/(delta_eg * y_abs_mean * chi2*(chi2 - chi1)) * ( np.exp(chi2 *time)/(ag + chi2) - 1/ag - np.exp(-ag*time)/(ag + chi2 ) + np.exp(-ag*time)/ag ) )

  g3 = np.zeros( len(time))

  g4 = np.exp(-ag*time)

  g5 = (y_mean*(delta_ee*z_abs_mean - kappa_le - chi1) * (delta_ee *z_abs_mean - kappa_le - chi2)/(delta_eg *y_abs_mean) *( (np.exp(chi1 *time) - np.exp(-ag*time))/(chi1 + ag) - (np.exp(chi2*time) - np.exp(-ag*time))/(ag + chi2)))/(chi2 - chi1) 

  g6 = (y_mean*( (delta_ee * z_abs_mean - kappa_le - chi1)*(np.exp(chi1 *time) - np.exp(-ag*time))/(chi1 + ag) - (delta_ee * z_abs_mean - kappa_le - chi2)*(np.exp(chi2*time) - np.exp(-ag*time))/(ag + chi2)))/(chi2 - chi1) 


  mean_lambda_pars = [ chi1, chi2, ce_0, cg_0, ce, cg]

  sol = solve_ivp(var_meh_ode, [0, time.max()], t_eval = time, y0= [0,0,0,0,0,0,0,0,0,0] ,method='RK45', args =( pars_le, pars_lg,  means, means_squared, mean_lambda_pars))

  # mean value of J_e and J_g
  J_e = np.exp(-ae*time)*initial_val[0] + z_mean*( ce_0/ae *(1 - np.exp(-ae*time)) + ce[0]/(ae + chi1)*(np.exp(chi1*time) - np.exp(-ae*time)) + ce[1]/(ae + chi2)*(np.exp(chi2*time) - np.exp(-ae*time)))
  
  J_g = np.exp(-ag*time)*initial_val[1] + y_mean*( cg_0/ag *(1 - np.exp(-ag*time)) + cg[0]/(ag + chi1)*(np.exp(chi1*time) - np.exp(-ag*time)) + cg[1]/(ag + chi2)*(np.exp(chi2*time) - np.exp(-ag*time)))

  h_func = np.array([h0, h3, h4, h5, h6])
  g_func = np.array([g0, g3, g4, g5, g6])

  return J_e, J_g, sol.y[0:3,:], h_func, g_func


def var_cir_ode(t, y, pars_e, pars_g, mean_cir_pars):

  kappa_el, beta_el, alpha_el, sigma_el = pars_e
  kappa_gas, beta_gas, alpha_gas, sigma_gas = pars_g

  # kappa_el,  alpha_el, sigma_el = pars_e
  # kappa_gas,  alpha_gas, sigma_gas = pars_g

  chi1, chi2, ce_0, cg_0, ce, cg = mean_cir_pars

  mean_cir_e = ce_0 + ce[0]*np.exp(chi1 * t) + ce[1]*np.exp(chi2 * t)
  mean_cir_g = cg_0 + cg[0]*np.exp(chi1 * t) + cg[1]*np.exp(chi2 * t)

  mean_cir = np.array([ mean_cir_e, mean_cir_g])

  B = np.array([[sigma_el**2, 0], [0, sigma_gas**2], [0,0]])

  A = np.array([[-2*kappa_el, 0, 2*kappa_el*alpha_el],
                [0, -2*kappa_gas, 2*kappa_gas*alpha_gas],
                [kappa_gas*alpha_gas, kappa_el*alpha_el, -(kappa_el + kappa_gas)]])

  rhs = B @ mean_cir + A @ y

  return rhs


# pozriet aj tie forwardy ci sa dobre pocitaju

def moments_cir(time, pars_e, pars_g, initial_val):

  kappa_el, beta_el, alpha_el, sigma_el = pars_e
  kappa_gas, beta_gas, alpha_gas, sigma_gas = pars_g

  chi1 = (-(kappa_el + kappa_gas) + np.sqrt( (kappa_el + kappa_gas)**2 + 4*kappa_el*kappa_gas*(alpha_el*alpha_gas - 1) ))/2
  chi2 = (-(kappa_el + kappa_gas) - np.sqrt( (kappa_el + kappa_gas)**2 + 4*kappa_el*kappa_gas*(alpha_el*alpha_gas - 1) ))/2

  V = np.array([ [kappa_el*alpha_el, kappa_el*alpha_el] , [kappa_el + chi1, kappa_el + chi2]])
  
  # calculation of constant coeficient of expected values of intensities
  ce_0, cg_0 = -V @ np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_el*beta_el, kappa_gas*beta_gas])

  ce , cg  = V @ np.diag(np.linalg.inv(V) @ initial_val + np.diag([1/chi1,1/chi2]) @ np.linalg.inv(V) @ np.array([kappa_el*beta_el, kappa_gas*beta_gas])) 

  # CIR PART OF FORWARD
  h0 = kappa_el * alpha_el *( np.exp(chi1*time) - 1) *( (kappa_el + chi2)*beta_el/alpha_el - kappa_gas*beta_gas)/(chi1 *( chi2 - chi1) ) + kappa_el * alpha_el *( np.exp(chi2*time) - 1) *( - (kappa_el + chi1)*beta_el/alpha_el + kappa_gas*beta_gas)/(chi2 *( chi2 - chi1) )

  h1 = ( (kappa_el + chi2 ) * np.exp(chi1 * time) - (kappa_el + chi1) * np.exp(chi2 *time) )/(chi2 - chi1)

  h2 = -kappa_el * alpha_el * ( np.exp(chi1 * time) - np.exp(chi2 * time) )/(chi2 - chi1)

  g0 = ( kappa_el + chi1) *( np.exp(chi1*time) - 1) *( (kappa_el + chi2)*beta_el/alpha_el - kappa_gas*beta_gas)/(chi1 *( chi2 - chi1) ) + (kappa_el + chi2) *( np.exp(chi2*time) - 1) *( - (kappa_el + chi1)*beta_el/alpha_el + kappa_gas*beta_gas)/(chi2 *( chi2 - chi1) )

  g1 = ( (kappa_el + chi1) * (kappa_el + chi2) * (np.exp(chi1 * time) - np.exp(chi2 * time)) /(kappa_el * alpha_el) )/(chi2 - chi1)

  g2 = ((kappa_el + chi2) * np.exp(chi2 *time) - (kappa_el + chi1) * np.exp(chi1 *time))/(chi2 - chi1)

  mean_cir_pars = [ chi1, chi2, ce_0, cg_0, ce, cg]

  sol = solve_ivp(var_cir_ode, [0, time.max()], t_eval = time, y0= [0,0,0] ,method='RK45', args =( pars_e, pars_g, mean_cir_pars))

  mean_e = ce_0 + ce[0]*np.exp(chi1 * time) + ce[1]*np.exp(chi2 * time)
  mean_g = cg_0 + cg[0]*np.exp(chi1 * time) + cg[1]*np.exp(chi2 * time)

  h_func = np.array([h0, h1, h2])
  g_func = np.array([g0, g1, g2])
  h_func.shape
  return mean_e, mean_g, sol.y, h_func, g_func



####################################################################################################
# Pricing of spread options - electricity vs gas price
####################################################################################################

# hr = 2.8

# N1, N2 = [100,100]

# ####################################################################################################
# # Discrete cosine transformation 2D - same as apply it in 1D along axis 0 and then apply for result
# ####################################################################################################
# Q = 2000

# L = 10

# time = np.arange(0,1, 1/252)

# cir_pars_e = cir_pars_e_Q.copy()
# cir_pars_g = cir_pars_g_Q.copy()
# me_pars_e = pars_e_Q.copy()
# me_pars_g = pars_g_Q.copy()
# jump_pars_e = jump_pars_e_Q.copy()
# jump_pars_g = jump_pars_g_Q.copy()
# init_value = initial_val_Q.copy()
# seasonal_e = seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = time.copy() ) 
# seasonal_g = seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = time.copy() )  
####################################################################################################
# Option price on Spark-spread 
####################################################################################################

def ss_option(hr, Q, N1, N2, L, time, cir_pars_e, cir_pars_g, me_pars_e, me_pars_g, jump_pars_e, jump_pars_g, seasonal_e, seasonal_g, init_value):

  kappa_el, beta_el, alpha_el, sigma_el = cir_pars_e
  kappa_gas, beta_gas, alpha_gas, sigma_gas = cir_pars_g

  ae, kappa_le, alpha_le, delta_ee, delta_eg= me_pars_e
  ag, kappa_lg, alpha_lg, delta_gg, delta_ge = me_pars_g

  # exected value of X^2
  z2 = var_trunc_norm(jump_pars_e, abs_val = False)
  y2 = var_trunc_norm(jump_pars_g, abs_val = False)

  # expected values of X|X|
  z_abs2 = var_trunc_norm(jump_pars_e, abs_val = True)
  y_abs2 = var_trunc_norm(jump_pars_g, abs_val = True)

  # expected value of X
  z_mean = mean_trunc_norm(jump_pars_e, abs_val = False)
  y_mean = mean_trunc_norm(jump_pars_g, abs_val = False)

  # expected values of |X|
  z_abs_mean = mean_trunc_norm(jump_pars_e, abs_val = True)
  y_abs_mean = mean_trunc_norm(jump_pars_g, abs_val = True)

  ####################################################################################################
  # EXPECTED VALUES AND VOLATILITIES OF FACTORS
  ####################################################################################################
  mean_cir_e, mean_cir_g, vol_cir, h_func_e, g_func_e = moments_cir(time = time.copy(), pars_e = cir_pars_e.copy(), pars_g = cir_pars_g.copy(), initial_val = init_value[:2].copy())

  mean_meh_e, mean_meh_g, vol_meh, h_func_g, g_func_g = moments_meh( time = time.copy(),  pars_le = me_pars_e.copy(), pars_lg = me_pars_g.copy(),  pars_jump_el = jump_pars_e.copy(), pars_jump_gas = jump_pars_g.copy() ,initial_val = init_value[2:].copy())
  
  h_func = h_func_e
  g_func = g_func_e

  h_func[0,:] = h_func[0,:] + h_func_g[0,:]
  g_func[0,:] = g_func[0,:] + g_func_g[0,:]

  h_func = np.concatenate((h_func, h_func_g[1:]), axis =0)
  g_func = np.concatenate((g_func, g_func_g[1:]), axis = 0)

  h_func = h_func * seasonal_e
  g_func = g_func * seasonal_g

  ####################################################################################################
  # OPTION VALUES
  ####################################################################################################
  opt_val = np.zeros((len(time)-1, len(hr)))

  opt_hedge = np.zeros((len(time)-1, 2, len(hr)))

  for k in range(1, len(time)):
    print(k)

    lb_e = seasonal_e[k]*(np.maximum(mean_cir_e[k] - L*np.sqrt(vol_cir[0,k]),0) + mean_meh_e[k] - L*np.sqrt(vol_meh[0,k]) )

    ub_e = seasonal_e[k]*(mean_cir_e[k] + L*np.sqrt(vol_cir[0,k]) + mean_meh_e[k] + L*np.sqrt(vol_meh[0,k]) )
    
    # lb_g = seasonal_g[k]*(np.maximum(mean_cir_g[k] - L*np.sqrt(vol_cir[1,k]),0) + mean_meh_g[k] - L*np.sqrt(vol_meh[1,k]) )

    ub_g = seasonal_g[k]*(mean_cir_g[k] + L*np.sqrt(vol_cir[1,k]) + mean_meh_g[k] + L*np.sqrt(vol_meh[1,k]) )

    # lb_e = 0
    lb_g = 0
 
    p_e = lb_e + ( np.arange(0, Q) + 1/2)*(ub_e - lb_e)/Q
    p_g = lb_g + ( np.arange(0, Q) + 1/2)*(ub_g - lb_g)/Q

    def compute_F(i, l, k):

      u_e = seasonal_e[k] * i * np.pi / (ub_e - lb_e)
      v_g = seasonal_g[k] * l * np.pi / (ub_g - lb_g)

      exp_cir_p = cir_char( u = u_e, v = v_g, t= 0, T = time[k], pars_e = cir_pars_e, pars_g = cir_pars_g)

      exp_cir_n = cir_char( u = u_e, v = -v_g, t= 0, T = time[k], pars_e = cir_pars_e, pars_g = cir_pars_g)

      exp_meh_p = meh_char(u = u_e, v = v_g, t = 0,  T = time[k],  pars_le = me_pars_e, pars_lg = me_pars_g,  pars_jump_el = jump_pars_e, pars_jump_gas = jump_pars_g)

      exp_meh_n = meh_char(u = u_e, v = -v_g, t = 0,  T = time[k],  pars_le = me_pars_e, pars_lg = me_pars_g,  pars_jump_el = jump_pars_e, pars_jump_gas = jump_pars_g)

      exp_p =  exp_cir_p[0] + exp_cir_p[1:] @ init_value[:2] + exp_meh_p[0] + exp_meh_p[1:] @ init_value[2:]  - i* np.pi *1j*lb_e/(ub_e - lb_e) - l* np.pi *1j*lb_g/(ub_g - lb_g) 

      exp_p = np.exp( np.real(exp_p) + 1j*np.imag(exp_p)).real 

      exp_n = exp_cir_n[0] + exp_cir_n[1:] @ init_value[:2] + exp_meh_n[0] + exp_meh_n[1:] @ init_value[2:] - i* np.pi *1j*lb_e/(ub_e - lb_e) + l* np.pi *1j*lb_g/(ub_g - lb_g) 

      exp_n = np.exp( np.real(exp_n) + 1j*np.imag(exp_n)).real 

      # derivative wrt to a
      d1 = der_mgf_trunc_norm(a  = exp_meh_p[1].real, b = exp_meh_p[3].real*delta_ee + exp_meh_p[4].real*delta_ge, pars = jump_pars_e, Atrue = True)
      # derivative wrt to a
      d2 = der_mgf_trunc_norm(a  = exp_meh_n[1].real, b = exp_meh_n[3].real*delta_ee + exp_meh_n[4].real*delta_ge, pars = jump_pars_e, Atrue = True)
      # derivative wrt to b
      d3 = der_mgf_trunc_norm(a  = exp_meh_p[1].real, b = exp_meh_p[3].real*delta_ee + exp_meh_p[4].real*delta_ge, pars = jump_pars_e, Atrue = False)
      # derivative wrt to b
      d4 = der_mgf_trunc_norm(a  = exp_meh_n[1].real, b = exp_meh_n[3].real*delta_ee + exp_meh_n[4].real*delta_ge, pars = jump_pars_e, Atrue = False)

      # derivative wrt to a
      d5 = der_mgf_trunc_norm(a  = exp_meh_p[2].real, b = exp_meh_p[3].real*delta_eg + exp_meh_p[4].real*delta_gg, pars = jump_pars_g, Atrue = True)
      # derivative wrt to a
      d6 = der_mgf_trunc_norm(a  = exp_meh_n[2].real, b = exp_meh_n[3].real*delta_eg + exp_meh_n[4].real*delta_gg, pars = jump_pars_g, Atrue = True)
      # derivative wrt to b
      d7 = der_mgf_trunc_norm(a  = exp_meh_p[2].real, b = exp_meh_p[3].real*delta_eg + exp_meh_p[4].real*delta_gg, pars = jump_pars_g, Atrue = False)
      # derivative wrt to b
      d8 = der_mgf_trunc_norm(a  = exp_meh_n[2].real, b = exp_meh_n[3].real*delta_eg + exp_meh_n[4].real*delta_gg, pars = jump_pars_g, Atrue = False)

      deriv = [d1, d2, d3, d4, d5, d6, d7, d8]

      return (i, l, exp_p, exp_n, exp_cir_p, exp_cir_n, deriv)
    
    F_12_p = np.zeros((N1,N2))
    F_12_n = np.zeros((N1,N2))

    # derivative wrt x_e
    F_12_p_e = np.zeros((N1,N2))
    F_12_n_e = np.zeros((N1,N2))

    # derivative wrt x_g
    F_12_p_g = np.zeros((N1,N2))
    F_12_n_g = np.zeros((N1,N2))

    # derivative lambda_e
    F_12_p_le = np.zeros((N1,N2))
    F_12_n_le = np.zeros((N1,N2))

    # derivative lambda_e ( with absolute value)
    F_12_p_le2 = np.zeros((N1,N2))
    F_12_n_le2 = np.zeros((N1,N2))

    # derivative lambda_g
    F_12_p_lg = np.zeros((N1,N2))
    F_12_n_lg = np.zeros((N1,N2))

    # derivative lambda_g ( with absolute value)
    F_12_p_lg2 = np.zeros((N1,N2))
    F_12_n_lg2= np.zeros((N1,N2))

    results = Parallel(n_jobs=14)(delayed(compute_F)(i, l, k) for i in range(N1) for l in range(N2))

    for i, l, exp_p, exp_n, exp_cir_p, exp_cir_n, deriv in results:
      
      F_12_p[i, l] = exp_p
      F_12_n[i, l] = exp_n

      # derivative wrt x_e
      F_12_p_e[i, l] = exp_p * exp_cir_p[1].real
      F_12_n_e[i, l] = exp_n * exp_cir_n[1].real

      # derivative wrt x_g
      F_12_p_g[i, l] = exp_p * exp_cir_p[2].real
      F_12_n_g[i, l] = exp_n * exp_cir_n[2].real

      # derivative wrt l_e
      F_12_p_le[i, l] = exp_p * (deriv[0] - z_mean)
      F_12_n_le[i, l] = exp_n * (deriv[1] - z_mean)

      # derivative wrt l_e abs val
      F_12_p_le2[i, l] = exp_p * (deriv[2] - z_abs_mean)
      F_12_n_le2[i, l] = exp_n * (deriv[3] - z_abs_mean)

      # derivative wrt l_g
      F_12_p_lg[i, l] = exp_p * (deriv[4] - y_mean)
      F_12_n_lg[i, l] = exp_n * (deriv[5] - y_mean)

      # derivative wrt l_g abs val
      F_12_p_lg2[i, l] = exp_p * (deriv[6] - y_abs_mean)
      F_12_n_lg2[i, l] = exp_n * (deriv[7] - y_abs_mean)


    # original values
    F_12_p[0,:] = F_12_p[0,:]/2
    F_12_p[:,0] = F_12_p[:,0]/2

    F_12_n[0,:] = F_12_n[0,:]/2
    F_12_n[:,0] = F_12_n[:,0]/2


    # derivative wrt to x_e
    F_12_p_e[0,:] = F_12_p_e[0,:]/2
    F_12_p_e[:,0] = F_12_p_e[:,0]/2

    F_12_n_e[0,:] = F_12_n_e[0,:]/2
    F_12_n_e[:,0] = F_12_n_e[:,0]/2

    # derivative wrt to x_g
    F_12_p_g[0,:] = F_12_p_g[0,:]/2
    F_12_p_g[:,0] = F_12_p_g[:,0]/2

    F_12_n_g[0,:] = F_12_n_g[0,:]/2
    F_12_n_g[:,0] = F_12_n_g[:,0]/2

    # derivative wrt to le
    F_12_p_le[0,:] = F_12_p_le[0,:]/2
    F_12_p_le[:,0] = F_12_p_le[:,0]/2

    F_12_n_le[0,:] = F_12_n_le[0,:]/2
    F_12_n_le[:,0] = F_12_n_le[:,0]/2

    # derivative wrt to le2
    F_12_p_le2[0,:] = F_12_p_le2[0,:]/2
    F_12_p_le2[:,0] = F_12_p_le2[:,0]/2

    F_12_n_le2[0,:] = F_12_n_le2[0,:]/2
    F_12_n_le2[:,0] = F_12_n_le2[:,0]/2

    # derivative wrt to lg
    F_12_p_lg[0,:] = F_12_p_lg[0,:]/2
    F_12_p_lg[:,0] = F_12_p_lg[:,0]/2

    F_12_n_lg[0,:] = F_12_n_lg[0,:]/2
    F_12_n_lg[:,0] = F_12_n_lg[:,0]/2

    # derivative wrt to lg2
    F_12_p_lg2[0,:] = F_12_p_lg2[0,:]/2
    F_12_p_lg2[:,0] = F_12_p_lg2[:,0]/2

    F_12_n_lg2[0,:] = F_12_n_lg2[0,:]/2
    F_12_n_lg2[:,0] = F_12_n_lg2[:,0]/2

    ####################################################################################################
    # MATRIX A FROM QUADRATIC HEDGE ... SAME REGARDELSS OF HR
    ####################################################################################################

    a1 = h_func[1,k]**2 * sigma_el**2 * init_value[0] + \
         h_func[2,k]**2 * sigma_gas* init_value[1] + \
        (h_func[3,k]**2 *z2 + 2*h_func[3,k]*(h_func[5,k]*delta_ee + h_func[6,k]*delta_ge)*z_abs2 + (h_func[5,k]*delta_ee + h_func[6,k]*delta_ge)**2 * z2)*init_value[4] + \
        (h_func[4,k]**2 *y2 + 2*h_func[4,k]*(h_func[5,k]*delta_eg + h_func[6,k]*delta_gg) * y_abs2 + (h_func[5,k]*delta_eg + h_func[6,k]*delta_gg)**2 * y2)*init_value[5]

    a2 = g_func[1,k]*h_func[1,k]* sigma_el**2 * init_value[0] + \
         g_func[2,k]*h_func[2,k]* sigma_gas **2 * init_value[1] + \
        (g_func[3,k]*h_func[3,k] ** z2 + g_func[3,k]*(h_func[5,k]*delta_ee + h_func[6,k]*delta_ge)*z_abs2 + h_func[3,k]*(g_func[5,k]*delta_ee + g_func[6,k]*delta_ge)*z_abs2 + (h_func[5,k]*delta_ee + h_func[6,k]*delta_ge) * (g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) * z2)* init_value[4] + \
        (g_func[4,k]*h_func[4,k] ** y2 + g_func[4,k]*(h_func[5,k]*delta_eg + h_func[6,k]*delta_gg)*y_abs2 + h_func[4,k]*(g_func[5,k]*delta_eg + g_func[6,k]*delta_gg)*y_abs2 + (h_func[5,k]*delta_eg + h_func[6,k]*delta_gg) * (g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) * y2) * init_value[5]

    a3 = h_func[1,k]*g_func[1,k] * sigma_el**2 * init_value[0] + \
         h_func[2,k]*g_func[2,k] * sigma_gas**2 * init_value[1] + \
         (h_func[3,k] * g_func[3,k] * z2 + h_func[3,k]*(g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) * z_abs2 + g_func[3,k] *(h_func[5,k]*delta_ee + h_func[6,k]*delta_ge) * z_abs2 + (g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) *(h_func[5,k]*delta_ee + h_func[6,k]*delta_ge) * z2)* init_value[4] + \
         (h_func[4,k] * g_func[4,k] * y2 + h_func[4,k]*(g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) * y_abs2 + g_func[4,k] *(h_func[5,k]*delta_eg + h_func[6,k]*delta_gg) * y_abs2 + (g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) *(h_func[5,k]*delta_eg + h_func[6,k]*delta_gg) * y2)* init_value[5] 

    a4 = g_func[1,k]**2 * sigma_el**2 * init_value[0] + \
         g_func[2,k]**2 * sigma_gas**2 *init_value[1] + \
         (g_func[3,k] * g_func[3,k] * z2 + g_func[3,k]*(g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) * z_abs2 + g_func[3,k] *(g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) * z_abs2 + (g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) *(g_func[5,k]*delta_ee + g_func[6,k]*delta_ge) * z2)* init_value[4] + \
         (g_func[4,k] * g_func[4,k] * y2 + g_func[4,k]*(g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) * y_abs2 + g_func[4,k] *(g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) * y_abs2 + (g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) *(g_func[5,k]*delta_eg + g_func[6,k]*delta_gg) * y2)* init_value[5] 

    A = np.array([[a1, a2], [a3, a4]])

    for m in range(0, len(hr)):
      
      g_eg = np.maximum(p_e[:, np.newaxis] - hr[m]*p_g, 0)

      V_12 = dct(dct(g_eg, axis=0), axis=1)/Q**2

      V_12 = V_12[0:N1, 0:N2]

      opt_val[k-1,m] = np.sum((F_12_p + F_12_n)*V_12/2)

      b1 = -np.sum((F_12_p_e + F_12_n_e)*V_12/2)*h_func[1,k]* sigma_el**2 * init_value[0] - \
            np.sum((F_12_p_g + F_12_n_g)*V_12/2)*h_func[2,k]* sigma_gas**2 * init_value[1] - \
            (h_func[3,k]*np.sum((F_12_p_le + F_12_n_le)*V_12/2) + \
            (h_func[5,k]*delta_ee + h_func[6,k]*delta_ge)*np.sum((F_12_p_le2 + F_12_n_le2)*V_12/2) ) * init_value[4] - \
            (h_func[4,k]*np.sum((F_12_p_lg + F_12_n_lg)*V_12/2) + \
            (h_func[5,k]*delta_eg + h_func[6,k]*delta_gg)*np.sum((F_12_p_lg2 + F_12_n_lg2)*V_12/2) ) * init_value[5]

      b2 = -np.sum((F_12_p_e + F_12_n_e)*V_12/2)*g_func[1,k]* sigma_el**2 * init_value[0] - \
            np.sum((F_12_p_g + F_12_n_g)*V_12/2)*g_func[2,k]* sigma_gas**2 * init_value[1] - \
            (g_func[3,k]*np.sum((F_12_p_le + F_12_n_le)*V_12/2) + \
            (g_func[5,k]*delta_ee + g_func[6,k]*delta_ge)*np.sum((F_12_p_le2 + F_12_n_le2)*V_12/2) ) * init_value[4] - \
            (g_func[4,k]*np.sum((F_12_p_lg + F_12_n_lg)*V_12/2) + \
            (g_func[5,k]*delta_eg + g_func[6,k]*delta_gg)*np.sum((F_12_p_lg2 + F_12_n_lg2)*V_12/2) ) * init_value[5]

      b = np.array([b1,b2])

      opt_hedge[k-1,:,m] = np.linalg.solve(A, b)

  return opt_val, opt_hedge

hr = np.array([1/.30, 1/0.35, 1/0.40, 1/0.45, 1/0.5, 1/0.55, 1/0.60, 1/0.65])


opt_val, opt_hedge = ss_option(hr = hr.copy(), 
                      Q = 2000, 
                      N1 = 100, 
                      N2 = 100, 
                      L = 8, 
                      time = np.arange(0,1, 1/252),
                      cir_pars_e = cir_pars_e_Q.copy(),
                      cir_pars_g = cir_pars_g_Q.copy(),
                      me_pars_e = pars_e_Q.copy(),
                      me_pars_g = pars_g_Q.copy(),
                      jump_pars_e = jump_pars_e_Q.copy(),
                      jump_pars_g = jump_pars_g_Q.copy(),
                      init_value = initial_val_Q.copy(),
                      seasonal_e = seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = np.arange(0,1, 1/252) ),
                      seasonal_g = seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = np.arange(0,1, 1/252) )  )


# hr = hr.copy()
# Q = 2000
# N1 = 100
# N2 = 100
# L = 8
# time = np.arange(0,1, 1/252)
# cir_pars_e = cir_pars_e_Q.copy()
# cir_pars_g = cir_pars_g_Q.copy()
# me_pars_e = pars_e_Q.copy()
# me_pars_g = pars_g_Q.copy()
# jump_pars_e = jump_pars_e_Q.copy()
# jump_pars_g = jump_pars_g_Q.copy()
# init_value = initial_val_Q.copy()
# seasonal_e = seas_func( coef= reg_el.params, period = 252, cnt_terms = 1, time = np.arange(0,1, 1/252) )
# seasonal_g = seas_func(coef= reg_gas.params, period = 252, cnt_terms = 1, time = np.arange(0,1, 1/252) )  

# save_object(opt_val, 'DATA_OUT/option_values_in_year.pkl')
# save_object(opt_hedge, 'DATA_OUT/opt_hedge_in_year.pkl')
opt_val = load_object('DATA_OUT/option_values_in_year.pkl')
opt_hedge = load_object('DATA_OUT/opt_hedge_in_year.pkl')

fig = go.Figure()

# fig.add_trace(go.Scatter(
#     name="Price of 1",
#      y =  opt_hedge[:,0]
# ))
opt_val.shape

fig.add_trace(go.Scatter(
    name="Price of 2",
     y=opt_val[:,1]
))

fig.add_trace(go.Scatter(
    name="Price of 3",
     y =  opt_val[:,2]
))

fig.add_trace(go.Scatter(
    name="Price of 4",
     y=opt_val[:,3] 
))


fig.add_trace(go.Scatter(
    name="Price of 5",
     y=opt_val[:,4]
))



fig.add_trace(go.Scatter(
    name="Price of 6",
     y=opt_val[:,5] 
))



fig.add_trace(go.Scatter(
    name="Price of 7",
     y=opt_val[:,6]
))



fig.add_trace(go.Scatter(
    name="Price of 8",
     y=opt_val[:,7] 
))


fig.show()



fig = go.Figure()

fig.add_trace(go.Scatter(
    name="Price of 1",
     y =  opt_hedge[:,0,7]
))

fig.add_trace(go.Scatter(
    name="Price of 2",
     y=opt_hedge[:,1,7]
))

fig.show()

import matplotlib.colors as mcolors

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
import matplotlib.colors as mcolors
import pandas as pd

# Apply scientific plot style
plt.rcParams.update({
    "font.family": "serif",
    "font.size": 12,
    "axes.labelsize": 12,
    "legend.fontsize": 11,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "figure.figsize": (9, 5),
    "axes.grid": True,
    "grid.alpha": 0.3,
})

# Generate daily date range for 2025
date_range = pd.date_range(start="2025-01-01", end="2025-12-31", periods = 251)
x = date_range[:opt_val.shape[0]]  # truncate or match if shorter

# Generate gradient colors from teal to coral
start_color = mcolors.to_rgb("#00897D")
end_color = mcolors.to_rgb("#FF6B6B")
colors = [(
    start_color[0] + (end_color[0] - start_color[0]) * i / 7,
    start_color[1] + (end_color[1] - start_color[1]) * i / 7,
    start_color[2] + (end_color[2] - start_color[2]) * i / 7
) for i in range(8)]

# Plot
fig, ax = plt.subplots()
for i in range(opt_val.shape[1]):
    ax.plot(x, opt_val[:, i], label=f"Hear-rate {round(hr[i],2)}", color=colors[i])

# Format x-axis as dates
ax.set_xlabel("Date")
ax.set_ylabel("Spark-spread Option Value [EUR/MWh]")

ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.xticks(rotation=45)

ax.legend()
ax.grid(True, linestyle='--', alpha=0.5)
plt.tight_layout()

plt.savefig("GRAPHS/Spark_spread_option.png", dpi=300)
plt.show()


# Apply scientific plot style
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

# Generate daily date range for 2025
date_range = pd.date_range(start="2025-01-01", end="2025-12-31", periods = 251)
x = date_range[:opt_val.shape[0]]  # truncate or match if shorter

# Generate gradient colors from teal to coral
start_color = mcolors.to_rgb("#00897D")
end_color = mcolors.to_rgb("#FF6B6B")
colors = [(
    start_color[0] + (end_color[0] - start_color[0]) * i / 7,
    start_color[1] + (end_color[1] - start_color[1]) * i / 7,
    start_color[2] + (end_color[2] - start_color[2]) * i / 7
) for i in range(8)]

# Plot
fig, ax = plt.subplots()
for i in range(opt_hedge.shape[2]):
    ax.plot(x, -opt_hedge[:,0,i], label=f"Heat-rate {round(hr[i],2)}", color=colors[i])

# Format x-axis as dates
ax.set_xlabel("Date")
ax.set_ylabel("Optimal hedge position in Electricity")

ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.xticks(rotation=45)

ax.legend()
ax.grid(True, linestyle='--', alpha=0.5)
plt.tight_layout()

plt.savefig("GRAPHS/Electricity_hedge.png", dpi=300)
plt.show()




# Apply scientific plot style
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

# Generate daily date range for 2025
date_range = pd.date_range(start="2025-01-01", end="2025-12-31", periods = 251)
x = date_range[:opt_val.shape[0]]  # truncate or match if shorter

# Generate gradient colors from teal to coral
start_color = mcolors.to_rgb("#00897D")
end_color = mcolors.to_rgb("#FF6B6B")
colors = [(
    start_color[0] + (end_color[0] - start_color[0]) * i / 7,
    start_color[1] + (end_color[1] - start_color[1]) * i / 7,
    start_color[2] + (end_color[2] - start_color[2]) * i / 7
) for i in range(8)]

# Plot
fig, ax = plt.subplots()
for i in range(opt_hedge.shape[2]):
    ax.plot(x, -opt_hedge[:,1,i], label=f"Heat-rate {round(hr[i],2)}", color=colors[i])

# Format x-axis as dates
ax.set_xlabel("Date")
ax.set_ylabel("Optimal hedge position in Natural Gas")

ax.xaxis.set_major_locator(mdates.MonthLocator())
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %Y'))
plt.xticks(rotation=45)

ax.legend()
ax.grid(True, linestyle='--', alpha=0.5)
plt.tight_layout()

plt.savefig("GRAPHS/Gas_hedge.png", dpi=300)
plt.show()



92.91 - hr*48.646


####################################################################################################
# Simulation of bivariate cir process to check wheter or not it corresponds
####################################################################################################

def bivariate_cir_sim(pars_el, pars_gas, dt, T):

  kappa_e, alpha_e, sigma_e = pars_el
  kappa_g, alpha_g, sigma_g = pars_gas

  intervals = np.arange(0, T, dt)
  if intervals[-1] != T: 
      intervals = np.append(intervals, T)

  cir_gas = np.ones(len(intervals) + 1)
  cir_el = np.ones(len(intervals) + 1)

  for i in range(0, len(intervals)):

    residuals1, residuals2 = sc.stats.norm.rvs(loc=0, scale=1, size=2)

    cir_gas[i+1] = cir_gas[i] + kappa_g*(1-alpha_g + alpha_g*cir_el[i] - cir_gas[i])*dt + sigma_g*np.sqrt(dt*np.maximum(cir_gas[i],0))*residuals2

    cir_el[i+1] = cir_el[i] + kappa_e*(1 - alpha_e + alpha_e*cir_gas[i] - cir_el[i])*dt + sigma_e*np.sqrt(dt*np.maximum(cir_el[i],0))*residuals1

  return cir_el, cir_gas

cir_el, cir_gas = bivariate_cir_sim(pars_el = pars_e, pars_gas = pars_g, dt = 1/(252*50), T = 10/252)

n_sim = 5* 10**4

cir_el = np.zeros((n_sim, 502))
cir_gas = np.zeros((n_sim, 502))

for n in range(0, n_sim):
  print(n)
  cir_el[n,:], cir_gas[n,:] =  bivariate_cir_sim(pars_el = pars_e, pars_gas = pars_g, dt = 1/(252*50), T = 10/252)

cir_el = cir_el*68
cir_gas = cir_gas*24

# fig_out = go.Figure(
#     data = go.Scatter(y =cir_el[:,-1], name="Orig El",)
# )

# fig_out.add_trace(go.Scatter(
#     name="New El",
#     y= cir_gas[:,-1]
# ))

# fig_out.add_trace(go.Scatter(
#     name="difference",
#     y= cir_el[:,-1] - hr*cir_gas[:,-1]
# ))

# fig_out.show()

np.average(np.maximum(cir_el[:,-1] - hr*cir_gas[:,-1],0))
# np.float64(6.653147394846042)
# np.float64(6.471261541053853)