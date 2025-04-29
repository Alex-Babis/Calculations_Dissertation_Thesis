# Creating density and moment generating functions for mixture of truncated normal distribution that deals with positive and negative jumps separately

import numpy as np 
from scipy.stats import norm, truncnorm

# Density function of double truncate normal distribution
# Input parameters are:
# mu, sigma <- different for negative (n) and positive (p) values
# prob <- importance of one side or other from interval (0,1)
# x <- value in which we want to calculate density
# c_p <- constant for positive part
# c_n <- constant of negative part

def dtrunc_norm(mu_p, mu_n, sigma_p, sigma_n, prob, x):
  
  # indices of 
  ind_p = np.where(x > 0 )
  
  ind_n = np.where(x <= 0 )

  x_p = x[ind_p]
  
  x_n = x[ind_n]
  
  # constant for postivie values 
  c_p = 1/np.maximum(norm.cdf(mu_p/sigma_p),0.000001)
  

  # constant for negative values
  c_n = 1/np.maximum(norm.cdf(-mu_n/sigma_n),0.000001)
  
  # density for positive values
  d_p = prob*c_p*norm.pdf(x_p, loc = mu_p, scale = sigma_p)
  
  # np.exp(- (x_p - mu_p)**2/(sigma_p**2 * 2))/(np.sqrt(2*np.pi) * sigma_p)
  
  # density for negative values
  d_n = (1-prob)*c_n*norm.pdf(x_n, loc = mu_n, scale = sigma_n)
  
  # np.exp(- (x_n - mu_n)**2/(sigma_n**2 * 2))/(np.sqrt(2*np.pi) * sigma_n)
  
  d_out = np.zeros(len(x))

  d_out[ind_p] = d_p
  
  d_out[ind_n] = d_n
  
  return d_out

# numerical problem when x mu_p is not positive and when mu_n is not negative ... check the wikipedia for solution

# # Testing function
# x = np.arange(-1, 1, 0.001)

# y = dtrunc_norm(mu_p = 0.2, mu_n = -0.3, sigma_p = 0.1, sigma_n = 0.1, prob = 0.4, x = x)

# a_transformed, b_transformed = (- np.inf - (-0.3)) / 0.1, (0 - (-0.3)) / 0.1

# y2 = truncnorm.pdf(x, a = a_transformed, b = b_transformed, loc=-0.3, scale=0.1)* (1- 0.4)

# a_transformed, b_transformed = (0 - (0.2)) / 0.1, (np.inf - (0.2)) / 0.1
# y3 = truncnorm.pdf(x, a = a_transformed, b = b_transformed, loc=0.2, scale=0.1)*  0.4

# #

# dens_fig = go.Figure(
#   data = go.Scatter(x = x, y = y)
# )

# dens_fig.add_trace(go.Scatter(
#     name="from python",
#     x = x,
#     y=y2 ,
#     line = dict(color='red')
# ))

# dens_fig.add_trace(go.Scatter(
#     name="from python",
#     x = x,
#     y=y3 ,
#     line = dict(color='red')
# ))

# dens_fig.show()



# Moment generating function for ax + b|x| ... the special cases are when a = 0 or b = 0 
# function will return the value for moment generating function given a and b and given all parameters of truncated normal distribution 
# a is the part with original distribution
# b is the part with absolute values of x

def char_trunc_norm(a, b, pars):
  
  mu_p = pars[0]
  mu_n = pars[1]
  sigma_p = pars[2]
  sigma_n = pars[3]
  prob = pars[4]

  # constant for postivie values 
  c_p = 1/norm.cdf(mu_p/sigma_p)
  
  # constant for negative values
  c_n = 1/norm.cdf(-mu_n/sigma_n)

  log_exp_p = (a+b) * mu_p + (a+b)**2 * sigma_p**2 / 2 + norm.logcdf( mu_p / sigma_p + (a + b) * sigma_p)
  log_exp_n = (a-b) * mu_n + (a-b)**2 * sigma_n**2 / 2 + norm.logcdf(-mu_n / sigma_n - (a - b) * sigma_n)

  mgf = prob * c_p * np.exp( log_exp_p ) + (1-prob) * c_n * np.exp( log_exp_n) 

  return mgf


def der_mgf_trunc_norm(a, b, pars, Atrue):
  
  mu_p = pars[0]
  mu_n = pars[1]
  sigma_p = pars[2]
  sigma_n = pars[3]
  prob = pars[4]

  # constant for postivie values 
  c_p = 1/norm.cdf(mu_p/sigma_p)
  
  # constant for negative values
  c_n = 1/norm.cdf(-mu_n/sigma_n)

  log_exp_p = (a+b) * mu_p + (a+b)**2 * sigma_p**2 / 2 + norm.logcdf( mu_p / sigma_p + (a + b) * sigma_p)
  log_exp_n = (a-b) * mu_n + (a-b)**2 * sigma_n**2 / 2 + norm.logcdf(-mu_n / sigma_n - (a - b) * sigma_n)
  
  if Atrue == True:
    mgf = prob * c_p * (np.exp( log_exp_p )*(mu_p + (a+b)* sigma_p**2) + np.exp((a+b) * mu_p + (a+b)**2 * sigma_p**2 / 2 + norm.logpdf( x = mu_p / sigma_p + (a + b) * sigma_p)) * sigma_p) + \
      (1-prob) * c_n * (np.exp( log_exp_n )*(mu_n + (a-b)* sigma_n**2) + np.exp((a-b) * mu_n + (a-b)**2 * sigma_n**2 / 2 + norm.logpdf( x =-mu_n / sigma_n - (a - b) * sigma_n)) * (- sigma_n))
  else:
    mgf = prob * c_p * (np.exp( log_exp_p )*(mu_p + (a+b)* sigma_p**2) + np.exp((a+b) * mu_p + (a+b)**2 * sigma_p**2 / 2 + norm.logpdf( x = mu_p / sigma_p + (a + b) * sigma_p)) * sigma_p) + \
      (1-prob) * c_n * (np.exp( log_exp_n )*(- mu_n - (a-b)* sigma_n**2) + np.exp((a-b) * mu_n + (a-b)**2 * sigma_n**2 / 2 + norm.logpdf( x = -mu_n / sigma_n - (a - b) * sigma_n)) * sigma_n)

  return mgf


# Mean value of distribution absolute value of x (|x|)
def mean_trunc_norm(pars, abs_val = True):
  
  mu_p, mu_n, sigma_p, sigma_n, prob = pars

  # mean value of positive values
  a_p, b_p =  (0 - (mu_p)) /sigma_p, (np.inf - (mu_p)) / sigma_p
  mean_p = truncnorm.mean(a = a_p, b= b_p, loc = mu_p, scale = sigma_p)

  # mean_value of negative values
  a_n, b_n =  (-np.inf - (mu_n)) /sigma_n, (0 - (mu_n)) / sigma_n
  mean_n = truncnorm.mean(a = a_n, b= b_n, loc = mu_n, scale = sigma_n)

  if abs_val == True:
    mean_value = prob *mean_p + (1-prob)*abs(mean_n)
  else:
    mean_value = prob *mean_p + (1-prob)*mean_n
  
  return mean_value


# Mean value of distribution absolute value of x (|x|)
def var_trunc_norm(pars, abs_val = True):
  
  mu_p, mu_n, sigma_p, sigma_n, prob = pars

  # mean value of positive values
  a_p, b_p =  (0 - (mu_p)) /sigma_p, (np.inf - (mu_p)) / sigma_p
  var_p = truncnorm.moment(order = 2,a = a_p, b= b_p, loc = mu_p, scale = sigma_p)

  # mean_value of negative values
  a_n, b_n =  (-np.inf - (mu_n)) /sigma_n, (0 - (mu_n)) / sigma_n
  var_n = truncnorm.moment(order = 2, a = a_n, b= b_n, loc = mu_n, scale = sigma_n)

  if abs_val == True:
    var_value = prob *var_p - (1-prob)*abs(var_n)
  else:
    var_value = prob *var_p + (1-prob)*var_n
  
  return var_value




def mle_jump_distribution(pars, data):
  
  probs = dtrunc_norm( 
      mu_p = pars[0], 
      mu_n = pars[1],
      sigma_p = pars[2],
      sigma_n = pars[3], 
      prob = pars[4],
      x = data
    )
  
  result = np.where(probs > 0.0000000001, probs, -np.inf)

  negat_llh = -sum(np.log(result, out = result, where=result > 0))

  return negat_llh

