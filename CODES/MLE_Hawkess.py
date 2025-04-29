# Fitting intensity dynamics of hawkess process
from scipy.stats import poisson
import numpy as np

def MLE_Hawkess_int(pars, jump_self, jump_other, dt):

  # mean reverting part
  kappa = pars[0]

  # long-term value
  theta = pars[1]

  # dependence on jump in self-exciting process
  gamma1 = pars[2]

  # dependence on jump in outsid-process
  gamma2 = pars[3]

  intensity = np.copy(jump_self)*0

  # initializing intensity as long-term value
  intensity[0] = theta

  for i in range(0, intensity.size - 1): 
    intensity[i+1] = intensity[i] + kappa*(theta - intensity[i])*dt + gamma1*abs(jump_self[i]) + gamma2*abs(jump_other[i])

  # jumps without their magnitude
  jumps = np.ones(len(jump_self))

  jumps[ jump_self == 0] = 0

  probs = poisson.pmf(jumps, intensity*dt, loc=0)
  
  result = np.where(probs > 0.000000000001, probs, -np.inf)

  negat_llh = -sum(np.log(result, out = result, where=result > 0))
  print(intensity[-1])
  return negat_llh
