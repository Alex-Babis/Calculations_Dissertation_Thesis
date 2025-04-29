# IMPORTING NEEDED LIBRARIES
import pandas as pd
import numpy as np
import scipy as sc
import plotly.graph_objects as go
import pickle
import calendar
from scipy.optimize import differential_evolution, minimize, fsolve,root, NonlinearConstraint
import multiprocessing
from datetime import date
import statsmodels.api as sm
import statsmodels.formula.api as smf
import operator
import importlib
from scipy.integrate import solve_ivp
from scipy.fftpack import dct
from scipy.stats import norm
from joblib import Parallel, delayed
import matplotlib.pyplot as plt

# CODES FOR DIFFERENTIAL EVOLUTION - SERIAL OPTIMIZATION
import CODES.MLE_CIR_fun 
import CODES.Trunc_norm_dist 
import CODES.FWD_fun 
importlib.reload(CODES.MLE_CIR_fun)
importlib.reload(CODES.Trunc_norm_dist)
importlib.reload(CODES.FWD_fun)

from CODES.MLE_CIR_fun import coupled_cir, uni_cir
from CODES.Trunc_norm_dist import dtrunc_norm, char_trunc_norm, der_mgf_trunc_norm, mean_trunc_norm, var_trunc_norm, mle_jump_distribution
from CODES.MLE_Hawkess import MLE_Hawkess_int
from CODES.FWD_fun import risk_neutral_fwd_price

import warnings
# Suppress the FutureWarning for Series.__getitem__
warnings.simplefilter(action='ignore', category=FutureWarning)

# FUNCTION TO SAVE OBJECTS AS PICKLE
def save_object(obj, filename):
    with open(filename, 'wb') as file:  # Overwrites any existing file.
        pickle.dump(obj, file, pickle.HIGHEST_PROTOCOL)

def load_object(filename):
    with open(filename, 'rb') as file: 
        return pickle.load(file)
    
    
exec(open('CODES/02_Data_seasonality.py').read())
    
exec(open('CODES/03_Start_parms.py').read())

# LOADING OPTIMIZED PARAMETERS OF PROCESSES
pars_e = load_object( 'DATA_OUT/pars_e.pkl')

pars_g = load_object( 'DATA_OUT/pars_g.pkl')

pars_jump_el = load_object( 'DATA_OUT/opt_el_jumps.pkl')

pars_jump_gas = load_object( 'DATA_OUT/opt_gas_jumps.pkl')

pars_le = load_object( 'DATA_OUT/opt_el_int.pkl')

pars_lg = load_object( 'DATA_OUT/opt_gas_int.pkl')

init_values = load_object( 'DATA_OUT/init_values.pkl')

# GAS FORWARD CURVE
gas_fwd = load_object( 'DATA_IN/Data_Gas2024.pkl')

el_fwd = load_object('DATA_IN/Data_Elektrina2024.pkl')

