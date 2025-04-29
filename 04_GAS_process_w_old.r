# GAS with conditional MVN

gas_cMVN <- function(   static_param = NULL,                # parameter to be optimized
                        data = NULL,
                        score_fun = NULL,
                        gas_a_lag = 1,
                        gas_b_lag = 1,
                        tau = NULL,
                        out = FALSE,
                        distr = "norm",
                        tv_par = NULL,
                        start_param = NULL,
                        h = NULL,
                        type = "NS",
                        dcov = FALSE,
                        delta_l = 0,
                        sigma_weights = NULL
                       ){

    # type - one of NS (Nelson-Siegel), NSS (Nelson-Siegel-Svensson), BL (Bliss)
    
    # delta_l only needed for NSS and BL models to separate lambda by at least constant

    n_tv_param <- sum(tv_par)
   
    A <- matrix(static_param[1:(gas_a_lag*n_tv_param)], 
                ncol = gas_a_lag, 
                nrow = n_tv_param)

    B <- matrix(static_param[(gas_a_lag*n_tv_param+1):((gas_b_lag + gas_a_lag)*n_tv_param)], 
               ncol = gas_b_lag, 
               nrow = n_tv_param)

    long_mu <- static_param[(gas_b_lag + gas_a_lag )*n_tv_param + 1:length(tv_par)]

    # S_t matica, same dimension as A, setting values to 0
    S_t <- A*0
    
    # same dimension as matrix B, setting all values as starting value
    ind <- which(tv_par)

    param_out <- c()
    data_out <- c()
    llh_out <- c()
    
    # omega
    omega <- long_mu[tv_par]*( rep(1, n_tv_param) - rowSums(B))

    # only if lambda is not time-varying other-wise using nonlinear SS method prior to GAS_proces
    # from histori data
    #if(!all(tv_par)){
        if(type == "NS"){

            # starting parameter as linear regression from first 5 values 
            lambda <- exp(long_mu[4])

            # culaculation of X matrix
            lt1 <- -lambda*tau

            lt1[4] <- -exp(log(lambda) + log(3))

            exp_l <- exp(lt1)

            const <- expm1(lt1)/(lt1)

            # matrix X
            X_mat <- rbind(rep(1, length(tau)), 
                            const,  
                            const - exp_l)

            # first 
            Y_ini <- as.vector(data[1,])

            start_param <- solve(X_mat%*%t(X_mat))%*%X_mat%*%Y_ini

            # first vector of parameters
            start_param <- c(start_param,long_mu[4])

        }else if (type == "NSS"){

            # starting parameter as linear regression from first 5 values 
            lambda1 <- exp(long_mu[5]) + exp(long_mu[6]) + delta_l

            lambda2 <- exp(long_mu[6])

            # culaculation of X matrix
            lt1 <- -lambda1*tau
            lt2 <- -lambda2*tau

            lt1[4] <- -exp(log(lambda1) + log(3))
            lt2[4] <- -exp(log(lambda2) + log(3))

            exp_l1 <- exp(lt1)
            exp_l2 <- exp(lt2)

            const1 <- expm1(lt1)/(lt1)
            const2 <- expm1(lt2)/(lt2)

            # matrix X
            X_mat <- rbind(rep(1, length(tau)), 
                            const1,  
                            const1 - exp_l1,
                            const2 - exp_l2)
            X_mat2 <- X_mat[-4,]

            # first 
            Y_ini <- as.vector(data[1,])

            start_param <- solve(X_mat%*%t(X_mat))%*%X_mat%*%Y_ini

            # first vector of parameters
            start_param <- c(start_param,long_mu[5], long_mu[6])

        }else{
            # starting parameter as linear regression from first 5 values 
            lambda1 <- exp(long_mu[4])
            lambda2 <- exp(long_mu[5])

            # culaculation of X matrix
            lt1 <- -lambda1*tau
            lt2 <- -lambda2*tau

            lt1[4] <- -exp(log(lambda1) + log(3))
            lt2[4] <- -exp(log(lambda2) + log(3))

            exp_l1 <- exp(lt1)
            exp_l2 <- exp(lt2)

            const1 <- expm1(lt1)/(lt1)
            const2 <- expm1(lt2)/(lt2)

            # matrix X
            X_mat <- rbind(rep(1, length(tau)), 
                            const1,
                            const2 - exp_l2)

            # first 
            Y_ini <- as.vector(data[1,])

            start_param <- solve(X_mat%*%t(X_mat))%*%X_mat%*%Y_ini

            # first vector of parameters
            start_param <- c(start_param, long_mu[4], long_mu[5])
        }

   # }
    
    if(dcov){
        start_param <- c(start_param, long_mu[length(tv_par)])
    }
    
    start_param <- as.matrix(start_param, ncol = 1)

    t_param <- start_param[ind,ncol(start_param):1]

    t_param <- matrix(t_param, nrow =  n_tv_param, ncol = ncol(start_param))

     # first vector of parameters
    t_param_new <- start_param
    
    
    if(distr == "norm"){

        cov_mat <- diag(static_param[-c(1:((gas_b_lag + gas_a_lag )*n_tv_param + length(tv_par)  ))]* sigma_weights)

        nu <- 0

    }else if (distr == "t") {

        nu <- exp(static_param[(gas_b_lag + gas_a_lag )*n_tv_param + length(tv_par) + 1]) + 2

        cov_mat <- diag(static_param[-c(1:((gas_b_lag + gas_a_lag )*n_tv_param + length(tv_par) + 1))]*sigma_weights)

    }

    

    # inverse of covariance matrix
    #cov_inv <- diag(1/diag(cov_mat))


    for( i in 1:gas_a_lag){
    S_t_new <- score_fun( x = data[i,],
                          #sigma_inv = cov_inv,
                          sigma = cov_mat,
                          pars = t_param_new[,i],
                          h = h,
                          tau = tau,
                          tv_par = tv_par,
                          nu = nu,
                          type = type,
                          delta_L = delta_l,
                          dcov = dcov,
                          out = out,
                          sigma_weights = sigma_weights)
    
    S_t[,i] <- S_t_new$score

    }



    # reversing columns of S_t so the first will be newest S_t
    S_t <- as.matrix(S_t[,rev( 1: ncol(S_t) )])
    

    # start of iteration 
    start_it <- gas_a_lag+1
 
    # end of iteration
    end_it <- nrow(data)

    # log likelihood 
    negative_llh <- 0

    for( i in start_it:end_it){

        # new parameters
        t_param_new[tv_par] <- omega + rowSums(A*S_t) + rowSums(B*t_param)
  
        # updating tv_param and S_t
        if(gas_b_lag != 0){
            t_param[,min(gas_b_lag,2):gas_b_lag] <- t_param[,1:(max(gas_b_lag,2)-1)]
        }

        t_param[,1] <- t_param_new[tv_par]


        # new score function and negative log likelihood
        S_t_new <- score_fun( x = data[i,],
                                #sigma_inv = cov_inv,
                                pars = t_param_new,
                                sigma = cov_mat,
                                h = h,
                                tv_par = tv_par,
                                tau = tau,
                                nu = nu,
                                type = type,
                                delta_L = delta_l,
                                dcov = dcov,
                                out = out,
                                sigma_weights = sigma_weights)
       
        if(out == TRUE){
            param_out <- cbind(param_out, S_t_new$out_pars)
            data_out <- cbind(data_out, c(data[i,], S_t_new$mu))
            llh_out <- cbind(llh_out, S_t_new$llh_yields)
        }

        S_t[,min(gas_a_lag,2):gas_a_lag] <- S_t[,1:(max(gas_a_lag,2)-1)]        
        S_t[,1] <- S_t_new$score

        #print(S_t_new$llh)

        # loglikelihood after iteration
        negative_llh <- negative_llh - S_t_new$llh


    }


print(negative_llh)

if( out == TRUE){
    par_cnt <- length(static_param)
    return(list(pars = param_out, values = data_out, llh = llh_out, par_cnt = par_cnt))
}else{
    return(negative_llh)
}
}
