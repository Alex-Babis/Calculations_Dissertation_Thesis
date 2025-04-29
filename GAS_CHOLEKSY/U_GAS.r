# GAS with conditional MVN

gas_U <- function(static_param = NULL,                # parameter to be optimized
                  data = NULL,
                  score_fun = NULL,
                  gas_a_lag = 1,
                  gas_b_lag = 1,
                  lag_order = 1,
                  long_mu = NULL,
                  start_param = NULL,
                  n_tv_param = NULL,
                  dist = NULL,
                  Inv_h = NULL,
                  out = FALSE
                       ){

    # matrix A
    A <- matrix(static_param[1:(gas_a_lag*n_tv_param)], 
                ncol = gas_a_lag, 
                nrow = n_tv_param)

    # dimension of random vector (x,y,x_hist,y_hist)
    n <- (1+lag_order)

    # parameter nu - degree of freedom for multivariate normal
    if(dist == "norm"){
        nu <- 0

        #long_mu <-  start_param <- c(0,static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])
        long_mu <-  start_param <- c(static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])
       
    }else{

        # nu <- static_param[(gas_b_lag + gas_a_lag )*n_tv_param + 1]
        nu <- 9

        # long_mu <-  start_param <- c(static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param + 1))])

        long_mu <-  start_param <- c(static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])

    }
    
    # matrix B
    if(gas_b_lag == 0){

        B <- 0

        # calculating omega from long term mean and matrix B
        omega <- long_mu

    }else{
        B <- matrix(static_param[(gas_a_lag*n_tv_param  + 1):((gas_b_lag + gas_a_lag )*n_tv_param)], 
                ncol = gas_b_lag, 
                nrow = n_tv_param)

        # calculating omega from long term  mean and matrix B
        omega <- long_mu * ( rep(1, n_tv_param) - rowSums(B))
    }


    if(gas_b_lag == 0){
        t_param <- matrix(rep(start_param,1), 
                ncol = 1, 
                nrow = n_tv_param,
                byrow = FALSE)
    }else{
        t_param <- matrix(rep(start_param,gas_b_lag), 
                ncol = gas_b_lag, 
                nrow = n_tv_param,
                byrow = FALSE)
    }

    # S_t matica, same dimension as A, setting values to 0
    S_t <- A*0

    
    # empty list for output parameters
    if(out){
       param_out <- list() 

       cor_out <- c()
    }

    # vector of starting mean values
    mu_xy <- start_param[1]

    # vector of starting volatilties
    var_xy <- exp(start_param[2])

    # angle 
    ang <- pi/(1+ exp(-start_param[3]))

    row_x <- c( cos(ang), sin(ang))


    # cholesky matrix
    sigma_mat <- matrix(0,2,2)
    
    sigma_mat[1,1] <- 1
    sigma_mat[2,] <- row_x


    mu_vec <- rep(mu_xy, lag_order + 1)

    var_vec <- rep(var_xy, lag_order + 1)


    # log likelihood 
    negative_llh <- 0

    for( i in 1:gas_a_lag){
    S_t_new <- score_fun( x = data[ i:(i + lag_order) ], 
                            mu = mu_vec,
                            var = var_vec,
                            ang = ang,
                            sigma = sigma_mat,
                            h = Inv_h,
                            n = n,
                            nu = nu,
                            out = out)

    
    S_t[,i] <- S_t_new$score

    #  # loglikelihood after iteration
    # negative_llh <- negative_llh - S_t_new$llh


    # if(out == TRUE){            
    #     param_out <- list.append(param_out, list(x=S_t_new$x_cur,
    #                                             mu = S_t_new$mu,
    #                                             sigma = S_t_new$sigma,
    #                                             mu_c = S_t_new$mu_c, 
    #                                             sigma_c = S_t_new$sigma_c,
    #                                             cov = S_t_new$cov,
    #                                             hdens = S_t_new$hdens))
    # }
    
    }
    # reversing columns of S_t so the first will be newest S_t
    
    S_t <- as.matrix(S_t[,rev(1:ncol(S_t))])



    # start of iteration 
    start_it <- (lag_order+gas_a_lag+1)
    
    # end of iteration
    end_it <- length(data)


    for( i in start_it:end_it){

        # new parameters
        t_param_new <- omega + rowSums(A*S_t) + rowSums(B*t_param)

        # updating tv_param and S_t
        if(gas_b_lag != 0){
            t_param[,min(gas_b_lag,2):gas_b_lag] <- t_param[,1:(max(gas_b_lag,2)-1)]
        }

        t_param[,1] <- t_param_new
       
        # update mu_vec and sigma_mat
        mu_vec <- mu_vec[-1]
        mu_vec <- c(mu_vec,t_param_new[1])

        var_vec <- var_vec[-1]
        var_vec <- c(var_vec,exp(t_param_new[2]))

        # new matrix z
        ang <- pi/(1+ exp(-t_param_new[3]))

        sigma_mat[2,] <- c( cos(ang), sin(ang))

        # new score function and negative log likelihood
        S_t_new <- score_fun( x = data[(i - lag_order):i ],
                              mu = mu_vec,
                              var = var_vec,
                              ang = ang,
                              sigma = sigma_mat,
                              n = n,
                              h = Inv_h,
                              nu = nu,
                              out = out)


        if(out == TRUE && i > 62 ){
            param_out <- list.append(param_out, list(x=S_t_new$x_cur, 
                                                     mu = S_t_new$mu,
                                                     sigma = S_t_new$sigma,
                                                     mu_c = S_t_new$mu_c, 
                                                     sigma_c = S_t_new$sigma_c,
                                                     cov = S_t_new$cov,
                                                     hdens = S_t_new$hdens))

            cor_out  <-  cbind(cor_out,S_t_new$cor_out)
        }

        S_t[,min(gas_a_lag,2):gas_a_lag] <- S_t[,1:(max(gas_a_lag,2)-1)]        
        S_t[,1] <- S_t_new$score

        if( i > 62){
          # loglikelihood after iteration
          negative_llh <- negative_llh - S_t_new$llh
        }


    

    }

    #print(negative_llh)

    if( out == TRUE){
        return(list(param_out = param_out, cor_out = cor_out))
    }else{
        return(negative_llh)
    }
}



