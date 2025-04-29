# GAS with conditional MVN

gas_M <- function(   static_param = NULL,
                        data = NULL,
                        score_fun = grad_lognorm,
                        gas_a_lag = 1,
                        gas_b_lag = 1,
                        lag_order = 1,
                        long_mu = NULL,
                        n_tv_param = NULL,
                        dist = NULL,
                        Inv_h = NULL,
                        out = FALSE
                       ){

    # dimension of random vector (x,y,x_hist,y_hist)
    n <- 2*(1+lag_order)


    # parameter nu - degree of freedom for multivariate normal
    if(dist == "norm"){
        nu <- 0

        #long_mu <-  start_param <- c(0,0,static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])
        long_mu <-  start_param <- c(static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])
        # start_param <- long_mu    
        
    }else{
        # nu <- static_param[((gas_b_lag + gas_a_lag )*n_tv_param + 1 )]

        # long_mu <- start_param <- c(0,0,static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param + 1 ))])
        
        nu <- 8

        long_mu <- start_param <- c(static_param[-(1:((gas_b_lag + gas_a_lag )*n_tv_param))])


    }

    # matrix A
    A <- matrix(static_param[1:(gas_a_lag*n_tv_param)], 
                ncol = gas_a_lag, 
                nrow = n_tv_param)

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

    # creating vectors of start values
    # mean value 
    mu_xy <- start_param[1:2]


    # variances 
    var_xy <- exp(start_param[3:4])
    # fac mat

    # angles
    ang <- pi/(1+ exp(-start_param[-c(1:4)]))

    # last two rows of correlation matrix
    row_x <- c( cos(ang[1]), sin(ang[1])*cos(ang[2]), sin(ang[1])*sin(ang[2]), 0)
    row_y <- c( cos(ang[3]), sin(ang[3])*cos(ang[4]), sin(ang[3])*sin(ang[4])*cos(ang[5]), sin(ang[3])*sin(ang[4])*sin(ang[5])  )

    sigma_mat <- matrix(0,4,4)
    sigma_mat[3,] <- row_x
    sigma_mat[4,] <- row_y

    sigma_mat[1,1] <- 1
    sigma_mat[2,1] <- sum(sigma_mat[3,]*sigma_mat[4,])
    sigma_mat[2,2] <- sqrt( 1 - sigma_mat[2,1]^2)

    mu_vec <- rep(mu_xy, lag_order + 1)

    var_vec <- rep(var_xy, lag_order + 1)


    for( i in 1:gas_a_lag){
    S_t_new <- score_fun( xy = data[ , i:(i + lag_order) ], 
                            mu = mu_vec,
                            var = var_vec,
                            ang = ang,
                            sigma = sigma_mat,
                            h = Inv_h,
                            n = n,
                            nu = nu,
                            out = out)
    
    S_t[,i] <- S_t_new$score

    # loglikelihood after iteration
    # negative_llh <- negative_llh - S_t_new$llh

    # negative_llh1 <- negative_llh1 - S_t_new$lh1
    # negative_llh2 <- negative_llh2 - S_t_new$lh2

    # if(out == TRUE){            
    #     param_out <- list.append(param_out, list(x=S_t_new$xy_cur,
    #                                             mu_c = S_t_new$mu_c, 
    #                                             sigma_c = S_t_new$sigma_c,
    #                                             mu = S_t_new$mu,
    #                                             sigma =S_t_new$sigma,
    #                                             hdens = S_t_new$hdens))
    # }
    
    }

    # reversing columns of S_t so the first will be newest S_t
    S_t <- as.matrix(S_t[,rev(1:ncol(S_t))])

    # start of iteration 
    start_it <- (lag_order+gas_a_lag+1)

    # end of iteration
    end_it <- ncol(data)

    tv_param_out <- c()

    # log likelihood 
    negative_llh <- negative_llh1 <- negative_llh2 <- 0

    for( i in start_it:end_it){


        # new parameters
        t_param_new <- omega + rowSums(A*S_t) + rowSums(B*t_param)

        # updating tv_param and S_t
        if(gas_b_lag != 0){
            t_param[,min(gas_b_lag,2):gas_b_lag] <- t_param[,1:(max(gas_b_lag,2)-1)]
        }

        t_param[,1] <- t_param_new
       
        # update mu_vec 
        mu_vec <- mu_vec[-c(1:2)]
        mu_vec <- c(mu_vec,t_param_new[1:2])

        # update var_vec 
        var_vec <- var_vec[-c(1:2)]
        var_vec <- c(var_vec,exp(t_param_new[3:4]))

        # update angles and correlation matrix 
        ang <- pi/(1 + exp(-t_param_new[-c(1:4)]))

        # last two rows of cholesky decomposition of correlation matrix

        row_x <- c( cos(ang[1]), sin(ang[1])*cos(ang[2]), sin(ang[1])*sin(ang[2]),0)
        row_y <- c( cos(ang[3]), sin(ang[3])*cos(ang[4]), sin(ang[3])*sin(ang[4])*cos(ang[5]), sin(ang[3])*sin(ang[4])*sin(ang[5])  )

        # upadte correlation matrix
        
        sigma_mat[2,1] <- sum(sigma_mat[3,]*sigma_mat[4,])
        sigma_mat[2,2] <- sqrt( 1 - sigma_mat[2,1]^2)

        sigma_mat[3,] <- row_x
        sigma_mat[4,] <- row_y

        # new score function and negative log likelihood
        S_t_new <- score_fun( xy = data[ , (i - lag_order):i ],
                              mu = mu_vec,
                              var = var_vec,
                              ang = ang,
                              sigma = sigma_mat,
                              n = n,
                              h = Inv_h,
                              nu = nu,
                              out = out)


        if(out == TRUE && i > 62){
            param_out <- list.append(param_out, list(x=S_t_new$xy_cur,
                                                    mu_c = S_t_new$mu_c, 
                                                    sigma_c = S_t_new$sigma_c,
                                                    mu = S_t_new$mu,
                                                    sigma =S_t_new$sigma,
                                                    hdens = S_t_new$hdens))

            tv_param_out <- cbind(tv_param_out,t_param_new)

            cor_out <- cbind(cor_out, S_t_new$cor_out)
        }

        S_t[,min(gas_a_lag,2):gas_a_lag] <- S_t[,1:(max(gas_a_lag,2)-1)]        
        S_t[,1] <- S_t_new$score

        # loglikelihood after iteration
        if( i > 62){
            negative_llh <- negative_llh - S_t_new$llh
            negative_llh1 <- negative_llh1 - S_t_new$lh1
            negative_llh2 <- negative_llh2 - S_t_new$lh2
        }
    

    }

    #print(negative_llh1)

    if( out == TRUE){
        #print(negative_llh1)
        return(list( param_out= param_out, tv_param_out= tv_param_out, cor_out = cor_out))
    }else{
        return(negative_llh = negative_llh)
    }
}

