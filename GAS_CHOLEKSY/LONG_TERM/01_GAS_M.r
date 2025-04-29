# GAS with conditional MVN

gas_M_long <- function( static_param = NULL,
                        data = NULL,
                        lag_order = 1,
                        dist = NULL
                       ){

    

    # creating vectors of start values
    # mean value 
    mu_xy <- static_param[1:2]

    # variances 
    var_xy <- exp(static_param[3:4])
    # fac mat

    # angles
    ang <- pi/(1+ exp(-static_param[-c(1:4)]))

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

    sigma <- sigma_mat * sqrt(var_vec)

    # sigma_t <- tcrossprod(sigma)

    # sigma_H <- sigma_t[1:2,1:2]

    # sigma_HN <- sigma_t[1:2,3:4]

    # sigma_NH <- sigma_t[3:4, 1:2]

    # sigma_N <- sigma_t[3:4,3:4]

    # # inverse of correlation of H
    # sigma_inv_H <- Rfast::spdinv(sigma_H)

    # # conditional sigma is only lower part of cholesky
    # sigma_c <- sigma_N - sigma_NH%*%sigma_inv_H%*%sigma_HN

    # sigma_c[1,2] <- sigma_c[2,1]    

    # start of iteration 
    start_it <- (lag_order+1)

    # end of iteration
    end_it <- ncol(data)

    # log likelihood 
    negative_llh <- 0

    for( i in start_it:end_it){

      x_vec <- as.vector(data[ , (i - lag_order):i ])

      # # vector b =  sigma_H^-1(x - mu)
      # b <- as.numeric(sigma_inv_H%*%(x_vec[1:2] - mu_vec[1:2]))

      # # conditional mu 
      # mu_c <- mu_vec[3:4] + sigma_NH%*%b

      # # LIKELIHOOD
      # llh <- mvnfast::dmvn(X = x_vec[3:4], mu = mu_c, sigma = sigma_c, log = TRUE)

      llh <- mvnfast::dmvn(X = x_vec, mu = mu_vec, sigma = sigma, log = TRUE, isChol = TRUE)

      # loglikelihood after iteration
      negative_llh <- negative_llh - llh

    }

    #print(negative_llh)


    return(negative_llh)

}



static_param = LT_fina2y$optim$bestmem
data = rbind(returns[,1],returns[,8])
lag_order = 1
dist = "norm"


