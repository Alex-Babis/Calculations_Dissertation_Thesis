# gradient of log density of normal distribution 

grad_norm_M <- function(
xy = NULL,
mu = NULL,
var = NULL,
sigma = NULL,
ang = NULL,
h = NULL,
n = NULL,
nu = NULL,
out = FALSE
){
    # PRVA CAST = VYPOCET LIKELIHOODU PRE PODMIENENE X a Y v CASE t
    x_vec <- as.vector(xy)
    
    # OLD VALUE OF CHOLESKY DECOMPOSITION and OLD CONDITIONAL DISTRIBUTION
    sigma_old <- sigma * sqrt(var)

    sigma_t <- tcrossprod(sigma_old)

    sigma_H <- sigma_t[1:2,1:2]

    # sigma_HN <- sigma_t[1:2,3:4]

    sigma_NH <- sigma_t[3:4, 1:2]

    sigma_N <- sigma_t[3:4,3:4]

    # inverse of correlation of H
    sigma_inv_H <- Rfast::spdinv(sigma_H)

    # vector b =  sigma_H^-1(x - mu)
    b <- as.numeric(sigma_inv_H%*%(x_vec[1:2] - mu[1:2]))

    # conditional mu 
    mu_c <- mu[3:4] + sigma_NH%*%b

    # conditional sigma is only lower part of cholesky
    # sigma_c <- sigma_N - sigma_NH%*%sigma_inv_H%*%sigma_HN

    # sigma_c[1,2] <- sigma_c[2,1] 

    sigma_c <- tcrossprod(sigma_old[3:4,3:4])

    # LIKELIHOOD
    #  llh <- mvnfast::dmvn(X = x_vec, mu = mu, sigma = sigma_old, log = TRUE, isChol = TRUE)
    llh <- mvnfast::dmvn(X = x_vec[3:4], mu = mu_c, sigma = sigma_c, log = TRUE)

    # lh1 <- dnorm(x_vec[3], mean = mu_c[1], sd = sqrt(sigma_c[1,1]), log = TRUE)

    # lh2 <- dnorm(x_vec[4], mean = mu_c[2], sd = sqrt(sigma_c[2,2]), log = TRUE)

    # llh <- lh1 + lh2
    
    if(out){

      sigma_new <- tcrossprod(sigma)
      out_list <- list(llh = llh, mu_c = mu_c , sigma_c = sigma_c, lh1 = 0, lh2 = 0,
                    xy_cur = x_vec[3:4], hdens = 0, mu = mu[3:4],sigma = sigma_N,cor_out = c(sigma_new[3,1:3],sigma_new[4,1:4]))
    }

    #llh <-  mvnfast::dmvn(X = x_vec[(n-1):n], mu = mu_c, sigma = sigma_c, log = TRUE)

    # DRUHA CAST = VYPOCET NOVEHO ROZDELENIA A GENEROVANIE PRESKALOVANIE HODNOT V PREDCHADZAJUCICH CASOCH

    # TRANSFORMATION OF DISTRIBUTION BY COVARIANCE

    
    # NEW VALUE OF CHOLESKY DECOMPOSITION
    # sigma[2,1] <- sigma[3,]%*%sigma[4,]
    # sigma[2,2] <- sqrt( 1 - sigma[2,1]^2)

    var_new <- var
    # var_new[1:2] <- var[3:4]

    sigma <- sigma * sqrt(var_new)

    sigma_new <- tcrossprod(sigma)

    # param_old <- cond_mu_sigma(x = x_vec, mu =mu, sigma_new = sigma_t)

    # param_new <- cond_mu_sigma(x = x_vec, mu =mu, sigma_new = sigma_new)

    # x_vec[1:2] <- ginv_2(param_new$sigma,h =1/2)%*%ginv_2(param_old$sigma, h=-1/2)%*%(x_vec[1:2] - param_old$mu ) + param_new$mu


    sigma_inv_o <- Rfast::spdinv(sigma_new)
    
    mu_grad <- as.numeric(sigma_inv_o%*%(x_vec - mu))

    sigma_grad <-  -(sigma_inv_o  - mu_grad%*%t(mu_grad) )

    sigma_grad <- c(sigma_grad[3,1:2],sigma_grad[3,3]/2,sigma_grad[4,1:3],sigma_grad[4,4]/2)

    grad <- c(mu_grad[3:4], sigma_grad)


    inf_mat <- matrix(0,9,9)

    inf_mat[1:2,1:2] <- sigma_inv_o[3:4,3:4]

    inf_lower <- matrix(0, 7,7)
    inf_lower[lower.tri(inf_lower,diag = TRUE)] <- c(sigma_inv_o[3,3]*sigma_inv_o[1,1] + sigma_inv_o[3,1]*sigma_inv_o[1,3],
                                                     sigma_inv_o[3,3]*sigma_inv_o[1,2] + sigma_inv_o[3,2]*sigma_inv_o[1,3],
                                                     sigma_inv_o[3,3]*sigma_inv_o[1,3] ,
                                                     sigma_inv_o[3,4]*sigma_inv_o[1,1] + sigma_inv_o[3,1]*sigma_inv_o[1,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[1,2] + sigma_inv_o[3,2]*sigma_inv_o[1,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[1,3] + sigma_inv_o[3,3]*sigma_inv_o[1,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[1,4] ,
                                                     
                                                     sigma_inv_o[3,3]*sigma_inv_o[2,2] + sigma_inv_o[3,2]*sigma_inv_o[2,3],
                                                     sigma_inv_o[3,3]*sigma_inv_o[2,3] ,
                                                     sigma_inv_o[3,4]*sigma_inv_o[2,1] + sigma_inv_o[3,1]*sigma_inv_o[2,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[2,2] + sigma_inv_o[3,2]*sigma_inv_o[2,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[2,3] + sigma_inv_o[3,3]*sigma_inv_o[2,4],
                                                     sigma_inv_o[3,4]*sigma_inv_o[2,4] ,
                                                     
                                                     sigma_inv_o[3,3]*sigma_inv_o[3,3]/2,
                                                     sigma_inv_o[3,4]*sigma_inv_o[3,1] ,
                                                     sigma_inv_o[3,4]*sigma_inv_o[3,2] ,
                                                     sigma_inv_o[3,4]*sigma_inv_o[3,3] ,
                                                     sigma_inv_o[3,4]*sigma_inv_o[3,4]/2,
                                                     
                                                     sigma_inv_o[4,4]*sigma_inv_o[1,1] + sigma_inv_o[4,1]*sigma_inv_o[1,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[1,2] + sigma_inv_o[4,2]*sigma_inv_o[1,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[1,3] + sigma_inv_o[4,3]*sigma_inv_o[1,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[1,4],
                                                     
                                                     sigma_inv_o[4,4]*sigma_inv_o[2,2] + sigma_inv_o[4,2]*sigma_inv_o[2,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[2,3] + sigma_inv_o[4,3]*sigma_inv_o[2,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[2,4] ,
                                                     
                                                     sigma_inv_o[4,4]*sigma_inv_o[3,3] + sigma_inv_o[4,3]*sigma_inv_o[3,4],
                                                     sigma_inv_o[4,4]*sigma_inv_o[3,4],
                                                     
                                                     sigma_inv_o[4,4]*sigma_inv_o[4,4]/2)

    inf_lower[upper.tri(inf_lower)] <- t(inf_lower)[upper.tri(inf_lower)]

    inf_mat[3:9,3:9] <- inf_lower  


    # factors jacobian    
    fac_jacob <- matrix(rep(0,(2*n - 1)^2), ncol = 2*n - 1, nrow = 2*n - 1)

    fac_jacob[1:3,1:3] <- t(sigma[1:3,1:3])

    fac_jacob[4:7,4:7] <- t(sigma[1:4,1:4])

    fac_jacob[1:3,6] <- sigma[4,1:3]
    
    fac_jacob[,3] <- 2*fac_jacob[,3]

    fac_jacob[,7] <- 2*fac_jacob[,7]

    # derivatives of cholesky matrix by factors

    fac_chol <- matrix(0, ncol =7, nrow =7)

    fac_chol[1,1:3] <- sigma[3,1:3]/2

    fac_chol[2,4:7] <- sigma[4,1:4]/2

    fac_chol[3,1:3] <- ang[1]*(1-ang[1]/pi)*c(-tan(ang[1]), 1/tan(ang[1]), 1/tan(ang[1]) )*sigma[3,1:3]

    fac_chol[4,1:3] <- ang[2]*(1-ang[2]/pi)*c( 0, -tan(ang[2]), 1/tan(ang[2]) )*sigma[3,1:3]

    fac_chol[5,4:7] <- ang[3]*(1-ang[3]/pi)*c( -tan(ang[3]), 1/tan(ang[3]), 1/tan(ang[3]), 1/tan(ang[3]) )*sigma[4,1:4]

    fac_chol[6,4:7] <- ang[4]*(1-ang[4]/pi)*c(0, -tan(ang[4]), 1/tan(ang[4]), 1/tan(ang[4]) )*sigma[4,1:4]

    fac_chol[7,4:7] <- ang[5]*(1-ang[5]/pi)*c(0, 0, -tan(ang[5]), 1/tan(ang[5]) )*sigma[4,1:4]

    # final jacobian of factors
    fac_jacob2 <- matrix(0,2*n +1,2*n +1)

    fac_jacob2[rbind(c(1,1),c(2,2))] <- 1

    fac_jacob2[3:(2*n +1),3:(2*n +1)] <- fac_chol%*%fac_jacob

    fac_jacob2[5:6,5] <- fac_jacob2[7:9,9] <- 0

    fac_der <- fac_jacob2
    
    grad_new <- fac_der%*%grad

    inf_new <- fac_der%*%inf_mat%*%t(fac_der)
   
    # inverse 
    inv_inf1 <- ginv_2(inf_new[1:2,1:2], h = h)

    inv_inf2 <- ginv_2(inf_new[-c(1,2), -c(1,2)], h = h)

    inv_inf <- 0*inf_new

    inv_inf[1:2,1:2] <-inv_inf1

    inv_inf[-c(1,2), -c(1,2)] <- inv_inf2

    # inv_inf <- ginv_2(inf_new, h = h)

    # score with unit variance
    score_lh <- inv_inf%*%grad_new
    #score_lh <- c(inv_inf1%*%grad_new[1:2], inv_inf2%*%grad_new[-c(1,2)])


    if(out){

        out_list <- list.append(out_list, score = score_lh)
        return(out_list)  

    }else{
        return(list(score = score_lh, llh = llh, lh1= 0, lh2 = 0))
    }

}


cond_mu_sigma <- function(x, mu , sigma_new){

        sigma_H <- sigma_new[1:2,1:2]

        sigma_HN <- sigma_new[1:2,3:4]

        sigma_NH <- sigma_new[3:4, 1:2]

        sigma_N <- sigma_new[3:4,3:4]

        # inverse of correlation of H
        sigma_inv_N <- Rfast::spdinv(sigma_N)

        # vector b =  sigma_H^-1(x - mu)
        b <- as.numeric(sigma_inv_N%*%(x[3:4] - mu[3:4]))

        # conditional mu 
        mu_c <- mu[1:2] + sigma_HN%*%b

        # conditional sigma
        sigma_c <- sigma_H - sigma_HN%*%sigma_inv_N%*%sigma_NH

        # making it symmetrix (2x2 matrix)
        sigma_c[1,2] <- sigma_c[2,1]

        return(list(mu = mu_c, sigma = sigma_c))
}





