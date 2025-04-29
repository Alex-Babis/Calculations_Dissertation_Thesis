# gradient of log density of normal distribution 

grad_t_M <- function(
xy = NULL,
mu = NULL,
var = NULL,
sigma = NULL,
ang = NULL,
h = -1/2,
n = NULL,
nu = NULL,
gamma_const = NULL,
out = FALSE
){
    # PRVA CAST = VYPOCET LIKELIHOODU PRE PODMIENENE X a Y v CASE t
    x_vec <- as.vector(xy)

    # OLD VALUE OF CHOLESKY DECOMPOSITION and OLD CONDITIONAL DISTRIBUTION
    sigma_old <- sigma * sqrt(var)

    # cor matrix
    sigma_t <- tcrossprod(sigma_old)

    sigma_H <- sigma_t[1:2,1:2]

    #sigma_HN <- sigma_t[1:2,3:4]

    sigma_NH <- sigma_t[3:4, 1:2]

    sigma_N <- sigma_t[3:4,3:4]

    # inverse of correlation of H
    sigma_inv_H <- Rfast::spdinv(sigma_H)

    # vector b =  sigma_H^-1(x - mu)
    b <- as.numeric(sigma_inv_H%*%(x_vec[1:2] - mu[1:2]))

    # conditional mu 
    mu_c <- mu[3:4] + sigma_NH%*%b

    # conditional sigma
    sigma_c <- tcrossprod(sigma_old[3:4,3:4])

    # conditional constant
    cc <- (nu + (x_vec[1:2] - mu[1:2])%*%b)/(nu+2)

    cc <- as.numeric(cc)
    sigma_c[1,2] <- sigma_c[2,1]

    sigma_c <- cc * sigma_c

    # conditional degree of freedom
    nu_c <- nu + 2

    llh <- mvnfast::dmvt(X = x_vec[3:4], mu = mu_c, sigma = sigma_c, df = nu_c, log = TRUE)

    if(out){

        den_hist <- mvnfast::dmvt(X = x_vec[1:(n-2)], mu = mu[1:(n-2)], sigma = sigma_H, df = nu)

        #sigma_out <- diag(1/sqrt(diag(sigma_new)))%*%sigma_new%*%diag(1/sqrt(diag(sigma_new)))

        sigma_out <- sigma_t
    }

    # DRUHA CAST = VYPOCET NOVEHO ROZDELENIA A GENEROVANIE PRESKALOVANIE HODNOT V PREDCHADZAJUCICH CASOCH

    # TRANSFORMATION OF DISTRIBUTION BY COVARIANCE
 
    
    # NEW VALUE OF CHOLESKY DECOMPOSITION
    # sigma[2,1] <- sigma[3,]%*%sigma[4,]
    # sigma[2,2] <- sqrt( 1 - sigma[2,1]^2)

    var_new <- var
    # var_new[1:2] <- var[3:4]

    sigma <- sigma* sqrt(var_new)

    sigma_new <- tcrossprod(sigma)

    # param_old <- cond_mu_sigma_t(x = x_vec, mu =mu, sigma = sigma_t, nu = nu)

    # param_new <- cond_mu_sigma_t(x = x_vec, mu =mu, sigma = sigma_new, nu = nu)

    # x_vec[1:2] <- ginv_2(param_new$sigma,h =1/2)%*%ginv_2(param_old$sigma, h=-1/2)%*%(x_vec[1:2] - param_old$mu ) + param_new$mu

    sigma_inv_o <- Rfast::spdinv(sigma_new)
    
    s <- as.numeric(t(x_vec - mu)%*%sigma_inv_o%*%(x_vec - mu))

    mu_grad_cons <- sigma_inv_o%*%(x_vec - mu)

    mu_grad <- (nu + 4)/(nu + s)*mu_grad_cons

    sigma_grad <- -(sigma_inv_o - ((nu + 4)/(nu+s))*(mu_grad_cons%*%t(mu_grad_cons)))

    sigma_grad <- c(sigma_grad[3,1:2], sigma_grad[3,3]/2, sigma_grad[4,1:3], sigma_grad[4,4]/2)

    grad <- c(mu_grad[3:4], sigma_grad)
    
    
    const1 <- (nu + 4)/(nu + 4 + 2)

    const2 <- 1/(nu + 4 + 2)

    inf_mat <- matrix(0,9,9)

    inf_mat[1:2,1:2] <- const1*sigma_inv_o[3:4,3:4]

    inf_lower1 <- inf_lower2 <- matrix(0, 7, 7)

    inf_lower1[lower.tri(inf_lower1,diag = TRUE)] <- c(sigma_inv_o[3,3]*sigma_inv_o[1,1] + sigma_inv_o[3,1]*sigma_inv_o[1,3],
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

    #inf_lower1[upper.tri(inf_lower1)] <- t(inf_lower1)[upper.tri(inf_lower1)]
    # inf_lower1 <- 2*Matrix::symmpart(inf_lower1)
    # diag(inf_lower1) <- diag(inf_lower1)/2

    inf_lower2[lower.tri(inf_lower2,diag = TRUE)] <- c( 4*sigma_inv_o[3,1]*sigma_inv_o[3,1],
                                                    4*sigma_inv_o[3,1]*sigma_inv_o[3,2],
                                                    2*sigma_inv_o[3,1]*sigma_inv_o[3,3] ,
                                                    4*sigma_inv_o[3,1]*sigma_inv_o[4,1],
                                                    4*sigma_inv_o[3,1]*sigma_inv_o[4,2],
                                                    4*sigma_inv_o[3,1]*sigma_inv_o[4,3],
                                                    2*sigma_inv_o[3,1]*sigma_inv_o[4,4] ,
                                                    
                                                    4*sigma_inv_o[3,2]*sigma_inv_o[3,2],
                                                    2*sigma_inv_o[3,2]*sigma_inv_o[3,3],
                                                    4*sigma_inv_o[3,2]*sigma_inv_o[4,1],
                                                    4*sigma_inv_o[3,2]*sigma_inv_o[4,2],
                                                    4*sigma_inv_o[3,2]*sigma_inv_o[4,3],
                                                    2*sigma_inv_o[3,2]*sigma_inv_o[4,4],
                                                    
                                                    sigma_inv_o[3,3]*sigma_inv_o[3,3],
                                                    2*sigma_inv_o[3,3]*sigma_inv_o[4,1] ,
                                                    2*sigma_inv_o[3,3]*sigma_inv_o[4,2] ,
                                                    2*sigma_inv_o[3,3]*sigma_inv_o[4,3] ,
                                                    sigma_inv_o[3,3]*sigma_inv_o[4,4],
                                                    
                                                    4*sigma_inv_o[4,1]*sigma_inv_o[4,1],
                                                    4*sigma_inv_o[4,1]*sigma_inv_o[4,2],
                                                    4*sigma_inv_o[4,1]*sigma_inv_o[4,3],
                                                    2*sigma_inv_o[4,1]*sigma_inv_o[4,4],
                                                    
                                                    4*sigma_inv_o[4,2]*sigma_inv_o[4,2],
                                                    4*sigma_inv_o[4,2]*sigma_inv_o[4,3],
                                                    2*sigma_inv_o[4,2]*sigma_inv_o[4,4],
                                                    
                                                    4*sigma_inv_o[4,3]*sigma_inv_o[4,3],
                                                    2*sigma_inv_o[4,3]*sigma_inv_o[4,4],
                                                    
                                                    sigma_inv_o[4,4]*sigma_inv_o[4,4])

    #inf_lower2[upper.tri(inf_lower2)] <- t(inf_lower2)[upper.tri(inf_lower2)]
    # inf_lower2 <- 2*Matrix::symmpart(inf_lower2)
    # diag(inf_lower2) <- diag(inf_lower2)/2


    inf_lower2 <-  const1*inf_lower1 -  const2*inf_lower2/2
    inf_lower2[upper.tri(inf_lower2)] <- t(inf_lower2)[upper.tri(inf_lower2)]

    inf_mat[3:9,3:9] <- inf_lower2


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

    fac_chol[5,4:7] <- ang[3]*(1-ang[3]/pi)*c( -tan(ang[3]), 1/tan(ang[3]), 1/tan(ang[3]), 1/tan(ang[3]))*sigma[4,1:4]

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
    # inv_inf1 <- ginv_2(inf_new[1:2,1:2], h = h)
    # inv_inf2 <- ginv_2(inf_new[-c(1,2), -c(1,2)], h = h)

    inv_inf <- ginv_2(inf_new, h = h)

    # score with unit variance
    score_lh <- inv_inf%*%grad_new
    # score_lh <- c(inv_inf1%*%grad_new[1:2], inv_inf2%*%grad_new[-c(1,2)])


    if(out){

        return(list(score = score_lh, llh = llh, mu_c = mu_c , sigma_c = sigma_c,
                    xy_cur = x_vec[(n-1):n], hdens = den_hist, mu = mu[(n-1):n],sigma = sigma_N,cor_out = c(sigma_out[3,1:3],sigma_out[4,1:4])))

    }else{
        return(list(score = score_lh, llh = llh))
    }

}




cond_mu_sigma_t <- function(x, mu , sigma, nu ){

        sigma_new <- tcrossprod(sigma)

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

        cc <- (nu + (x[3:4] - mu[3:4])%*%b)/(nu+2)

        cc <- as.numeric(cc)

        # conditional sigma
        sigma_c <- sigma_H - sigma_HN%*%sigma_inv_N%*%sigma_NH

        # making it symmetrix (2x2 matrix)
        sigma_c[1,2] <- sigma_c[2,1]

        sigma_c <- cc * sigma_c

        return(list(mu = mu_c, sigma = sigma_c))
}


