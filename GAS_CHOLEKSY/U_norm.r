# gradient of log density of normal distribution 

grad_norm_U <- function(
x = NULL,
mu = NULL,
var = NULL,
ang = NULL,
sigma = NULL,
h = NULL,
n = NULL,
nu = NULL,
out = FALSE
){
    # common vector of data 
    x_vec  <- x
    
    #LIKELIHOOD EVALUATION
    # OLD VALUE OF CHOLESKY DECOMPOSITION and OLD CONDITIONAL DISTRIBUTION
    sigma_old <- sigma * sqrt(var)

    sigma_t <- tcrossprod(sigma_old)

    sigma_H <- sigma_t[1,1]

    sigma_NH <- sigma_t[2,1]

    sigma_N <- sigma_t[2,2]

    # inverse of correlation of H
    #sigma_inv_H <-  Rfast::spdinv(sigma_H)

    # vector b =  sigma_H^-1(x - mu)
    #b <- as.numeric( sigma_inv_H%*%(x_vec[1:(n-1)] - mu[1:(n-1)]))
    b <- as.numeric( (x_vec[1] - mu[1])/sigma_H )
    # conditional mu 
    mu_c <- mu[2] + sigma_NH*b

    # conditional sigma
    # sigma_c <- sigma_N - sigma_NH^2/sigma_H
    sigma_c <- sigma_old[2,2]^2
    
    # llh <- mvnfast::dmvn(X = x_vec, mu = mu, sigma = sigma_t, log = TRUE)
    llh <- mvnfast::dmvn(X = x_vec[2], mu = mu_c , sigma = sigma_c, log = TRUE)

    if(out){
        
        sigma_new <- tcrossprod(sigma)
        out_list <- list(llh = llh, mu_c = mu_c , sigma_c = sigma_c, x_cur = x_vec[n],
                    hdens = 0, mu = mu[n], sigma  = sigma_N, cov = sigma_NH, cor_out = c(sigma_new[2,1:2]))
    }

    #llh <- mvnfast::dmvn(X = x_vec, mu = mu , sigma = sigma_old, log = TRUE, isChol = TRUE)
    # DRUHA CAST = VYPOCET NOVEHO ROZDELENIA A GENEROVANIE PRESKALOVANIE HODNOT V PREDCHADZAJUCICH CASOCH

    # TRANSFORMATION OF DISTRIBUTION BY COVARIANCE


    # NEW VALUE OF CHOLESKY DECOMPOSITION
    var_new <- var
    #var_new[1] <- var_new[2]

    sigma <- sigma * sqrt(var_new)

    #param_old <- cond_mu_sigma_uni(x = x_vec, mu =mu, sigma = sigma_old)

    #param_new <- cond_mu_sigma_uni(x = x_vec, mu =mu, sigma = sigma)

    #x_vec[1] <- param_new$sigma^(1/2)*param_old$sigma^(-1/2)*(x_vec[1] - param_old$mu ) + param_new$mu

    # correlation matrix 
    sigma_new <- tcrossprod(sigma)

    sigma_inv_o <-  Rfast::spdinv(sigma_new)
  
    mu_grad <- as.numeric(sigma_inv_o%*%(x_vec - mu))

    sigma_grad <-  -(sigma_inv_o - diag(diag(sigma_inv_o))/2 - mu_grad%*%t(mu_grad) + diag(diag( mu_grad%*%t(mu_grad)))/2 )

    grad <- c(mu_grad[2], sigma_grad[2,1:2])

    inf_mat <- matrix(c(sigma_inv_o[2,2], 0, 0, 0, sigma_inv_o[1,1]*sigma_inv_o[2,2] + sigma_inv_o[1,2]^2,
                    sigma_inv_o[1,2]*sigma_inv_o[2,2] , 0 , sigma_inv_o[1,2]*sigma_inv_o[2,2],
                    sigma_inv_o[2,2]^2/2), 3, 3)

    
    # factors jacobian    
    fac_jacob <- t(sigma)

    fac_jacob[,2] <- fac_jacob[,2]*2

    fac_jacob <- rbind(0,cbind(0,fac_jacob))

    fac_jacob[1,1] <- 1

    fac_chol <- matrix(0, ncol =3, nrow =3)

    fac_chol[1,1] <- 1

    fac_chol[2,2:3] <- sigma[2,1:2]/2

    fac_chol[3,2:3] <- ang*(1-ang/(pi))*c(-tan(ang), 1/tan(ang))* sigma[2,]

    fac_der <- fac_chol%*%fac_jacob
    # new grad
    grad_new <- as.vector(fac_der%*%grad)

    # new inf mat
    inf_new <- as.matrix(fac_der%*%inf_mat%*%t(fac_der))

    # inverse 
    inv_inf <- ginv_2(inf_new, h = h)

    # score with unit variance
    score_lh <- inv_inf%*%grad_new


    if(out){

        out_list <- list.append(out_list, score = score_lh)
        return(out_list)    
     }else{
        return(list(score = score_lh, llh = llh))

    }
}



cond_mu_sigma_uni <- function(x, mu , sigma){

        sigma_new <- tcrossprod(sigma)

        sigma_H <- sigma_new[1,1]

        sigma_HN <- sigma_new[1,2]

        sigma_N <- sigma_new[2,2]

        # inverse of correlation of H

        # vector b =  sigma_H^-1(x - mu)
        b <- (x[2] - mu[2])/sigma_N

        # conditional mu 
        mu_c <- mu[1] + sigma_HN*b

        # conditional sigma
        sigma_c <- sigma_H - sigma_HN^2/sigma_N

        return(list(mu = mu_c, sigma = sigma_c))
}

