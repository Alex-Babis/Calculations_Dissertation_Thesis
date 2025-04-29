score_t <- function(
x = NULL,
sigma = NULL,
pars = NULL,
h = NULL,
tau = NULL,
tv_par = NULL,
nu = NULL,
delta_L = NULL,
type = NULL,
dcov= NULL,
out = NULL
){


# without dynamic volatility
if(!dcov){
    sigma_inv <- diag(1/diag(sigma)) 
}else {
   tv_sigma <-  exp(pars[length(pars)])
   diag(sigma) <- diag(sigma) + tv_sigma
   sigma_inv <- diag(1/diag(sigma )) 
   names(tv_sigma) <- "tv_sigma"
}

if(type == "NS"){

    beta <- pars[1:3]

    lambda1 <- exp(pars[4])

    lt1 <- -lambda1*tau

    lt1[4] <- -exp(log(lambda1) + log(3))

    exp_l1 <- exp(lt1)

    const1 <- expm1(lt1)/(lt1)

    fac <- rbind(rep(1, length(tau)), 
                    const1,  
                    const1 - exp_l1)
    
    names(beta) <- c("beta1","beta2","beta3")

}else if (type == "NSS") {
    # Coeficient beta
    beta <- pars[1:4]

    lambda1 <- exp(pars[5]) + exp(pars[6])  + delta_L

    lambda2 <- exp(pars[6]) 

    lt1 <- -lambda1*tau

    lt1[4] <- -exp(log(lambda1) + log(3))

    lt2 <- -lambda2*tau

    lt2[4] <- -exp(log(lambda2) + log(3))

    exp_l1 <- exp(lt1)

    const1 <- expm1(lt1)/(lt1)

    exp_l2 <- exp(lt2)

    const2 <- expm1(lt2)/(lt2)

    fac <- matrix(1, nrow=4, ncol = length(tau))

    fac[2,] <- const1 
    fac[3,] <- const1 - exp_l1
    fac[4,] <- const2 - exp_l2
    names(beta) <- c("beta1","beta2","beta3","beta4")

}else {
    # Coeficient beta
    beta <- pars[1:3]

    # Bliss is without further constraint
    lambda1 <- exp(pars[4]) 

    lambda2 <- exp(pars[5]) 

    lt1 <- -lambda1*tau

    lt1[4] <- -exp(log(lambda1) + log(3))

    lt2 <- -lambda2*tau

    lt2[4] <- -exp(log(lambda2) + log(3))

    exp_l1 <- exp(lt1)

    const1 <- expm1(lt1)/(lt1)

    exp_l2 <- exp(lt2)

    const2 <- expm1(lt2)/(lt2)

    fac <- matrix(1, nrow=3, ncol = length(tau))

    fac[2,] <- const1 
    fac[3,] <- const2 - exp_l2

    names(beta) <- c("beta1","beta2","beta3")
}

# SCORE OF BETAS

#print(c(lambda1,lambda2))
mu <- as.vector(beta%*%fac)

# grad of mean value

# value s for gradient and inf mat
s <- as.numeric(t(x - mu)%*%sigma_inv%*%(x - mu))

# grad of mean value
grad_mu <- (nu + length(tau))/(nu + s)* sigma_inv%*%(x - mu)

inf_mat_mu <- (nu + length(tau))/(nu + 2 + length(tau))*sigma_inv

# gradient with respect to factors
grad <- fac%*%grad_mu

# information matrix with respect to factors
inf_mat <- fac%*%inf_mat_mu%*%t(fac)

inv_inf <- ginv_2(inf_mat, h = h)

score <- inv_inf%*%grad

# SCORE OF LAMBDAS
if(type == "NS"){
    if(tv_par[4]){

        lambda_grad <- (beta[2] + beta[3])*(exp_l1 - const1)/lambda1 + beta[3]*tau*exp_l1

        lambda_grad <- lambda_grad*exp(pars[4])

        score_lambda <- lambda_grad%*%grad_mu*(lambda_grad%*%inf_mat_mu%*%lambda_grad)^h

        score <- c(score, score_lambda)

        lambda_tv <- lambda1
        names(lambda_tv) <- "lambda1"
    }
}else if (type == "NSS") {

    if(tv_par[5] && tv_par[6]){

        lambda1_grad <- (beta[2] + beta[3])*(exp_l1 - const1)/lambda1 + beta[3]*tau*exp_l1

        lambda2_grad <- beta[4]*(exp_l2 - const2) + beta[4]*tau*exp_l2*lambda2

        grad1 <- lambda1_grad*exp(pars[5])

        grad2 <- (lambda1_grad*exp(pars[6]) + lambda2_grad)

        lambda_grad <- matrix(c(grad1,grad2), nrow = 2, ncol = length(tau), byrow = TRUE)

        inf_mat <- lambda_grad%*%inf_mat_mu%*%t(lambda_grad)

        inf_lambda <- ginv_2(inf_mat, h = h)

        score_lambda <- inf_lambda%*%lambda_grad%*%grad_mu

        score <- c(score, score_lambda)

        lambda_tv <- c(lambda1, lambda2)
        names(lambda_tv) <- c("lambda1", "lambda2")

    }
}else{
    if(tv_par[4] && tv_par[5]){

        lambda1_grad <- beta[2]*(exp_l1 - const1)

        lambda2_grad <- beta[3]*(exp_l2 - const2) + beta[3]*tau*exp_l2*lambda2

        grad1 <- lambda1_grad

        grad2 <- lambda2_grad

        lambda_grad <- matrix(c(grad1,grad2), nrow = 2, ncol = length(tau), byrow = TRUE)

        inf_mat <- lambda_grad%*%inf_mat_mu%*%t(lambda_grad)

        inf_lambda <- ginv_2(inf_mat, h = h)

        score_lambda <- inf_lambda%*%lambda_grad%*%grad_mu

        score <- c(score, score_lambda)

        lambda_tv <- c(lambda1, lambda2)
        names(lambda_tv) <- c("lambda1", "lambda2")

    }
}

out_pars <- beta

if(exists("lambda_tv")){
    out_pars <- c(out_pars, lambda_tv)
}

if(exists("tv_sigma")){
    out_pars <- c(out_pars, tv_sigma)
}

if(dcov){
    # SIGMA SCORE AND INFORMATION MATRIX
    sigma_grad <- -(1/diag(sigma) - (nu + length(tau))/(nu + s)*(x-mu)^2/(diag(sigma^2)))/2

    sigma_inf <- (nu + length(tau) - 1)/(2*(nu + length(tau) + 2))*sigma_inv^2

    sigma_fac <- rep( exp(pars[length(pars)]), length(tau) )

    sigma_score <- sigma_fac%*%sigma_grad*(sigma_fac%*%sigma_inf%*%sigma_fac)^h

    score <- c(score, sigma_score)
}


#print(score)
llh <- mvnfast::dmvt(X = x, mu = mu, sigma = diag(sqrt(diag(sigma))), df = nu, log = TRUE, isChol = TRUE)


if(!out){
    return(list(score =score, llh = llh, mu = mu, out_pars = out_pars))
}else{

    llh_yields <- log(dt((x - mu)/sqrt(diag(sigma)), df = nu)) - log(sqrt(diag(sigma)))
    llh_yields <- c(llh_yields, llh)
    
    return(list(score =score, llh = llh, mu = mu, out_pars = out_pars, llh_yields = llh_yields))

}

}

