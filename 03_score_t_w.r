score_t <- function(
x = NULL,
sigma_inv = NULL,
sigma = NULL,
pars = NULL,
h = NULL,
tau = NULL,
tv_par = NULL,
nu = NULL
){

beta <- pars[1:3]

lambda <- exp(pars[4])

lt1 <- -lambda*tau

lt1[4] <- -exp(log(lambda) + log(3))

exp_l <- exp(lt1)

const <- expm1(lt1)/(lt1)

fac <- rbind(rep(1, length(tau)), 
                const,  
                const - exp_l)
                
mu <- t(fac)%*%beta

s <- as.numeric(t(x - mu)%*%sigma_inv%*%(x - mu))

# grad of mean value
grad_mu <- (nu + length(tau))/(nu + s)* sigma_inv%*%(x - mu)

inf_mat_mu <- (nu + length(tau))/(nu + 2 + length(tau))*sigma_inv

# gradient with respect to factors
grad <- fac%*%grad_mu

# information matrix with respect to factors
inf_mat <- fac%*%inf_mat_mu%*%t(fac)

inv_inf <- ginv_2(inf_mat, h = h)
#inv_inf <- solve(inf_mat)

score <- inv_inf%*%grad

if(tv_par[4] == TRUE){

lambda_grad <- (beta[2]+beta[3])*(exp_l - const) - beta[3]*exp_l*lt1

score_lambda <- lambda_grad%*%grad_mu*(lambda_grad%*%inf_mat_mu%*%lambda_grad)^h

score <- c(score, score_lambda)
}

#llh <- dmvt(x = x, delta = mu, sigma = sigma, df = ni, log = TRUE)
llh <- mvnfast::dmvt(X = x, mu = mu, sigma = diag(sqrt(diag(sigma))), df = nu, log = TRUE, isChol = TRUE)

return(list(score =score, llh = llh))

}
