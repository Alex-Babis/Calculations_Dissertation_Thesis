# STARTING PARAMETERS FOR LAMBDA AND COV MODELS 

# STARTING LAMBDA WILL BE 
if(!all(tv_par)){
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

    }