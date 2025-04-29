
# x is always yield rate
# y is always index


Entropy_fun_t <- function(Obj_x =NULL, Obj_y = NULL, Obj_xy = NULL, alpha, wind){

        # Whole entropy
        entr_x <- numeric(length(alpha))
        entr_y <- numeric(length(alpha))

        # p-value of weighted t-test
        test_x <- numeric(length(alpha))
        test_y <- numeric(length(alpha))

        # # probability of history of x normed to 1
        # w_x <- Obj_x$dens/ sum(Obj_x$dens)
        # # probability of history of y normed to 1
        # w_y <- Obj_y$dens/ sum(Obj_y$dens)
        # # probability of both histories x and y normed to 1
        # w_xy <- Obj_xy$xy_dens/ sum(Obj_xy$xy_dens)

        w_x <- w_y <- w_xy <- rep(1,length(Obj_x$dens))
        
        ts_entr_x <- c()
        ts_entr_y <- c()

        nu_x = nu_y = nu_xy = 10


        for( i in 1:length(alpha)){
                print(i)
                # plug-in-constant for entropy
 
                const_x <- gamma((nu_x + 1)/2)/gamma(nu_x/2)*( beta((nu_x*alpha[i] + alpha[i] - 1)/2,1/2)/gamma(1/2) )^(1/alpha[i]) * nu_x^((1-alpha[i])/(2*alpha[i]))

                const_y <- gamma((nu_y + 1)/2)/gamma(nu_y/2)*( beta((nu_y*alpha[i] + alpha[i] - 1)/2,1/2)/gamma(1/2) )^(1/alpha[i]) * nu_y^((1-alpha[i])/(2*alpha[i]))

                const_xy <- gamma((nu_xy + 1)/2)/gamma(nu_xy/2)*( beta((nu_xy*alpha[i] + alpha[i] - 1)/2,1/2)/gamma(1/2) )^(1/alpha[i]) * nu_xy^((1-alpha[i])/(2*alpha[i]))

                x_val <- entropy_t(x = Obj_x$sigma_c, 
                                        y = Obj_xy$x_sigma_c, 
                                        w_x = w_x, 
                                        w_y = w_xy,
                                        nu_x = nu_x,
                                        nu_y = nu_xy, 
                                        const_x = const_x,
                                        const_y = const_xy,
                                        alpha = alpha[i],
                                        test = TRUE)

    
                entr_x[i] <- x_val$entr
                test_x[i] <- x_val$test

                y_val <- entropy_t(x = Obj_y$sigma_c,
                                        y = Obj_xy$y_sigma_c, 
                                        w_x = w_y, 
                                        w_y = w_xy,
                                        nu_x = nu_y,
                                        nu_y = nu_xy, 
                                        const_x = const_y,
                                        const_y = const_xy,
                                        alpha = alpha[i],
                                        test = TRUE)


                entr_y[i] <- y_val$entr
                test_y[i] <- y_val$test



                # time varying entropy
                entr_x_wind <- numeric(wind)
                entr_y_wind <- numeric(wind)

                for( j in 1:(length(w_x) - wind)){
                
                # # probability of history of x normed to 1
                # w_x_wind <- Obj_x$dens[j:(j+wind)] / sum(Obj_x$dens[j:(j+wind)])
                # # probability of history of y normed to 1
                # w_y_wind  <- Obj_y$dens[j:(j+wind)] / sum(Obj_y$dens[j:(j+wind)])
                # # probability of both histories x and y normed to 1
                # w_xy_wind  <- Obj_xy$xy_dens[j:(j+wind)] / sum(Obj_xy$xy_dens[j:(j+wind)])
                
                w_x_wind <- w_y_wind <- w_xy_wind <- rep(1, wind)


                entr_x_wind[j] <- entropy_t(x = Obj_x$sigma_c[j:(j+wind-1)], 
                                          y = Obj_xy$x_sigma_c[j:(j+wind-1)], 
                                          w_x = w_x_wind, 
                                          w_y = w_xy_wind, 
                                          nu_x = nu_x,
                                          nu_y = nu_xy, 
                                          const_x = const_x,
                                          const_y = const_xy,
                                          alpha = alpha[i],
                                          test = FALSE)$entr

                entr_y_wind[j] <- entropy_t(x = Obj_y$sigma_c[j:(j+wind-1)],
                                          y = Obj_xy$y_sigma_c[j:(j+wind-1)], 
                                          w_x = w_y_wind, 
                                          w_y = w_xy_wind,  
                                          nu_x = nu_y,
                                          nu_y = nu_xy, 
                                          const_x = const_y,
                                          const_y = const_xy, 
                                          alpha = alpha[i],
                                          test = FALSE)$entr
                }

                ts_entr_x <- rbind(ts_entr_x, entr_x_wind)
                ts_entr_y <- rbind(ts_entr_y, entr_y_wind)

        }
        # test_x = test_x, test_y = test_y,
        return(list(entr_x = entr_x, entr_y = entr_y, test_x = test_x, test_y = test_y, ts_entr_x = ts_entr_x, ts_entr_y = ts_entr_y))

}

entropy_t <- function(x, y, w_x, w_y, nu_x, nu_y, const_x, const_y, alpha, test = TRUE){

        # w_x <- history of itself only
        # w_y <- history of both time series

        if(alpha ==1){
           entr <-  (w_x%*%log(x)/2 - w_y%*%log(y)/2)/length(x)
           test <- t.test(x = log(x)/2, y = log(y)/2,  alternative = "two.sided")$p.value
        }else{
          entr <- alpha/(1-alpha) * (log( w_x%*%(const_x*x^( (1-alpha)/(2*alpha)) )) - log( w_y%*%(const_y*y^( (1-alpha)/(2*alpha)) )) )
          if(alpha < 1){
            test_orinet = "two.sided"
          }else{
            test_orinet = "two.sided"
          }
          test <- t.test(x = x^( (1-alpha)/(2*alpha)), y = y^( (1-alpha)/(2*alpha) ), alternative = test_orinet)$p.value

        }

        return(list(entr= entr, test= test))
}

# bootstrap_sample <- function(samp_x, w_x, samp_y, w_y, N = 1000, const_x, const_y, alpha ){
    
#     entr <- alpha/(1-alpha) * (log( const_x*w_x%*%samp_x^( (1-alpha)/(2*alpha) ), base = 2) - log( const_y*w_y%*%samp_y^( (1-alpha)/(2*alpha) ), base = 2) )
    
#     bt_sample_x <- c()
#     bt_sample_y <- c()

#     for( i in 1:N){
   
#         # random sample 1
#         rand_x <- sample(1:length(w_x), size = length(w_x), replace = TRUE, prob = w_x)
        
#         x <- samp_x[rand_x]
#         new_w_x <- w_x[rand_x]

#         new_w_x <- new_w_x/sum(new_w_x)

#         bt_sample_x <- c(bt_sample_x, const_x*new_w_x%*%x^( (1-alpha[1])/(2*alpha[1]) ))

#         # random sample 2
#         rand_y <- sample(1:length(w_y), size = length(w_y), replace = TRUE, prob = w_y)

#         y <- samp_y[rand_y]
#         new_w_y <- w_y[rand_y]
#         new_w_y <- new_w_y/sum(new_w_y)

#         bt_sample_y <- c(bt_sample_y, const_y*new_w_y%*%y^( (1-alpha[1])/(2*alpha[1])) ) 
#     }
    
#     mat_x <- t(matrix(bt_sample_x, N,N))

#     # vector of test statistics
#     test_stat <- as.vector(mat_x/bt_sample_y) 

 
#     # confidence levels
#     ci <- quantile(test_stat,prob = c(0.05,0.95))

#     ci <-  alpha[1]/(1-alpha[1]) * log(ci, base = 2)
    
#     return(list(entr = entr, ci = ci))

# }


ret_func <- function(GAS_obj, UNI = TRUE, nu= NULL){

    if(UNI){
        
    val <- c()
    mu <- c()
    mu_c <- c()
    sigma <- c()
    sigma_c <- c()
    dens <- c()
    cov <- c()

    for( i in 1:length(GAS_obj)){
        val <- c(val,GAS_obj[[i]]$x)
        mu <- c(mu,GAS_obj[[i]]$mu)
        mu_c <- c(mu_c,GAS_obj[[i]]$mu_c)
        sigma <- c(sigma,GAS_obj[[i]]$sigma)
        sigma_c <- c(sigma_c,GAS_obj[[i]]$sigma_c)
        dens <- c(dens,GAS_obj[[i]]$hdens)
        cov <- c(cov,GAS_obj[[i]]$cov) 
    }

    df <- data.frame(
        value = val,
        mu = mu,
        mu_c = mu_c,
        sigma = sigma,
        sigma_c = sigma_c,
        dens = dens
    )

    # df_long <- data.frame(x = rep(1:length(GAS_obj),5),
    #                     value = c(val,mu, mu_c,sqrt(sigma),sqrt(sigma_c)),
    #                     ts = c(rep("value",length(GAS_obj)),
    #                         rep("mean",length(GAS_obj)),
    #                         rep("mean_c",length(GAS_obj)),
    #                         rep("sigma",length(GAS_obj)),
    #                         rep("sigma_c",length(GAS_obj))))
                            
    # df_long$ts <- factor(df_long$ts, levels = c("value","mean","mean_c",
    #                                         "sigma","sigma_c"))


    print("X- LH")
    x_lh <- -sum(log(dt(x = (df$val - df$mu_c)/sqrt(df$sigma_c) , df = nu)/sqrt(df$sigma_c)))
    print(x_lh)

    #p1<-ggplot(data= df_long, aes( x = x, y = value, group = ts, colour = ts)) + 
    #geom_line() +
    #scale_color_manual(values=c("value" = "grey","mean" = "red","mean_c"="green",
    #                        "sigma" = "blue", "sigma_c" = "orange"))

    #return(list(graph = p1, df = df))
    return(df)

    }else{
    
    x_val <- c()
    x_mu <- c()
    x_mu_c <- c()
    x_sigma <- c()
    x_sigma_c <- c()


    xy_dens <- c()
    xy_cov <- c()
    xy_cov_c <- c()

    y_val <- c()
    y_mu <- c()
    y_mu_c <- c()
    y_sigma <- c()
    y_sigma_c <- c()

    for( i in 1:length(GAS_obj)){

        x_val <- c(x_val,GAS_obj[[i]]$x[1])
        x_mu <- c(x_mu,GAS_obj[[i]]$mu[1])
        x_mu_c <- c(x_mu_c,GAS_obj[[i]]$mu_c[1])
        x_sigma <- c(x_sigma,GAS_obj[[i]]$sigma[1,1])
        x_sigma_c <- c(x_sigma_c,GAS_obj[[i]]$sigma_c[1,1])

        xy_dens <- c(xy_dens,GAS_obj[[i]]$hdens)
        xy_cov <- c(xy_cov,GAS_obj[[i]]$sigma[1,2]) 
        xy_cov_c <- c(xy_cov_c,GAS_obj[[i]]$sigma_c[1,2]) 

        y_val <- c(y_val,GAS_obj[[i]]$x[2])
        y_mu <- c(y_mu,GAS_obj[[i]]$mu[2])
        y_mu_c <- c(y_mu_c,GAS_obj[[i]]$mu_c[2])
        y_sigma <- c(y_sigma,GAS_obj[[i]]$sigma[2,2])
        y_sigma_c <- c(y_sigma_c,GAS_obj[[i]]$sigma_c[2,2])

    }

    df <- data.frame(
        x_val = x_val,
        x_mu = x_mu,
        x_mu_c = x_mu_c,
        x_sigma = x_sigma,
        x_sigma_c = x_sigma_c,

        y_val = y_val,
        y_mu = y_mu,
        y_mu_c = y_mu_c,
        y_sigma = y_sigma,
        y_sigma_c = y_sigma_c,

        xy_dens = xy_dens,
        xy_cov = xy_cov,
        xy_cov_c = xy_cov_c
    )

    
    # df_long_x <- data.frame(x = rep(1:length(GAS_obj),5),
    #                     value = c(x_val,x_mu, x_mu_c,sqrt(x_sigma),sqrt(x_sigma_c)),
    #                     ts = c(rep("value",length(GAS_obj)),
    #                         rep("mean",length(GAS_obj)),
    #                         rep("mean_c",length(GAS_obj)),
    #                         rep("sigma",length(GAS_obj)),
    #                         rep("sigma_c",length(GAS_obj))))
                            
    # df_long_x$ts <- factor(df_long_x$ts, levels = c("value","mean","mean_c",
    #                                         "sigma","sigma_c"))


    print("1- LH")
    x_lh <- -sum(log(dt(x = (df$x_val - df$x_mu_c)/sqrt(df$x_sigma_c) , df = nu)/sqrt(df$x_sigma_c)))
    print(x_lh)

    print("2- LH")
    y_lh <- -sum(log(dt(x = (df$y_val- df$y_mu_c)/sqrt(df$y_sigma_c) , df= nu)/sqrt(df$y_sigma_c)) )
    print(y_lh)

    print("Both- LH")
    lh <- 0 
    for( i in 1:length(df$x_val))
    {
  
     lh <- lh - sum(mvnfast::dmvt(X = c(df$x_val[i],df$y_val[i]), 
                                mu = c(df$x_mu_c[i],df$y_mu_c[i]),
                                sigma = matrix(c(df$x_sigma_c[i],df$xy_cov_c[i],df$xy_cov_c[i],df$y_sigma_c[i]),2,2), df = nu,
                                log = TRUE))
    }
    print(lh)


    #px <-ggplot(data= df_long_x, aes( x = x, y = value, group = ts, colour = ts)) + 
    #     geom_line() +
    #     scale_color_manual(values=c("value" = "grey","mean" = "red","mean_c"="green",
    #                                 "sigma" = "blue", "sigma_c" = "orange"))

    #df_long_y <- data.frame(x = rep(1:length(GAS_obj),5),
    #                    value = c(y_val,y_mu, y_mu_c,sqrt(y_sigma),sqrt(y_sigma_c)),
    ##                    ts = c(rep("value",length(GAS_obj)),
    #                        rep("mean",length(GAS_obj)),
    #                        rep("mean_c",length(GAS_obj)),
    #                        rep("sigma",length(GAS_obj)),
    #                        rep("sigma_c",length(GAS_obj))))
                            
    #df_long_y$ts <- factor(df_long_y$ts, levels = c("value","mean","mean_c",
    #                                        "sigma","sigma_c"))

    #py <-ggplot(data= df_long_y, aes( x = x, y = value, group = ts, colour = ts)) + 
    #     geom_line() +
    #     scale_color_manual(values=c("value" = "grey","mean" = "red","mean_c"="green",
    #                                 "sigma" = "blue", "sigma_c" = "orange"))

    #return(list(graph_x = px,graph_y = py, df = df))

    return(df)
    }
   
}




