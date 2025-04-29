
# x is always yield rate
# y is always index

Entropy_fun <- function(Obj_x,Obj_y,Obj_xy, alpha, wind){

        # Whole entropy
        entr_x <- numeric(length(alpha))
        entr_y <- numeric(length(alpha))

        # p-value of weighted t-test
        test_x <- numeric(length(alpha))
        test_y <- numeric(length(alpha))

        # probability of history of x normed to 1
        w_x <- Obj_x$dens / sum(Obj_x$dens)
        # probability of history of y normed to 1
        w_y <- Obj_y$dens / sum(Obj_y$dens)
        # probability of both histories x and y normed to 1
        w_xy <- Obj_xy$xy_dens/ sum(Obj_xy$xy_dens)

        w_x <- w_y <- w_xy <- rep(1,length(Obj_x$dens))

        ts_entr_x <- c()
        ts_entr_y <- c()

        for( i in 1:length(alpha)){
  
                x_val <- entropy_norm(x = Obj_x$sigma_c, 
                                        y = Obj_xy$x_sigma_c, 
                                        w_x = w_x, 
                                        w_y = w_xy, 
                                        alpha = alpha[i])

                entr_x[i] <- x_val$entr
                test_x[i] <- x_val$test


                y_val <- entropy_norm(x = Obj_y$sigma_c,
                                        y = Obj_xy$y_sigma_c, 
                                        w_x = w_y, 
                                        w_y = w_xy, 
                                        alpha = alpha[i])

                entr_y[i] <- y_val$entr
                test_y[i] <- y_val$test

                # time varying entropy
                entr_x_wind <- numeric(wind)
                entr_y_wind <- numeric(wind)

                for( j in 1:(length(w_x) - wind)){
                
                # probability of history of x normed to 1
                # w_x_wind <- Obj_x$dens[j:(j+wind)] / sum(Obj_x$dens[j:(j+wind)])
                # # probability of history of y normed to 1
                # w_y_wind  <- Obj_y$dens[j:(j+wind)] / sum(Obj_y$dens[j:(j+wind)])
                # # probability of both histories x and y normed to 1
                # w_xy_wind  <- Obj_xy$xy_dens[j:(j+wind)] / sum(Obj_xy$xy_dens[j:(j+wind)])

                w_x_wind <- w_y_wind <- w_xy_wind <- rep(1, wind)
                
                entr_x_wind[j] <- entropy_norm(x = Obj_x$sigma_c[j:(j+wind-1)], 
                                          y = Obj_xy$x_sigma_c[j:(j+wind-1)], 
                                          w_x = w_x_wind, 
                                          w_y = w_xy_wind, 
                                          alpha = alpha[i])$entr
                entr_y_wind[j] <- entropy_norm(x = Obj_y$sigma_c[j:(j+wind-1)],
                                          y = Obj_xy$y_sigma_c[j:(j+wind-1)], 
                                          w_x = w_y_wind, 
                                          w_y = w_xy_wind, 
                                          alpha = alpha[i])$entr
                }

                ts_entr_x <- rbind(ts_entr_x, entr_x_wind)
                ts_entr_y <- rbind(ts_entr_y, entr_y_wind)

        }

        return(list(entr_x = entr_x, entr_y = entr_y, test_x = test_x, test_y = test_y, ts_entr_x = ts_entr_x, ts_entr_y = ts_entr_y,
        xp1 = x_val$p1, xp2 = x_val$p2, yp1 = y_val$p1, yp2 = y_val$p2))

}



entropy_norm <- function(x, y, w_x, w_y, alpha ){

        # w_x <- history of itself only
        # w_y <- history of both time series
        if(alpha ==1){
           entr <-  (w_x%*%log(x)/2- w_y%*%log(y)/2)/length(x)
           test <- t.test(x = log(x)/2, y = log(y)/2,  alternative = "two.sided")$p.value
        }else{
          entr <- alpha/(1-alpha) * (log( w_x%*%x^( (1-alpha)/(2*alpha) )) - log( w_y%*%y^( (1-alpha)/(2*alpha) )) )

          if(alpha < 1){
            test_orinet = "two.sided"
          }else{
            test_orinet = "two.sided"
          }

          test <- t.test(x = x^( (1-alpha)/(2*alpha)), y = y^( (1-alpha)/(2*alpha) ),  alternative = test_orinet)$p.value


        }
        # test <- wtd.t.test(x = x^( (1-alpha)/(2*alpha)), y = y^( (1-alpha)/(2*alpha) ), 
        #         weight = w_x, weighty = w_y, mean1 = TRUE)$coefficients["p.value"]

        #test <- t.test(x = x^( (1-alpha)/(2*alpha)), y = y^( (1-alpha)/(2*alpha) ),  alternative = "two.sided")$p.value

         #, alternative = "greater"
        return(list(entr= entr, test= test, p1 =  x^( (1-alpha)/(2*alpha)) ,  p2 = y^( (1-alpha)/(2*alpha) ) ))
}

ret_func <- function(GAS_obj, UNI = TRUE){

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

    print("X- LH")
    x_lh <- -sum(dnorm(x = df$val, mean= df$mu_c, sd= sqrt(df$sigma_c) , log= TRUE))
    print(x_lh)



    df_long <- data.frame(x = rep(1:length(GAS_obj),5),
                        value = c(val,mu, mu_c,sqrt(sigma),sqrt(sigma_c)),
                        ts = c(rep("value",length(GAS_obj)),
                            rep("mean",length(GAS_obj)),
                            rep("mean_c",length(GAS_obj)),
                            rep("sigma",length(GAS_obj)),
                            rep("sigma_c",length(GAS_obj))))
                            
    df_long$ts <- factor(df_long$ts, levels = c("value","mean","mean_c",
                                            "sigma","sigma_c"))


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

    print("X- LH")
    x_lh <- -sum(dnorm(x = df$x_val, mean= df$x_mu_c, sd= sqrt(df$x_sigma_c) , log= TRUE))
    print(x_lh)

    print("Y- LH")
    y_lh <- -sum(dnorm(x = df$y_val, mean= df$y_mu_c, sd= sqrt(df$y_sigma_c) , log= TRUE))
    print(y_lh)

   

    print("Both- LH")
    lh <- 0 
    for( i in 1:length(df$x_val))
    {
  
     lh <- lh - sum(mvnfast::dmvn(X = c(df$x_val[i],df$y_val[i]), 
                                mu = c(df$x_mu_c[i],df$y_mu_c[i]),
                                sigma = matrix(c(df$x_sigma_c[i],df$xy_cov_c[i],df$xy_cov_c[i],df$y_sigma_c[i]),2,2),
                                log = TRUE))
    }
    print(length(df$x_val))
    print(lh)
    
    df_long_x <- data.frame(x = rep(1:length(GAS_obj),5),
                        value = c(x_val,x_mu, x_mu_c,sqrt(x_sigma),sqrt(x_sigma_c)),
                        ts = c(rep("value",length(GAS_obj)),
                            rep("mean",length(GAS_obj)),
                            rep("mean_c",length(GAS_obj)),
                            rep("sigma",length(GAS_obj)),
                            rep("sigma_c",length(GAS_obj))))
                            
    df_long_x$ts <- factor(df_long_x$ts, levels = c("value","mean","mean_c",
                                            "sigma","sigma_c"))

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


