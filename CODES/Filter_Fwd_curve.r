# Artificial data

bid <- c(252.5,248.75,253.25,252,252,252,252,236,256,260,263,275,276,280,235,230,251,266.5,220.5,218.5,250,240)
ask <- c(253,250,257,255,255,255,254,236.5,258,263,269,277,276.5,283.5,240,233,259,272,228.5,226.5,250.5,244)
hodnoty <- (bid+ask)/2

zaciatok_m <- c("2005-05-09","2005-05-16","2005-05-23","2005-05-30","2005-06-06","2005-06-13","2005-06-01","2005-07-01","2005-08-01","2005-09-01","2005-10-01","2005-11-01","2005-10-01","2006-01-01","2006-04-01","2006-07-01","2006-10-01","2007-01-01","2007-04-01","2007-07-01","2007-10-01","2008-01-01")


koniec_m <- c("2005-05-15","2005-05-22","2005-05-29","2005-06-05","2005-06-12","2005-06-19","2005-06-30","2005-07-31","2005-08-31","2005-09-30","2005-10-31","2005-11-30","2005-12-31","2006-03-31","2006-06-30","2006-09-30","2006-12-31","2007-03-31","2007-06-30","2007-09-30","2007-12-31","2008-12-31")

mean(c(280,235,230,251))

# Datum
zaciatok_m <- as.Date(zaciatok_m)
koniec_m <- as.Date(koniec_m)

# CISLO
start_s <- as.numeric(zaciatok_m) - 12911 + 6
end_s <- as.numeric(koniec_m) - 12911 + 7

start_s<- start_s/365
end_s <- end_s/365
# Vykreslenie hodnot a grafov 

times <- unique(c(start_s, end_s))
times[order(times)]


library(ggplot2)

df <- data.frame(start_x = start_s,
                 end_x = end_s, 
                 start_y = hodnoty,
                 end_y = hodnoty)


ggplot(df, aes( x = start_x, y = start_y, xend = end_x, yend = end_y)) + geom_segment()


# pocet segmentov


Ucelova_funkcia <- function( start, end ){

    times <- unique(c(start,end))
     
    times <- times[order(times)] 

    # count of spline functions
    cnt <- length(times) - 1

    # creating empty matrix for objective function
    H <- matrix(0, 5*cnt, 5*cnt)

    # creating vector of distances between two points
    dif_t <- diff(times)

    for( i in 1:cnt){

        H[1:3 + (i-1)*5, 1:3 + (i-1)*5] <- matrix(c(144/5 * dif_t[i]^5, 18*dif_t[i]^4,8*dif_t[i]^3 ,
                                                    18*dif_t[i]^4, 12*dif_t[i]^3, 6*dif_t[i]^2,
                                                    8*dif_t[i]^3, 6*dif_t[i]^2, 4*dif_t[i]), 3,3, byrow= TRUE)
    }

    return(H)

}

# function that integrates spline on interval t0, t1
spline_int <- function(t1,t0,r){

    # coef <- c( -exp(-r*dif_t)*( (r*dif_t)^4 + 4*(r*dif_t)^3 + 12*(r*dif_t)^2 + 24*(r*dif_t) + 24  )/r^5,
    #            -exp(-r*dif_t)*( (r*dif_t)^3 + 3*(r*dif_t)^2 + 6*(r*dif_t) + 6  )/r^4,
    #            -exp(-r*dif_t)*( (r*dif_t)^2 + 2*(r*dif_t) + 2  )/r^3,
    #            -exp(-r*dif_t)*( (r*dif_t) + 1  )/r^2,
    #            -exp(-r*dif_t)/r)

    coef <- c( (t1^5 - t0^5)/5, 
               (t1^4 - t0^4)/4,
               (t1^3 - t0^3)/3,
               (t1^2 - t0^2)/2,
               (t1^1 - t0^1)/1 )

    coef <- matrix(coef, nrow = 5, ncol = length(t1), byrow = TRUE)

    coef <- as.vector(coef)

    return(coef)
}

Splain_e <- function(time = 0.5, coef = coef, start = start_s, end = end_s){

    times <- unique(c(start, end))
    
    times <- times[order(times)]

    time_mat <- lapply(time, function(x){

        ind <- max(which(x >= times))

        value <- x^(4:0)%*%coef[ind,]

        return(value)
    } )

    time_mat <- unlist(time_mat)

    return(time_mat)
}



Ohranicenia <- function(F_g = hodnoty[1:2], start = start_s, end = end_s, r =0.05){

    times <- unique(c(start, end))

    times <- times[order(times)]

    # number of spline function
    n <- length(times) - 1

    # Continuity
    A_1 <- matrix(0, nrow = n - 1, ncol = n*5 )

    # Continuity of first derivatives
    A_2 <- matrix(0, nrow = n - 1, ncol = n*5)

    # Continuity of second derivatives
    A_3 <- matrix(0, nrow = n - 1, ncol = n*5)

    # Boundry condition on first derivative
    A_4 <- matrix(0, nrow = 1, ncol =  n*5)

    for(i in 1:(n-1)){
        A_1[i, 1:10 + (i-1)*5] <-  c(times[i + 1]^4,times[i + 1]^3,times[i + 1]^2,times[i + 1],1,
                                     -times[i + 1]^4,-times[i + 1]^3,-times[i + 1]^2,-times[i + 1],-1)

        A_2[i, 1:10 + (i-1)*5] <- c(4*times[i + 1]^3, 3*times[i + 1]^2, 2*times[i + 1],1, 0, 
                                    -4*times[i + 1]^3, -3*times[i + 1]^2, -2*times[i + 1],-1, 0)

        A_3[i, 1:10 + (i-1)*5] <- c(12*times[i + 1]^2, 6*times[i + 1], 2, 0, 0, 
                                    -12*times[i + 1]^2, -6*times[i + 1], -2,0, 0)

    }

    A_4[ 1:5 + (n-1)*5] <- c(4*times[n+1]^3, 3*times[n+1]^2, 2*times[n+1],1, 0)


    # Condition on average-based fwd curves
    A_5 <- matrix(0, nrow = length(F_g), ncol = n *5)
    ps_5 <- rep(0,length(F_g))

    for(i in 1:length(F_g)){

        ind_t <- which(times >= start[i] & times <= end[i])

        #times_new <- times[ind_t]

        ps_5[i] <- F_g[i]*(end[i]-start[i])
        
        A_5[i, (ind_t[1]*5 - 4):((ind_t[length(ind_t)] - 1)*5) ] <- spline_int(t1 = times[ind_t[-1]], 
                                                                               t0 = times[ind_t[-length(ind_t)]], r= r)

        length((ind_t[1]*5 - 4):((ind_t[length(ind_t)] - 1)*5))
    }

    # season <- rep(0, length(F_g))

    # seas_fun <- function(x){
    #     exp(-r*x)*(145.732 + 29.735*cos((x+16.691)*2*pi/365))
    # }

    # for( i in 1:length(F_g)){
    #     season[i] <- integrate(f = seas_fun, lower = start[i], upper = end[i])$value
    # } 

    #ps_5 <- F_g - 1/(end-start)* season




    A <- rbind(A_1, A_2, A_3, A_4, A_5)

    ps <- c(rep(0, nrow(A) - length(ps_5)),ps_5)


    return(list(A = A, ps = ps))
}


new_ohr <- Ohranicenia(F_g = hodnoty, start = start_s, end = end_s, r =0.05)
H_t <- Ucelova_funkcia(start = start_s, end = end_s)

H_new <- rbind( cbind( 2*H_t, t(new_ohr$A)), cbind(new_ohr$A, matrix(0, nrow(new_ohr$A),nrow(new_ohr$A))))

ps_new <- c(rep(0,nrow(H_t)), new_ohr$ps)

sol <- solve(H_new, ps_new)
 
coef <- matrix(sol[1:nrow(H_t)], byrow =  TRUE, ncol = 5)


time <- seq(min(start_s), max(end_s), 0.001)
val <-Splain_e(time = time, coef = coef, start = start_s, end = end_s)
plot(time, val, type = 'l', ylim = c(0, 450))
segments(x0 = df$start_x, y0 = df$start_y , x1 =df$end_x, y1 = df$end_y)
