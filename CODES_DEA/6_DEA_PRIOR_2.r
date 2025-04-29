
# Russel output-only
RusselDEA.out.only2 <- function(i){
  identita <- diag(rep(1,3))

  # rozmery premenn�ch modelu 
  s <- nrow(Y)
  
  # lambda - 46 rozmern� nez�porn� premenn�
  lambda <- Variable(ncol(Y), nonneg = TRUE)
  
  # phi - s-rozmern� kladn� premenn�
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmern� premenn�
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma1 <- Variable(2,2, PSD = TRUE)
  Sigma2 <- Variable(2,2, PSD = TRUE)
  Sigma3 <- Variable(2,2, PSD = TRUE)  
  # ucelova funkcia
  objective <- Minimize( sum( tau )/s) 
  
  
  # ohranicenia
  constr <- list( Y%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  phi >= 1,

                  Sigma1[1,1] == phi[1],
                  Sigma1[2,2] == tau[1],
                  Sigma1[1,2] == 1,
                  Sigma1[2,1] == 1,

                  Sigma2[1,1] == phi[2],
                  Sigma2[2,2] == tau[2],
                  Sigma2[1,2] == 1,
                  Sigma2[2,1] == 1,

                  Sigma3[1,1] == phi[3],
                  Sigma3[2,2] == tau[3],
                  Sigma3[1,2] == 1,
                  Sigma3[2,1] == 1

  )
  
  # generovanie optimaliza�n�ho probl�mu problemu
  prob <- Problem(objective, constr)
  
  # rie�enie probl�mu 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps_rel = 1e-15,
               eps_abs = 1e-15)
  
  vystup <- c(sol$value,sol[[1]], round(sol[[3]], digits = 4))
  names(vystup) <- c("efektivita","tau1","tau2","tau3",1:28) 

  # v�stupom je vektor s celkovou a parcialn�mi efektivitami
  return(vystup)
}



# Russel output-only
supeff.out.only <- function(i){
  
  identita <- diag(rep(1,3))
  
  # rozmery premenn�ch modelu 
  s <- nrow(Y)
  
  # lambda - 46 rozmern� nez�porn� premenn�
  lambda <- Variable(ncol(Y[,-i]), nonneg = TRUE)
  
  # phi - s-rozmern� kladn� premenn�
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmern� premenn�
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  # ucelova funkcia
  objective <- Minimize( sum( tau )/s ) 
  
  # ohranicenia
  constr <- list( Y[,-i]%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  Sigma[1:3,1:3] == diag(phi),
                  Sigma[4:6,4:6] == diag(tau),
                  Sigma[1:3,4:6] == identita,
                  Sigma[4:6,1:3] == identita,
                  phi <= 1
  )
  
  # generovanie optimaliza�n�ho probl�mu problemu
  prob <- Problem(objective, constr)
  
  # rie�enie probl�mu 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps_rel = 1e-15,
               eps_abs = 1e-15)
  vystup <- c(sol$value,sol[[1]])
  names(vystup) <- c("efektivita","tau1","tau2","tau3") 
  
  # v�stupom je vektor s celkovou a parcialn�mi efektivitami
  return(vystup)
}



# DEA outputs
RusselDEA.out.both <- function(i){
  
  identita <- diag(rep(1,3))
  
  # rozmery premenn�ch modelu 
  m <- nrow(B)
  s <- nrow(Y)
  
  # deklarovanie premennych 
  # theta - s-rozmern� premenna
  theta <- Variable(m, nonneg = TRUE)
  
  # lambda - 46 rozmern� nez�porn� premenn�
  lambda <- Variable(ncol(Y), nonneg = TRUE)
  
  # phi - s-rozmern� kladn� premenn�
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmern� premenn�
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  
  # ucelova funkcia
  objective <- Minimize( ( sum( tau ) + sum(theta)  )/(m+s) )
  
  
  # ohranicenia
  constr <- list( B%*%lambda <= theta*B[,i],
                  Y%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  Sigma[1:3,1:3] == diag(phi),
                  Sigma[4:6,4:6] == diag(tau),
                  Sigma[1:3,4:6] == identita,
                  Sigma[4:6,1:3] == identita,
                  theta <= 1,
                  phi >= 1
  )
  
  # generovanie optimaliza�n�ho probl�mu problemu
  prob <- Problem(objective, constr)
  
  # rie�enie probl�mu 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000,eps_rel = 1e-15,
               eps_abs = 1e-15)
  vystup <- c(sol$value,sol[[1]], sol[[2]],round(sol[[3]], digits = 4))
  names(vystup) <- c("efektivita","tau1","tau2","tau3","theta1","theta2","theta3",1:28) 
  
  # v�stupom je vektor s celkovou a parcialn�mi efektivitami
  return(vystup)
}


supeff.out.both <- function(i){
  
  identita <- diag(rep(1,3))
  
  # rozmery premenn�ch modelu 
  m <- nrow(B)
  s <- nrow(Y)
  
  # deklarovanie premennych 
  # theta - s-rozmern� premenna
  theta <- Variable(m, nonneg = TRUE)
  
  # lambda - 46 rozmern� nez�porn� premenn�
  lambda <- Variable(ncol(Y[,-i]), nonneg = TRUE)
  
  # phi - s-rozmern� kladn� premenn�
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmern� premenn�
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  
  # ucelova funkcia
  objective <- Minimize( ( sum( tau ) + sum(theta)  )/(m+s) )
  
  
  # ohranicenia
  constr <- list( B[,-i]%*%lambda <= theta*B[,i],
                  Y[,-i]%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  Sigma[1:3,1:3] == diag(phi),
                  Sigma[4:6,4:6] == diag(tau),
                  Sigma[1:3,4:6] == identita,
                  Sigma[4:6,1:3] == identita,
                  theta >= 1,
                  phi <= 1
  )
  
  # generovanie optimaliza�n�ho probl�mu problemu
  prob <- Problem(objective, constr)
  
  # rie�enie probl�mu 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps_rel = 1e-20,
               eps_abs = 1e-20)
  vystup <- c(sol$value,sol[[1]], sol[[2]])
  names(vystup) <- c("efektivita","tau1","tau2","tau3","theta1","theta2","theta3") 
  
  # v�stupom je vektor s celkovou a parcialn�mi efektivitami
  return(vystup)
}

