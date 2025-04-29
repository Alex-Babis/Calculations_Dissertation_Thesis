# DEA OUTPUT-ORIENTED
RusselDEA.out.orient <- function(i)
{
  identita <- diag(rep(1,3))
  
  # rozmery premenných modelu 
  m <- nrow(B)
  s <- nrow(Y)
  
  # deklarovanie premennych 
  # theta - s-rozmerná premenna
  theta <- Variable(m, nonneg = TRUE)
  
  # lambda - 46 rozmerná nezáporná premenná
  lambda <- Variable(ncol(Y), nonneg = TRUE)
  
  # phi - s-rozmerná kladná premenná
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmerná premenná
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  # ucelova funkcia
  objective <- Minimize(sum( tau )/ m )
  
  
  # ohranicenia
  constr <- list( B%*%lambda <= B[,i],
                  Y%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  Sigma[1:3,1:3] == diag(phi),
                  Sigma[4:6,4:6] == diag(tau),
                  Sigma[1:3,4:6] == identita,
                  Sigma[4:6,1:3] == identita,
                  phi >= 1
  )
  
  # generovanie optimalizaèného problému problemu
  prob <- Problem(objective, constr)
  
  # riešenie problému 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps = 1e-9)
  vystup <- c(sol$value,sol[[1]],round(sol[[3]], digits = 4))
  names(vystup) <- c("efektivita","tau1","tau2","tau3",1:28) 
  
  # výstupom je vektor s celkovou a parcialnými efektivitami
  return(vystup)
}




















supeff.out.orient <- function(i)
{
  identita <- diag(rep(1,3))
  
  # rozmery premenných modelu 
  m <- nrow(B)
  s <- nrow(Y)
  
  # deklarovanie premennych 
  # theta - s-rozmerná premenna
  theta <- Variable(m, nonneg = TRUE)
  
  # lambda - 46 rozmerná nezáporná premenná
  lambda <- Variable(ncol(Y[,-i]), nonneg = TRUE)
  
  # phi - s-rozmerná kladná premenná
  phi <- Variable(s, nonneg = TRUE)
  
  # tau - p-rozmerná premenná
  tau <- Variable(s, nonneg = TRUE)
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  # ucelova funkcia
  objective <- Minimize(sum( tau )/ m )
  
  
  # ohranicenia
  constr <- list( B[,-i]%*%lambda <= B[,i],
                  Y[,-i]%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                  Sigma[1:3,1:3] == diag(phi),
                  Sigma[4:6,4:6] == diag(tau),
                  Sigma[1:3,4:6] == identita,
                  Sigma[4:6,1:3] == identita,
                  phi <= 1
  )
  
  # generovanie optimalizaèného problému problemu
  prob <- Problem(objective, constr)
  
  # riešenie problému 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps = 1e-9)
  vystup <- c(sol$value,sol[[1]],round(sol[[3]], digits = 4))
  names(vystup) <- c("efektivita","tau1","tau2","tau3",1:28) 
  
  # výstupom je vektor s celkovou a parcialnými efektivitami
  return(vystup)
}






# druha faza pre max slacky pri B
RusselDEA.out.orient <- function(i)
{

  # rozmery premenných modelu 
  m <- nrow(B)

  # deklarovanie premennych 
  # theta - s-rozmerná premenna
  slacks <- Variable(m, nonneg = TRUE)
  
  # lambda - 46 rozmerná nezáporná premenná
  lambda <- Variable(ncol(Y), nonneg = TRUE)
  
  # phi - s-rozmerná kladná premenná
  phi <- efektivita.out.orient[i,2:4]
  
  # symetricka PSD 2x2 matica 
  Sigma <- Variable(6,6, PSD = TRUE)
  
  # ucelova funkcia
  objective <- Maximaze(sum(slacks))
  
  # ohranicenia
  constr <- list( B%*%lambda + slacks == B[,i],
                  Y%*%lambda >= phi*Y[,i],
                  sum(lambda) == 1,
                )
  
  # generovanie optimalizaèného problému problemu
  prob <- Problem(objective, constr)
  
  # riešenie problému 
  sol <- solve(prob,  solver = "SCS", max_iters= 20000, eps = 1e-9)
  vystup <- c(sol$value,sol[[1]],round(sol[[3]], digits = 4))
  names(vystup) <- c("efektivita","slack1","slack2","slack3",1:28) 
  
  # výstupom je vektor s celkovou a parcialnými efektivitami
  return(vystup)
}