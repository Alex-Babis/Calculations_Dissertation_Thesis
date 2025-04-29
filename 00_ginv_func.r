# half inverse of matrix
ginv_2 <- function (X, tol = .Machine$double.eps^(1/3), h = -1) {

   
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- eigen(X, symmetric = TRUE)
    Positive <- Xsvd$values > max(tol * Xsvd$values[1L], 0)
    if (all(Positive)) 
        Xsvd$vectors %*% (Xsvd$values^(h) * t(Xsvd$vectors))
      
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$vectors[, Positive, drop = FALSE] %*% (Xsvd$values[Positive]^(h) *
        t(Xsvd$vectors[, Positive, drop = FALSE]))
}


#.Machine$double.eps^(1/4)
