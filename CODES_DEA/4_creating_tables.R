create.tables <- function(
  var.names = NULL,
  s_l = "long"
){
  # loading all data for long or short position

  load(paste0("data/VaRnorm_504_",s_l,".Rdata"),norm.tab <- new.env())
  norm.tab <- norm.tab$garch.var.list
  
  load(paste0("data/VaRsnorm_504_",s_l,".Rdata"),snorm.tab <- new.env())
  snorm.tab <- snorm.tab$garch.var.list
  
  load(paste0("data/VaRstd_504_",s_l,".Rdata"),std.tab <- new.env())
  std.tab <- std.tab$garch.var.list
  
  load(paste0("data/VaRsstd_504_",s_l,".Rdata"),sstd.tab <- new.env())
  sstd.tab <- sstd.tab$garch.var.list
  
  tab10 <- list()
  tab5 <- list()
  tab2.5 <- list()
  tab1 <- list()

  for(i in 1:length(var.names))
  {
    # for alpha == 10
    # NORM
    tab10_norm <- lapply(norm.tab, function(x){
                        y <- x[seq(1, nrow(x), 4),var.names[i]]
                        return(y)
                      })
    tab10_norm <- do.call(rbind, tab10_norm)
    rownames(tab10_norm) <- paste0(rownames(tab10_norm),"_norm")
    
    # SNORM
    tab10_snorm <- lapply(snorm.tab, function(x){
      y <- x[seq(1, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab10_snorm <- do.call(rbind, tab10_snorm)
    rownames(tab10_snorm) <- paste0(rownames(tab10_snorm),"_snorm")
    
    # STD
    tab10_std <- lapply(std.tab, function(x){
      y <- x[seq(1, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab10_std <- do.call(rbind, tab10_std)
    rownames(tab10_std) <- paste0(rownames(tab10_std),"_std")
    
    # SSTD
    tab10_sstd <- lapply(sstd.tab, function(x){
      y <- x[seq(1, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab10_sstd <- do.call(rbind, tab10_sstd)
    rownames(tab10_sstd) <- paste0(rownames(tab10_sstd),"_sstd")
    
    tab10[[i]] <- rbind(tab10_norm,tab10_snorm,tab10_std,tab10_sstd)
    
    # for alpha == 5
    # NORM
    tab5_norm <- lapply(norm.tab, function(x){
      y <- x[seq(2, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab5_norm <- do.call(rbind, tab5_norm)
    rownames(tab5_norm) <- paste0(rownames(tab5_norm),"_norm")
    
    # SNORM
    tab5_snorm <- lapply(snorm.tab, function(x){
      y <- x[seq(2, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab5_snorm <- do.call(rbind, tab5_snorm)
    rownames(tab5_snorm) <- paste0(rownames(tab5_snorm),"_snorm")
    
    # STD
    tab5_std <- lapply(std.tab, function(x){
      y <- x[seq(2, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab5_std <- do.call(rbind, tab5_std)
    rownames(tab5_std) <- paste0(rownames(tab5_std),"_std")
    
    # SSTD
    tab5_sstd <- lapply(sstd.tab, function(x){
      y <- x[seq(2, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab5_sstd <- do.call(rbind, tab5_sstd)
    rownames(tab5_sstd) <- paste0(rownames(tab5_sstd),"_sstd")
    
    tab5[[i]] <- rbind(tab5_norm,tab5_snorm,tab5_std,tab5_sstd)
    
    # for alpha == 2.5
    # NORM
    tab2.5_norm <- lapply(norm.tab, function(x){
      y <- x[seq(3, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab2.5_norm <- do.call(rbind, tab2.5_norm)
    rownames(tab2.5_norm) <- paste0(rownames(tab2.5_norm),"_norm")
    
    # SNORM
    tab2.5_snorm <- lapply(snorm.tab, function(x){
      y <- x[seq(3, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab2.5_snorm <- do.call(rbind, tab2.5_snorm)
    rownames(tab2.5_snorm) <- paste0(rownames(tab2.5_snorm),"_snorm")
    
    # STD
    tab2.5_std <- lapply(std.tab, function(x){
      y <- x[seq(3, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab2.5_std <- do.call(rbind, tab2.5_std)
    rownames(tab2.5_std) <- paste0(rownames(tab2.5_std),"_std")
    
    # SSTD
    tab2.5_sstd <- lapply(sstd.tab, function(x){
      y <- x[seq(3, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab2.5_sstd <- do.call(rbind, tab2.5_sstd)
    rownames(tab2.5_sstd) <- paste0(rownames(tab2.5_sstd),"_sstd")
    
    tab2.5[[i]] <- rbind(tab2.5_norm,tab2.5_snorm,tab2.5_std,tab2.5_sstd)
    
    # for alpha == 1
    # NORM
    tab1_norm <- lapply(norm.tab, function(x){
      y <- x[seq(4, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab1_norm <- do.call(rbind, tab1_norm)
    rownames(tab1_norm) <- paste0(rownames(tab1_norm),"_norm")
    
    # SNORM
    tab1_snorm <- lapply(snorm.tab, function(x){
      y <- x[seq(4, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab1_snorm <- do.call(rbind, tab1_snorm)
    rownames(tab1_snorm) <- paste0(rownames(tab1_snorm),"_snorm")
    
    # STD
    tab1_std <- lapply(std.tab, function(x){
      y <- x[seq(4, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab1_std <- do.call(rbind, tab1_std)
    rownames(tab1_std) <- paste0(rownames(tab1_std),"_std")
    
    # SSTD
    tab1_sstd <- lapply(sstd.tab, function(x){
      y <- x[seq(4, nrow(x), 4),var.names[i]]
      return(y)
    })
    tab1_sstd <- do.call(rbind, tab1_sstd)
    rownames(tab1_sstd) <- paste0(rownames(tab1_sstd),"_sstd")
    
    tab1[[i]] <- rbind(tab1_norm,tab1_snorm,tab1_std,tab1_sstd)
  }
  
  names(tab10) <- var.names
  names(tab5) <- var.names
  names(tab2.5) <- var.names
  names(tab1) <- var.names
  
  
  # creating list for every alpha level 10,5,2.5,1
  nam <- paste(s_l, "10", sep = "")
  assign(nam, tab10, envir = .GlobalEnv)
  
  nam <- paste(s_l, "5", sep = "")
  assign(nam, tab5, envir = .GlobalEnv)
  
  nam <- paste(s_l, "2.5", sep = "")
  assign(nam, tab2.5, envir = .GlobalEnv)
  
  nam <- paste(s_l, "1", sep = "")
  assign(nam, tab1, envir = .GlobalEnv)
  
  
}

