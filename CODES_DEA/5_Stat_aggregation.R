# sourcing libraries and setdw
source("codes/0_libraries.R")
# loading tables
source("codes/4_creating_tables.R")

# loading data about stock returns
load("data/stocks_lr.RData")

# creating tables
create.tables(var.names = c("LRuc.Pvalue","LRcc.Pvalue",
                            "Ind.Pvalue", "DQ.pvalue","Loss.Loss","Loss.RC1","Loss.RC2",
                            "Loss.RL.rel","Loss.RQ.rel"),
              s_l= "long")
create.tables(var.names = c("LRuc.Pvalue","LRcc.Pvalue",
                            "Ind.Pvalue", "DQ.pvalue","Loss.Loss","Loss.RC1","Loss.RC2",
                            "Loss.RL.rel","Loss.RQ.rel"),
              s_l= "short")

# count aggregation
good.agr <- function(x, alpha){
  # matrix of TF
  bol.list <- lapply(x, function(z) (z >= alpha)*1)
  
  prod.mat <- x[[1]]*0 + 1
  
  for(i in 1:length(x))
  {
    prod.mat <- prod.mat * bol.list[[i]]
  }
  
  sum.mat <- apply(prod.mat, 1 , sum)
  
  return(sum.mat)
 # return(prod.mat)
}
