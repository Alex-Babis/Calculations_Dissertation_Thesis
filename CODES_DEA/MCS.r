
# computing value at risk for specific alpha 
VaR_aplpha <- function(data = NULL,
                       distribution = NULL,
                       alpha = NULL,
                       short = FALSE){
  # find which parameter from lambda skew and shape do exist in data
  param <- colnames(data)[colnames(data) %in% c("lambda","shape", "skew")]

  # create list with parameter available
  params <- lapply(param, function(i) data[,i])
  
  # name elements of list according to values
  names(params) <- param
  
  if(short)
  {
    alpha <- 1 - alpha
  }
  # create list of all parameters
  params <- c(list(distribution = distribution,
                   p = alpha,
                   mu = data[,"Mean_t_1"],
                   sigma = data[, "Sigma_t_1"]),params)

  # calculate VaR for specific alpha
  ret.var <- do.call(qdist,params)
  
  if( short){
    ret.var <-  -ret.var
  }
  # return value at risk 
  # 
  return(ret.var)
  
}


distrib <- c("norm", "snorm", "std","sstd")
garch.models <- data.frame( model = c("sGARCH","gjrGARCH","eGARCH",
                                      "iGARCH","fiGARCH",
                                      "csGARCH","fGARCH"),
                            submodels = I(list(NULL, NULL, NULL,
                                               NULL,NULL,
                                               NULL, "TGARCH")))

alpha = c(0.05,0.025,0.01)
#alpha = 0.05
short_var <- list()
long_var <- list()


pocet <- 1
for(k in 1:3){

for(i in 1:length(distrib)){
  
  for( j in 1:nrow(garch.models))
  {
    # load data for specific model 
    load.file <- paste0("data/",garch.models[[j,1]],
                          c("_")[!is.null(garch.models[[j,2]])],
                          garch.models[[j,2]],"_spec_",distrib[i],"_504_corrected.Rdata")
    
    load(load.file)
    # data loaded into garches file
    garch.tab <- c()
    
    # run VaR testing 
    for( mods in 1:length(garches))
    {

      garch_var <- VaR_aplpha(data = garches[[mods]], 
                              distribution = distrib[i], 
                              alpha= alpha[k], short = FALSE)
     
      loss_var <- LossVaR(stocks.lr[505:1026,mods], garch_var[-523], which = 'asymmetricLoss', 
             type = 'normal', tau=  alpha[k])

      garch.tab <- cbind(garch.tab,loss_var)
       
      
    }
    long_var[[pocet]] <- garch.tab
    names(long_var)[pocet] <- paste0(garch.models[[j,1]],
                                       c("_")[!is.null(garch.models[[j,2]])],
                                       garch.models[[j,2]],"_",distrib[i],"_",alpha[k])

    pocet <- pocet + 1
  }
  

  gc()
  
}


}


#save(short_var, file = "Short_var.Rdata")
#save(long_var, file = "Long_var.Rdata")

load(file = "Short_var.Rdata")
load(file = "Long_var.Rdata")

library(MCS)
# Long VaR MCS
# pre alpha 5% nedava zmysel... vsetky modely su rovnako dobre
# skusime alpha 20% 
long_mcs_list <- list()
short_mcs_list <- list()
cl1 <- makeCluster(14)

for(i in 1:ncol(stocks.lr)){
    print(i)
    loss_05 <- c()
    loss_025 <- c()
    loss_001 <- c()

    for( j in 1:(length(short_var)/3)){
        loss_05 <- cbind(loss_05, short_var[[j]][,i])
        loss_025 <- cbind(loss_025, short_var[[j + 28]][,i])
        loss_001 <- cbind(loss_001, short_var[[j + 2*28]][,i])
    }

    mcs05 <- MCSprocedure(Loss=loss_05,alpha=0.2,B=15000,statistic='Tmax',cl = cl1)
    mcs025 <- MCSprocedure(Loss=loss_025,alpha=0.2,B=15000,statistic='Tmax',cl = cl1)
    mcs001 <- MCSprocedure(Loss=loss_001,alpha=0.2,B=15000,statistic='Tmax',cl = cl1)
    
    short_mcs_list <- c(short_mcs_list,mcs05,mcs025,mcs001)

    names(short_mcs_list)[1:3 + (i-1)*3] <- c(paste0(i,"_05"),paste0(i,"_025"),paste0(i,"_001"))
    
 }

stopCluster(cl1)
# save(long_mcs_list, file = "long_mcs_list.Rdata")
# save(short_mcs_list, file = "short_mcs_list.Rdata")



ranking_by_loss <- c()

for(i in 1:ncol(stocks.lr)){
    print(i)
    loss_05 <- c()
    loss_025 <- c()
    loss_001 <- c()

    for( j in 1:(length(long_var)/3)){
        loss_05 <- cbind(loss_05, long_var[[j]][,i])
        loss_025 <- cbind(loss_025, long_var[[j + 28]][,i])
        loss_001 <- cbind(loss_001, long_var[[j + 2*28]][,i])
    }

    mcs05 <- apply(loss_05,2, mean)
    mcs025<- apply(loss_025,2, mean)
    mcs001<- apply(loss_001,2, mean)
   
    ranking_by_loss <- cbind(ranking_by_loss,order(mcs05),order(mcs025),order(mcs001))

    colnames(ranking_by_loss)[1:3 + (i-1)*3] <- c(paste0(i,"_05"),paste0(i,"_025"),paste0(i,"_001"))
    
 }


vol_dyn <- c("GARCH","GARCH","GARCH","GARCH",
               "iGARCH","iGARCH","iGARCH","iGARCH",
               "fiGARCH","fiGARCH","fiGARCH","fiGARCH",
               "csGARCH","csGARCH","csGARCH","csGARCH",
               "eGARCH","eGARCH","eGARCH","eGARCH",
               "tGARCH","tGARCH","tGARCH","tGARCH",
               "gjrGARCH","gjrGARCH","gjrGARCH","gjrGARCH")

vol_dis <- c("Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd",
             "Norm","sNorm","Std","sStd")

vol_nazvy <- paste0(vol_dyn, "_" ,vol_dis)

# mean rank
mean_rank_long_5 <- apply(ranking_by_loss[,1:28 * 3 - 2], 1, mean)
mean_rank_long_5 <- data.frame(Modely = names(long_var)[1:28],Mean_rank = mean_rank_long_5)
mean_rank_long_5 <- mean_rank_long_5[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22)),]
rownames(mean_rank_long_5) <- 1:28
mean_rank_long_5$Modely <- vol_nazvy


mean_rank_long_25 <- apply(ranking_by_loss[,1:28 * 3 - 1], 1, mean)
mean_rank_long_25 <- data.frame(Modely =names(long_var)[1:28],Mean_rank =mean_rank_long_25)
mean_rank_long_25 <- mean_rank_long_25[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22)),]
rownames(mean_rank_long_25) <- 1:28
mean_rank_long_25$Modely <- vol_nazvy

mean_rank_long_1 <- apply(ranking_by_loss[,1:28 * 3 ], 1, mean)
mean_rank_long_1 <- data.frame(Modely =names(long_var)[1:28],Mean_rank =mean_rank_long_1)
mean_rank_long_1 <- mean_rank_long_1[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22)),]
rownames(mean_rank_long_1) <- 1:28
mean_rank_long_1$Modely <- vol_nazvy

mean_rank_long <- apply(ranking_by_loss, 1, mean)
mean_rank_long <- data.frame(Modely =names(long_var)[1:28],Mean_rank =mean_rank_long)
mean_rank_long <- mean_rank_long[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22)),]
rownames(mean_rank_long) <- 1:28
mean_rank_long$Modely <- vol_nazvy


barplot(mean_rank_long[,2], names.arg = mean_rank_long[,1])
barplot(mean_rank_long_1[,2], names.arg = mean_rank_long_1[,1])
barplot(mean_rank_long_25[,2], names.arg = mean_rank_long_25[,1])
barplot(mean_rank_long_5[,2], names.arg = mean_rank_long_5[,1])

barplot(1/(mean_rank_long[,2]*max(1/mean_rank_long[,2])), names.arg = mean_rank_long[,1])
barplot(1/(mean_rank_long_1[,2]*max(1/mean_rank_long_1[,2])), names.arg = mean_rank_long_1[,1])
barplot(1/(mean_rank_long_25[,2]*max(1/mean_rank_long_25[,2])), names.arg = mean_rank_long_25[,1])
barplot(1/(mean_rank_long_5[,2]*max(1/mean_rank_long_5[,2])), names.arg = mean_rank_long_5[,1])

### Poznamka >> iba obycajny priemer bez rankovania nedava zmysel, pretoze hodnoty su velmi podobne


# pocet nachadzani sa v MSC 

load(file = "long_mcs_list.Rdata")
names(long_mcs_list)
grep("_05",names(long_mcs_list))
long_mcs_list[grep("_05",names(long_mcs_list))]
rownames(long_mcs_list$`43_05`@show)


long_model <- c(paste0("model_",1:28))
long_model_new <- c()
for( i in grep("_05",names(long_mcs_list))){

  long_model_new <- cbind(long_model_new,long_model%in%rownames(long_mcs_list[[i]]@show))

}

rowSums(long_model_new)



long_model <- c(paste0("model_",1:28))
long_model_new <- c()
for( i in grep("_05",names(long_mcs_list))){
  na_vec <- rep(NA,28)
  na_vec[(long_model%in%rownames(long_mcs_list[[i]]@show))] <- long_mcs_list[[i]]@show[,"Rank_R"]
  long_model_new <- cbind(long_model_new,na_vec)

}

mean_rank <- apply(long_model_new,1, function(x){
  mean(x, na.rm = TRUE)
})


mean_rank <- mean_rank[c(c(1,8,15,22),
           3 + c(1,8,15,22),
           4 + c(1,8,15,22),
           5 + c(1,8,15,22),
           2 + c(1,8,15,22),
           6 + c(1,8,15,22),
           1 + c(1,8,15,22))]
names(mean_rank) <-vol_nazvy




barplot(1/(mean_rank*max(1/mean_rank)), names.arg = names(mean_rank))
