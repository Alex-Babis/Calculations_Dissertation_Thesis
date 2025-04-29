# Metric function

metrics_fun <- function(df, met = "mse", par_cnt = NULL){

 
    if(met == "llh"){

    llh <- -rowSums(df, na.rm = TRUE)

    bic <- par_cnt*log(nrow(df)) + 2*llh[length(llh)]

    metrics <- c(llh,bic)


    }else{
        mse <- rowSums((df[1:9,]-df[10:18,])^2, na.rm = TRUE)/ncol(df)
        rmse <- sqrt(rowSums((df[1:9,]-df[10:18,])^2, na.rm = TRUE)/ncol(df))

        mae <- rowSums(abs(df[1:9,]-df[10:18,]), na.rm = TRUE)/ncol(df)
    
        # bude euklidovska vzdialenost
        mse_all <-sum((df[1:9,]-df[10:18,])^2, na.rm = TRUE)/(ncol(df)*nrow(df))
        mae_all <- 100*(sum(abs(df[1:9,]-df[10:18,]), na.rm = TRUE)/(ncol(df)*nrow(df)))

        eu_all <- mean(sqrt(colSums((df[1:9,]-df[10:18,])^2, na.rm = TRUE)))



        metrics <- cbind(rbind(mse, mae),c(eu_all,mae_all))
        rownames(metrics) <-  c("mse", "mae")
        
        if(met == "mse"){
            metrics <-  metrics[rownames(metrics) == "mse",]
        }else if (met == "mae") {
            metrics <-  metrics[rownames(metrics) == "mae",]
        }

    }
    

    return(metrics)

}




### TABLES 
Model_name <- c("NS","NSS","BL")

#Model_dist <- c("norm","t")
Model_dist <- c("norm","t")

Model_scale <- c("inv","sqrt")

Model_spec <- c("b", "L", "cov")

# MSE on forecasted values
Met_all_forecast <- c()

# MSE on filterd values
Met_all_filter <- c()

# LLH on forecasted values
LLH_all_forecast <- c()

# LLH on filterd values
LLH_all_filter <- c()

# Filtered parameters on in-sample values
Param_all_forecast <- list()

# Filtered parameters on out-sample values
Param_all_filter <- list()

# MATRIX OF BETA1
Beta1 <- c()

# MATRIX OF BETA2
Beta2 <- c()

# MATRIX OF BETA3
Beta3 <- c()

# MATRIX OF BETA4
Beta4 <- c()

# MATRIX OF Lambda1
Lambda1 <- c()

# MATRIX OF Lambda2
Lambda2 <- c()

# MATRIX OF Lambda2
Tv_sigma <- c()

# nu 
nu <- c()

# counting of tables
cnt <- 1

for(m_dist in Model_dist){

    for(m_name in Model_name){

         for(m_spec in Model_spec){

            for(m_scale in Model_scale){

                m_type <- paste0(m_name,"_",m_dist,"_",m_scale,"_",m_spec,"_for")
                
                model <- get(m_type)
                model$par_cnt
                # MSE

                Met_all_forecast <- rbind(Met_all_forecast, metrics_fun(model$values[,897:976], met = "mse"))

                rownames(Met_all_forecast)[cnt] <- m_type

                Met_all_filter <- rbind(Met_all_filter, metrics_fun(model$values[,1:896], met = "mse"))

                rownames(Met_all_filter)[cnt] <- m_type

                # LLH

                LLH_all_forecast <- rbind(LLH_all_forecast, metrics_fun(model$llh[,897:976], met = "llh", par_cnt = model$par_cnt))
                
                rownames(LLH_all_forecast)[cnt] <- m_type

                LLH_all_filter <- rbind(LLH_all_filter, metrics_fun(model$llh[,1:896], met = "llh",par_cnt = model$par_cnt))

                rownames(LLH_all_filter)[cnt] <- m_type


                Param_all_forecast <- c(Param_all_forecast, model$pars[,897:976])

                names(Param_all_forecast)[cnt] <- m_type

                Param_all_filter <- c(Param_all_filter, model$pars[,1:897])

                names(Param_all_filter)[cnt] <- m_type

                nu <- c(nu,model$nu)
                names(nu)[cnt] <- m_type

                if(sum( rownames(model$pars) == "beta1") == 1){
                    Beta1 <- rbind(Beta1, model$pars[rownames(model$pars) == "beta1",])
                    rownames(Beta1)[nrow(Beta1)] <- m_type
                }

                if(sum( rownames(model$pars) == "beta2") == 1){
                    Beta2 <- rbind(Beta2, model$pars[rownames(model$pars) == "beta2",])
                    rownames(Beta2)[nrow(Beta2)] <- m_type
                }

                if(sum( rownames(model$pars) == "beta3") == 1){
                    Beta3 <- rbind(Beta3, model$pars[rownames(model$pars) == "beta3",])
                    rownames(Beta3)[nrow(Beta3)] <- m_type
                }

                if(sum( rownames(model$pars) == "beta4") == 1){
                    Beta4 <- rbind(Beta4, model$pars[rownames(model$pars) == "beta4",])
                    rownames(Beta4)[nrow(Beta4)] <- m_type
                }

                if(sum( rownames(model$pars) == "lambda1") == 1){
                    Lambda1 <- rbind(Lambda1, model$pars[rownames(model$pars) == "lambda1",])
                    rownames(Lambda1)[nrow(Lambda1)] <- m_type
                }

                if(sum( rownames(model$pars) == "lambda2") == 1){
                    Lambda2 <- rbind(Lambda2, model$pars[rownames(model$pars) == "lambda2",])
                    rownames(Lambda2)[nrow(Lambda2)] <- m_type
                }

                if(sum( rownames(model$pars) == "tv_sigma") == 1){
                    Tv_sigma <- rbind(Tv_sigma, model$pars[rownames(model$pars) == "tv_sigma",])
                    rownames(Tv_sigma)[nrow(Tv_sigma)] <- m_type
                }



                cnt <- cnt + 1

            }
        }
    }
}

# NSS_norm_inv_b_for NSS_t_inv_b_for


View(Met_all_forecast[grepl("NSS_",rownames(Met_all_forecast)),])
View(Met_all_filter[grepl("BL_",rownames(Met_all_filter)),])

View(LLH_all_forecast[grepl("NSS_",rownames(LLH_all_forecast)),])
View(LLH_all_filter[grepl("BL_",rownames(LLH_all_filter)),])

LLH_all_forecast <- round(LLH_all_forecast, digits = 2)
LLH_all_forecast <- LLH_all_forecast[,-ncol(LLH_all_forecast)]
rownames(LLH_all_forecast) <- gsub("_for","",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("_","-",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("-t-","-T-",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("inv","I",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("norm","N",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("inv","I",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("sqrt","S",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("cov","C",rownames(LLH_all_forecast) )
rownames(LLH_all_forecast) <- gsub("-b","-B",rownames(LLH_all_forecast) )



colnames(LLH_all_forecast) <- c("3M", "6M", "12M","36M","60M","84M","120M","240M","360M","All")

LLH_all_filter <- round(LLH_all_filter, digits = 2)
rownames(LLH_all_filter) <- gsub("_for","",rownames(LLH_all_filter) )
colnames(LLH_all_filter) <- c("3M", "6M", "12M","36M","60M","84M","120M","240M","360M","All","BIC")

rownames(LLH_all_filter) <- gsub("_for","",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("_","-",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("-t-","-T-",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("inv","I",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("norm","N",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("inv","I",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("sqrt","S",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("cov","C",rownames(LLH_all_filter) )
rownames(LLH_all_filter) <- gsub("-b","-B",rownames(LLH_all_filter) )

View(LLH_all_filter)
rownames(LLH_all_filter)[apply(LLH_all_filter,2, which.min)]
rownames(LLH_all_forecast)[apply(LLH_all_forecast,2, which.min)]

Hmisc::latex(LLH_all_filter, file = '', caption = "Filtering period")

Hmisc::latex(LLH_all_forecast, file = '', caption = "Forcasting period")


View(Met_all_forecast)
Met_all_forecast <- round(Met_all_forecast, digits = 5)
rownames(Met_all_forecast) <- gsub("_for","",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("_","-",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("-t-","-T-",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("inv","I",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("norm","N",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("inv","I",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("sqrt","S",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("cov","C",rownames(Met_all_forecast) )
rownames(Met_all_forecast) <- gsub("-b","-B",rownames(Met_all_forecast) )



colnames(Met_all_forecast) <- c("3M", "6M", "12M","36M","60M","84M","120M","240M","360M","All")
Hmisc::latex(Met_all_forecast, file = '', caption = "Forcasting period")

rownames(Met_all_forecast)[apply(Met_all_forecast,2, which.min)]

nu <- nu[nu > 0]
View(nu)
#### GRAFY 

graph_pars <- function(mat, out_name ){


    if(nrow(mat) == 6){
            rownames(mat) <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
            lev_names   <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")

                cc <- c("#8531d3","#B194CD","#b80357","#be7195","#00897D","#89dad3")

    }else if(nrow(mat) == 4){
        rownames(mat) <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        cc <- c("#b80357","#be7195","#00897D","#89dad3")
    }else{
        rownames(mat) <-c("Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Cov-Inv","Cov-sqrt")
        cc <- c("#00897D","#89dad3")
        }

    df <- as.data.frame(t(mat))
    df$time <- as.Date(all_dates[-1])

    #cc <- scales::seq_gradient_pal("#00897D", "#B194CD", "Lab")(seq(0,1,length.out=ncol(df)))


    long_beta <- df %>% pivot_longer(-time,values_to = "Value",names_to = "Models")

    long_beta$Models <- factor(long_beta$Models, levels = lev_names)
    
    p1 <- ggplot(long_beta, aes(x = time, y = Value, group = Models, color= Models))+ geom_line()+geom_point(size =0)+
    scale_colour_manual(values=cc) + 
    geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dashed",alpha=0.6)+
    geom_vline(data = data.frame(xint=19358), aes(xintercept = xint), linetype = "dashed",alpha=0.6) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5, size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        legend.text = element_text(size =25),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position="bottom" )+ 
    scale_x_date(breaks = scales::breaks_pretty(20))+
    labs(y = "Value in %", x = "")+ guides(colour = guide_legend(nrow = 1,override.aes= list(size = 15,shape = 15)))
  

    # ggplot2::ggsave(paste0("Graphs/",out_name,".png"),p1,width = 2400,
    #             height = 1200,units = "px",
    #             scale = 2,
    #             dpi = 300)
    
    return(p1)
}


# NS 

graph_pars(Beta1[grepl("NS_t",rownames(Beta1)),], out_name = "NS_t_beta1")
graph_pars(Beta2[grepl("NS_t",rownames(Beta2)),], out_name = "NS_t_beta2")
graph_pars(Beta3[grepl("NS_t",rownames(Beta3)),], out_name = "NS_t_beta3")

graph_pars(Beta1[grepl("NS_norm",rownames(Beta1)),],out_name = "NS_norm_beta1")
graph_pars(Beta2[grepl("NS_norm",rownames(Beta2)),],out_name = "NS_norm_beta2")
graph_pars(Beta3[grepl("NS_norm",rownames(Beta3)),],out_name = "NS_norm_beta3")

graph_pars(Lambda1[grepl("NS_norm",rownames(Lambda1)),],out_name = "NS_norm_lambda")
graph_pars(Lambda1[grepl("NS_t",rownames(Lambda1)),],out_name = "NS_t_lambda")

graph_pars(Tv_sigma[grepl("NS_norm",rownames(Tv_sigma)),],out_name = "NS_norm_cov")
graph_pars(Tv_sigma[grepl("NS_t",rownames(Tv_sigma)),],out_name = "NS_t_cov")

# NSS 

graph_pars(Beta1[grepl("NSS_norm",rownames(Beta1)),], out_name = "NSS_norm_beta1")
graph_pars(Beta2[grepl("NSS_norm",rownames(Beta2)),], out_name = "NSS_norm_beta2")
graph_pars(Beta3[grepl("NSS_norm",rownames(Beta3)),], out_name = "NSS_norm_beta3")
graph_pars(Beta4[grepl("NSS_norm",rownames(Beta4)),], out_name = "NSS_norm_beta4")

graph_pars(Beta1[grepl("NSS_t",rownames(Beta1)),], out_name = "NSS_t_beta1")
graph_pars(Beta2[grepl("NSS_t",rownames(Beta2)),], out_name = "NSS_t_beta2")
graph_pars(Beta3[grepl("NSS_t",rownames(Beta3)),], out_name = "NSS_t_beta3")
graph_pars(Beta4[grepl("NSS_t",rownames(Beta4)),], out_name = "NSS_t_beta4")

graph_pars(Lambda1[grepl("NSS_norm",rownames(Lambda1)),],out_name = "NSS_norm_lambda1")
graph_pars(Lambda2[grepl("NSS_norm",rownames(Lambda2)),],out_name = "NSS_norm_lambda2")

graph_pars(Lambda1[grepl("NSS_t",rownames(Lambda1)),],out_name = "NSS_t_lambda1")
graph_pars(Lambda2[grepl("NSS_t",rownames(Lambda2)),],out_name = "NSS_t_lambda2")

graph_pars(Tv_sigma[grepl("NSS_norm",rownames(Tv_sigma)),],out_name = "NSS_norm_cov")
graph_pars(Tv_sigma[grepl("NSS_t",rownames(Tv_sigma)),],out_name = "NSS_t_cov")
# BL 

graph_pars(Beta1[grepl("BL_t",rownames(Beta1)),], out_name = "BL_t_beta1")
graph_pars(Beta2[grepl("BL_t",rownames(Beta2)),], out_name = "BL_t_beta2")
graph_pars(Beta3[grepl("BL_t",rownames(Beta3)),], out_name = "BL_t_beta3")

graph_pars(Beta1[grepl("BL_norm",rownames(Beta1)),],out_name = "BL_norm_beta1")
graph_pars(Beta2[grepl("BL_norm",rownames(Beta2)),],out_name = "BL_norm_beta2")
graph_pars(Beta3[grepl("BL_norm",rownames(Beta3)),],out_name = "BL_norm_beta3")

graph_pars(Lambda1[grepl("BL_norm",rownames(Lambda1)),],out_name = "BL_norm_lambda1")
graph_pars(Lambda1[grepl("BL_t",rownames(Lambda1)),],out_name = "BL_t_lambda1")

graph_pars(Lambda2[grepl("BL_norm",rownames(Lambda2)),],out_name = "BL_norm_lambda2")
graph_pars(Lambda2[grepl("BL_t",rownames(Lambda2)),],out_name = "BL_t_lambda2")

graph_pars(Tv_sigma[grepl("BL_norm",rownames(Tv_sigma)),],out_name = "BL_norm_cov")
graph_pars(Tv_sigma[grepl("BL_t",rownames(Tv_sigma)),],out_name = "BL_t_cov")



# GRAPH FUNCTION THAT DISPLAY ALL MODELS AS 2 GRAPH NEXT TO EACH OTHER
# FOR BOTH DISTRIBUTION SEPARATELY TO BE ABLE TO COMPARE THE RESULTS

graph_pars2 <- function(mat1,mat2,name1, name2, out_name ){


    if(nrow(mat1) == 6){
            rownames(mat1) <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
            lev_names   <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")

                cc <- c("#8531d3","#B194CD","#b80357","#be7195","#00897D","#89dad3")

    }else if(nrow(mat1) == 4){
        rownames(mat1) <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        cc <- c("#b80357","#be7195","#00897D","#89dad3")
    }else{
        rownames(mat1) <-c("Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Cov-Inv","Cov-sqrt")
        cc <- c("#00897D","#89dad3")
        }

    df1 <- as.data.frame(t(mat1))
    df1$time <- as.Date(all_dates[-1])

    if(nrow(mat2) == 6){
            rownames(mat2) <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
            lev_names   <-  c("Base-Inv", "Base-sqrt","Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")

                cc <- c("#8531d3","#B194CD","#b80357","#be7195","#00897D","#89dad3")

    }else if(nrow(mat2) == 4){
        rownames(mat2) <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Lambda-Inv","Lambda-sqrt","Cov-Inv","Cov-sqrt")
        cc <- c("#b80357","#be7195","#00897D","#89dad3")
    }else{
        rownames(mat2) <-c("Cov-Inv","Cov-sqrt")
        lev_names  <-  c("Cov-Inv","Cov-sqrt")
        cc <- c("#00897D","#89dad3")
        }

    df2 <- as.data.frame(t(mat2))
    df2$time <- as.Date(all_dates[-1])


    df = dplyr::bind_rows(list(data1 = df1, data2 = df2), .id = "Model")

    if( name1 == name2){
        df$Model[df$Model == "data1"] <- "Beta4"
        df$Model[df$Model == "data2"] <- "Lambda2"

        cc <- c("#8531d3","#B194CD","#b80357","#be7195","#00897D","#89dad3")


    }else{
        df$Model[df$Model == "data1"] <- name1
        df$Model[df$Model == "data2"] <- name2
    }

    #cc <- scales::seq_gradient_pal("#00897D", "#B194CD", "Lab")(seq(0,1,length.out=ncol(df)))


    long_beta <- df %>% pivot_longer(-c(time,Model),values_to = "Value",names_to = "Models")

    long_beta$Models <- factor(long_beta$Models, levels = lev_names)
    
    p1 <- ggplot(long_beta, aes(x = time, y = Value, group = Models, color= Models))+ geom_line()+geom_point(size =0)+
    scale_colour_manual(values=cc) + 
    geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dashed",alpha=0.6)+
    geom_vline(data = data.frame(xint=19358), aes(xintercept = xint), linetype = "dashed",alpha=0.6) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),
        legend.text = element_text(size =12),
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position="bottom" )+ 
    scale_x_date(breaks = scales::breaks_pretty(20))+
    labs(y = "Value in %", x = "")+ guides(colour = guide_legend(nrow = 1,override.aes= list(size = 15,shape = 15)))+facet_wrap( ~ Model)


    ggplot2::ggsave(paste0("Graphs/new_graphs/",out_name,".png"),p1,dpi = 300, width = 7, height = 5)
    
    return(0)
}

mat1 <- Beta4[grepl("NSS_norm",rownames(Beta4)),]
mat2 <- Lambda2[grepl("NSS_norm",rownames(Lambda2)),]
name1 = "NSS"
name2 = "NSS"


# NS A NSS NORM

graph_pars2(Beta1[grepl("NS_norm",rownames(Beta1)),],
            Beta1[grepl("NSS_norm",rownames(Beta1)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_NORM_BETA1")

graph_pars2(Beta2[grepl("NS_norm",rownames(Beta2)),],
            Beta2[grepl("NSS_norm",rownames(Beta2)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_NORM_BETA2")

graph_pars2(Beta3[grepl("NS_norm",rownames(Beta3)),],
            Beta3[grepl("NSS_norm",rownames(Beta3)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_NORM_BETA3")

graph_pars2(Lambda1[grepl("NS_norm",rownames(Lambda1)),],
            Lambda1[grepl("NSS_norm",rownames(Lambda1)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_NORM_Lambda1")

graph_pars2(Tv_sigma[grepl("NS_norm",rownames(Tv_sigma)),],
            Tv_sigma[grepl("NSS_norm",rownames(Tv_sigma)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_NORM_Tv_sigma")

# NS A NSS T
graph_pars2(Beta1[grepl("NS_t",rownames(Beta1)),],
            Beta1[grepl("NSS_t",rownames(Beta1)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_t_BETA1")

graph_pars2(Beta2[grepl("NS_t",rownames(Beta2)),],
            Beta2[grepl("NSS_t",rownames(Beta2)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_t_BETA2")

graph_pars2(Beta3[grepl("NS_t",rownames(Beta3)),],
            Beta3[grepl("NSS_t",rownames(Beta3)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_t_BETA3")

graph_pars2(Lambda1[grepl("NS_t",rownames(Lambda1)),],
            Lambda1[grepl("NSS_t",rownames(Lambda1)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_t_Lambda1")

graph_pars2(Beta4[grepl("NSS_t",rownames(Beta4)),],
            Lambda2[grepl("NSS_t",rownames(Lambda2)),],
            name1 = "NSS", name2 = "NSS", out_name = "NSS_t_BETA4_Lambda2")

graph_pars2(Tv_sigma[grepl("NS_t",rownames(Tv_sigma)),],
            Tv_sigma[grepl("NSS_t",rownames(Tv_sigma)),],
            name1 = "NS", name2 = "NSS", out_name = "NS_NSS_t_Tv_sigma")


# NSS 4
graph_pars2(Beta4[grepl("NSS_norm",rownames(Beta4)),],
            Beta4[grepl("NSS_t",rownames(Beta4)),],
            name1 = "NSS_norm", name2 = "NSS_t", out_name = "NSS_NORM_T_BETA4")

graph_pars2(Lambda2[grepl("NSS_norm",rownames(Lambda2)),],
            Lambda2[grepl("NSS_t",rownames(Lambda2)),],
            name1 = "NSS_norm", name2 = "NSS_t", out_name = "NSS_NORM_T_Lambda2")

# BL
graph_pars2(Beta1[grepl("BL_norm",rownames(Beta1)),],
            Beta1[grepl("BL_t",rownames(Beta1)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_BETA1")

graph_pars2(Beta2[grepl("BL_norm",rownames(Beta2)),],
            Beta2[grepl("BL_t",rownames(Beta2)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_BETA2")


graph_pars2(Beta3[grepl("BL_norm",rownames(Beta3)),],
            Beta3[grepl("BL_t",rownames(Beta3)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_BETA3")

graph_pars2(Lambda1[grepl("BL_norm",rownames(Lambda1)),],
            Lambda1[grepl("BL_t",rownames(Lambda1)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_LAMBDA1")


graph_pars2(Lambda2[grepl("BL_norm",rownames(Lambda2)),],
            Lambda2[grepl("BL_t",rownames(Lambda2)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_LAMBDA2")



graph_pars2(Tv_sigma[grepl("BL_norm",rownames(Tv_sigma)),],
            Tv_sigma[grepl("BL_t",rownames(Tv_sigma)),],
            name1 = "BL_norm", name2 = "BL_t", out_name = "BL_TV_SIGMA")


### GRAPHS pre Filtered data from best model which is NSS-T-I-C

data_NSS_t <- data.frame(

time = as.Date(all_dates[-1]),

Beta1 = Beta1[grepl("NSS_t_inv_cov",rownames(Beta1)),],
Beta2 = Beta2[grepl("NSS_t_inv_cov",rownames(Beta2)),],
Beta3 = Beta3[grepl("NSS_t_inv_cov",rownames(Beta3)),],
Beta4 = Beta4[grepl("NSS_t_inv_cov",rownames(Beta4)),],

Lambda1 = Lambda1[grepl("NSS_t_inv_cov",rownames(Lambda1)),],
Lambda2 = Lambda2[grepl("NSS_t_inv_cov",rownames(Lambda2)),],

Tv_sigma = Tv_sigma[grepl("NSS_t_inv_cov",rownames(Tv_sigma)),] )
head(data_NSS_t)

long_beta <- data_NSS_t[,c("time","Beta1","Beta2","Beta3","Beta4")] |> pivot_longer(-time, values_to = "Beta", names_to = "Factors")

p_beta <- long_beta |> ggplot(aes(x = time, y = Beta, group = Factors, linetype  = Factors)) + geom_line()+ 
    geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dashed",alpha=0.6)+
    geom_vline(data = data.frame(xint=19358), aes(xintercept = xint), linetype = "dashed",alpha=0.6) +
    theme_bw()+
    scale_linetype_manual(values=c(1,2,3,4))+
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),legend.position="none" )+ 
    labs(y = "Value in %", x = "")+ guides(colour = guide_legend(nrow = 1,override.aes= list(size = 12,shape = 15)))


long_lambda <-  data_NSS_t[,c("time","Lambda1","Lambda2")] |> pivot_longer(-time, values_to = "Lambda", names_to = "Factors")


p_lambda <- long_lambda |> ggplot(aes(x = time, y = Lambda, group = Factors, linetype  = Factors)) + geom_line()+ 
    geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dashed",alpha=0.6)+
    geom_vline(data = data.frame(xint=19358), aes(xintercept = xint), linetype = "dashed",alpha=0.6) +
    theme_bw()+
    scale_linetype_manual(values=c(1,3))+
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank(),legend.position="none" )+ 
    labs(y = "Value in %", x = "")+ guides(colour = guide_legend(nrow = 1,override.aes= list(size = 12,shape = 15)))


p_vol <- data_NSS_t |> ggplot(aes(x = time, y = Tv_sigma)) + geom_line()+ 
    geom_hline(data = data.frame(yint=0), aes(yintercept = yint), linetype = "dashed",alpha=0.6)+
    geom_vline(data = data.frame(xint=19358), aes(xintercept = xint), linetype = "dashed",alpha=0.6) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_blank() )+ 
    labs(y = "Value in %", x = "")+ guides(colour = guide_legend(nrow = 1,override.aes= list(size = 12,shape = 15)))


ggplot2::ggsave(paste0("Graphs_new/BETA_FILTERED.png"),p_beta,dpi = 300, width = 7, height = 5)

ggplot2::ggsave(paste0("Graphs_new/LAMBDA_FILTERED.png"),p_lambda,dpi = 300, width = 7, height = 5)

ggplot2::ggsave(paste0("Graphs_new/VOLATILITY_FILTERED.png"),p_vol,dpi = 300, width = 7, height = 5)
