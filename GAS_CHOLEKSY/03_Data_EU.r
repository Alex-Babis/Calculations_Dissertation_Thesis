# SPOTOVE CENY PLYNU 
ttf <- read_xlsx("Libraries_data/Data/SPOT_TTF.xlsx",
col_types = c("date", "numeric"))

colnames(ttf) <- c("Date", "ttf")
ttf$Date <- as.Date(ttf$Date)


# SPOTOVE CENY ELEKTRINY

ee <- read_xlsx("Libraries_data/Data/DAY_EUROPE_AHEAD.xlsx",
col_types = c("date", rep("numeric",10)))

colnames(ee)[1] <- "Date"
ee$Date <- as.Date(ee$Date)

ee <- ee |> group_by(Date) |> summarise(AUSTRA = mean(AUSTRIA),BELGIUM = mean(BELGIUM),CZECH = mean(CZECH),DENMARK = mean(DENMARK),GERMANY = mean(GERMANY),HUNGARY = mean(HUNGARY),NETHERLANDS = mean(NETHERLANDS),POLAND = mean(POLAND),SLOVAKIA = mean(SLOVAKIA),SLOVENIA = mean(SLOVENIA))
ee <- as.data.frame(ee)


# YIELD RATES Czech
cech <- read.csv("Libraries_data/Data/CZK_10.csv",
                header = TRUE, sep = ",")

cech <- cech[,c(1,2)]           
colnames(cech) <- c("Date", "cech")

cech$cech <- as.numeric(gsub(",", "", cech$cech))

cech$Date <- as.Date(cech$Date, format = "%m/%d/%Y")

# YIELD RATES AUS
aust <- read.csv("Libraries_data/Data/AUS_10.csv",
                header = TRUE, sep = ",")

aust <- aust[,c(1,2)]           
colnames(aust) <- c("Date", "aust")

aust$aust <- as.numeric(gsub(",", "", aust$aust))

aust$Date <- as.Date(aust$Date, format = "%m/%d/%Y")

aust[1223,2] <- mean(aust[c(1222,1224),2])

# YIELD RATES SVK
svk <- read.csv("Libraries_data/Data/SVK_10.csv",
                header = TRUE, sep = ",")

svk <- svk[,c(1,2)]           
colnames(svk) <- c("Date", "svk")

svk$svk <- as.numeric(gsub(",", "", svk$svk))

svk$Date <- as.Date(svk$Date, format = "%m/%d/%Y")

# YIELD RATES pol
pol <- read.csv("Libraries_data/Data/POL_10.csv",
                header = TRUE, sep = ",")

pol <- pol[,c(1,2)]           
colnames(pol) <- c("Date", "pol")

pol$pol <- as.numeric(gsub(",", "", pol$pol))

pol$Date <- as.Date(pol$Date, format = "%m/%d/%Y")

# YIELD RATES HUN
hun <- read.csv("Libraries_data/Data/HUN_10.csv",
                header = TRUE, sep = ",")

hun <- hun[,c(1,2)]           
colnames(hun) <- c("Date", "hun")

hun$hun <- as.numeric(gsub(",", "", hun$hun))

hun$Date <- as.Date(hun$Date, format = "%m/%d/%Y")

# YIELD RATES SWI
swis <- read.csv("Libraries_data/Data/SWI_10.csv",
                header = TRUE, sep = ",")

swis <- swis[,c(1,2)]           
colnames(swis) <- c("Date", "swis")

swis$swis <- as.numeric(gsub(",", "", swis$swis))

swis$Date <- as.Date(swis$Date, format = "%m/%d/%Y")

# YIELD RATES GER
ger <- read.csv("Libraries_data/Data/GER_10.csv",
                header = TRUE, sep = ",")

ger <- ger[,c(1,2)]           
colnames(ger) <- c("Date", "ger")

ger$ger <- as.numeric(gsub(",", "", ger$ger))

ger$Date <- as.Date(ger$Date, format = "%m/%d/%Y")

# YIELD RATES SLO
slo <- read.csv("Libraries_data/Data/SLO_10.csv",
                header = TRUE, sep = ",")

slo <- slo[,c(1,2)]           
colnames(slo) <- c("Date", "slo")

slo$slo <- as.numeric(gsub(",", "", slo$slo))

slo$Date <- as.Date(slo$Date, format = "%m/%d/%Y")


slo$slo[3940] <- mean(slo[c(3939,3941),2])



df_list <- c("aust","svk", "pol",
             "hun","swis","ger",
             "slo")      


data_all <- cech

for(i in df_list){
    data_all <- merge(data_all,eval(parse(text = i)), by = "Date", suffixes =c("",""))
}

colnames(data_all) <- c("Date","cech", "aust","svk", "pol",
             "hun","swis","ger","slo")


data_new <- data_all[data_all$Date >= "2019-03-01" & data_all$Date < "2024-06-01" ,]

#data_new <- data_all[weekdays(data_all$Date) == "Monday",]
# data_new <- asinh(data_new[,2:9])
data_new <- data_new[,2:9]
returns <- apply(data_new, 2,diff, simplify = TRUE)




# DIM NOW 1219    8
# DIM NEW 1256    8
# O 37 viac pozorovani... to znamena ze likelihood zacneme optimalizovat az po 37 pozorovani a odvtedy si budeme ukladat aj parametre rozdelenia

returns <- returns[,c("cech", "aust","svk", "pol",
             "hun","swis","ger","slo")]

colnames(returns) <- c("CZE","AUS" ,"SVK","POL","HUN","SWI","GER","SLO")

M = cor(returns)

# png(filename = "Graphs/Corplot.png", width = 1400, height = 1400, res = 300)

# cor_plot <- corrplot(M, is.corr = FALSE, col.lim = c(0, 1),method = 'color', order = 'AOE', tl.col = 'black', tl.cex = 1,
#          col = COL2('PRGn'), cl.pos = 'b', addgrid.col = 'white', addCoef.col = 'black')
# dev.off()


cor_mat_1 <- c()
head(returns)
for( i in 1:ncol(returns)){
    plot(returns[,i], type ='l', main = i)
    cor_mat_1 <- rbind(cor_mat_1,
                       cor(cbind(returns[2:nrow(returns),i],returns[2:nrow(returns)-1,]))[1,])
}

rownames(cor_mat_1) <- c("cech", "aust","svk", "pol",
             "hun","swis","ger","slo")













# #data_all[,2:11] <- data_all[,2:11] - data_all$ger

# data_all <- merge(data_all,ttf, by = "Date", suffixes =c("",""))
# data_all <- merge(data_all,ee, by = "Date", suffixes =c("",""))
# head(data_all)




# stocks5 <- c("^CXPAX","^CXPBX","^CXPCX","^CXPVX","^CXPNX","^CXPIX","^CXPPX","^CXPRX","^CXPSX","^CXPHX","^CXPTX","^CXPLX","^CXPUX","CZKEUR=X","HUFEUR=X","PLNEUR=X")

# getSymbols(stocks5, from = "2010-01-01", to = "2024-11-03", auto.assign = TRUE)

# ex_rate <- merge( `CXPAX`$`CXPAX.Adjusted`,
#                   `CXPBX`$`CXPBX.Adjusted`,
#                   `CXPCX`$`CXPCX.Adjusted`,
#                   `CXPVX`$`CXPVX.Adjusted`,
#                   `CXPNX`$`CXPNX.Adjusted`,
#                   `CXPIX`$`CXPIX.Adjusted`,
#                   `CXPPX`$`CXPPX.Adjusted`,
#                   `CXPRX`$`CXPRX.Adjusted`,
#                   `CXPSX`$`CXPSX.Adjusted`,
#                   `CXPHX`$`CXPHX.Adjusted`,
#                   `CXPTX`$`CXPTX.Adjusted`,
#                   `CXPLX`$`CXPLX.Adjusted`,
#                   `CXPUX`$`CXPUX.Adjusted`,
#                   `CZKEUR=X`$`CZKEUR=X.Adjusted`,
#                   `HUFEUR=X`$`HUFEUR=X.Adjusted`,
#                   `PLNEUR=X`$`PLNEUR=X.Adjusted`)

# colnames(ex_rate) <- c("Auto","Bank","Chem","Fina","Indy","Insur","Pharma","Retail","Softw","Tech","Tele","Transport","Util","CZK","HUF","PLN")
# ex_rate$Date <- index(ex_rate)

# ex_new <- as.data.frame(ex_rate)
# ex_new$Date <- as.Date(index(ex_rate))

# data_all <- merge(data_all,ex_new, by = "Date", suffixes =c("",""))


# data_all <- data_all[complete.cases(data_all),]


# # data_new <- data_all[weekdays(data_all$date) == "Monday",]
# # logarithms of stock indexes

# data_new <- data_all[data_all$Date >= "2019-01-01" & data_all$Date < "2024-01-01",]
# # data_new <- data_all[data_all$date >= "2014-01-01" ,]
# # data_new <- data_new[weekdays(data_new$date) == "Monday",]

# data_new[,-c(1:24)] <- log(data_new[,-c(1:24)])
# data_new[,2:24] <- asinh(data_new[,2:24])
# #data_all <- data_all[data_all$Date >= "2020-01-01" & data_all$Date < "2024-01-01" ,]

# returns <- apply(data_new[,-1], 2,diff, simplify = TRUE)


# returns <- returns[,c("cech", "aust","svk", "pol",
#              "hun","slo","bel","neth","ttf",colnames(ee)[-1], "Auto","Bank","Chem","Fina","Indy","Insur","Pharma","Retail","Softw","Tech","Tele","Transport","Util","CZK","HUF","PLN")]

# M = cor(returns)
# corrplot(M, method = 'number') # colorful number

# cor_mat_1 <- c()
# head(returns)
# for( i in 1:ncol(returns)){
#     plot(returns[,i], type ='l', main = i)
#     cor_mat_1 <- rbind(cor_mat_1,
#                        cor(cbind(returns[2:nrow(returns),i],returns[2:nrow(returns)-1,]))[1,])
# }

# rownames(cor_mat_1) <- c("cech", "aust","svk", "pol",
#              "hun","slo","bel","neth", "ttf",colnames(ee)[-1],"Auto","Bank","Chem","Fina","Indy","Insur","Pharma","Retail","Softw","Tech","Tele","Transport","Util","CZK","HUF","PLN")




