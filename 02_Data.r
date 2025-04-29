# loading data about Slovak yield rates

ger_1Y <- read.csv("Data_libraries/Germany1Y.csv", header = TRUE,sep = ",")
ger_3Y <- read.csv("Data_libraries/Germany3Y.csv", header = TRUE,sep = ",")
ger_5Y <- read.csv("Data_libraries/Germany5Y.csv", header = TRUE,sep = ",")
ger_7Y <- read.csv("Data_libraries/Germany7Y.csv", header = TRUE,sep = ",")
ger_10Y <- read.csv("Data_libraries/Germany10Y.csv", header = TRUE,sep = ",")
ger_20Y <- read.csv("Data_libraries/Germany20Y.csv", header = TRUE,sep = ",")
ger_30Y <- read.csv("Data_libraries/Germany30Y.csv", header = TRUE,sep = ",")
ger_3M <- read.csv("Data_libraries/Germany3M.csv", header = TRUE,sep = ",")
ger_6M <- read.csv("Data_libraries/Germany6M.csv", header = TRUE,sep = ",")


### try to pair to current values
ger_1Y_new <- read.csv("yieal1Y.csv", header = TRUE,sep = ",")
ger_3Y_new <- read.csv("yieal3Y.csv", header = TRUE,sep = ",")
ger_5Y_new <- read.csv("yieal5Y.csv", header = TRUE,sep = ",")
ger_7Y_new <- read.csv("yieal7Y.csv", header = TRUE,sep = ",")
ger_10Y_new <- read.csv("yieal10Y.csv", header = TRUE,sep = ",")
ger_20Y_new <- read.csv("yieal20Y.csv", header = TRUE,sep = ",")
ger_30Y_new <- read.csv("yieal30Y.csv", header = TRUE,sep = ",")
ger_3M_new <- read.csv("yieal3M.csv", header = TRUE,sep = ",")
ger_6M_new <- read.csv("yieal6M.csv", header = TRUE,sep = ",")

ger_1Y_new <- ger_1Y_new[,c("date","close")]
ger_3Y_new <- ger_3Y_new[,c("date","close")]
ger_5Y_new <- ger_5Y_new[,c("date","close")]
ger_7Y_new <- ger_7Y_new[,c("date","close")]
ger_10Y_new <- ger_10Y_new[,c("date","close")]
ger_20Y_new <- ger_20Y_new[,c("date","close")]
ger_30Y_new <- ger_30Y_new[,c("date","close")]
ger_3M_new <- ger_3M_new[,c("date","close")]
ger_6M_new <- ger_6M_new[,c("date","close")]

ger_1Y_new$date <- as.Date(ger_1Y_new$date, format = "%m/%d/%Y")
ger_3Y_new$date <- as.Date(ger_3Y_new$date, format = "%m/%d/%Y")
ger_5Y_new$date <- as.Date(ger_5Y_new$date, format = "%m/%d/%Y")
ger_7Y_new$date <- as.Date(ger_7Y_new$date, format = "%m/%d/%Y")
ger_10Y_new$date <- as.Date(ger_10Y_new$date, format = "%m/%d/%Y")
ger_20Y_new$date <- as.Date(ger_20Y_new$date, format = "%m/%d/%Y")
ger_30Y_new$date <- as.Date(ger_30Y_new$date, format = "%m/%d/%Y")
ger_3M_new$date <- as.Date(ger_3M_new$date, format = "%m/%d/%Y")
ger_6M_new$date <- as.Date(ger_6M_new$date, format = "%m/%d/%Y")



#put all data frames into one
df_list_new <- c("ger_3M_new","ger_6M_new","ger_1Y_new", "ger_3Y_new","ger_5Y_new","ger_7Y_new","ger_10Y_new",
             "ger_20Y_new","ger_30Y_new")      

ger_yield_new <- ger_3M_new

for(i in df_list_new){
    ger_yield_new <- merge(ger_yield_new,eval(parse(text = i)), by = "date", suffixes =c("",i))
}

ger_yield_new <- ger_yield_new[,-2]
colnames(ger_yield_new)[-1] <- df_list_new
head(ger_yield_new)


# leaving only date and price 
ger_1Y <- ger_1Y[,c("Date","Price")]
ger_3Y <- ger_3Y[,c("Date","Price")]
ger_5Y <- ger_5Y[,c("Date","Price")]
ger_7Y <- ger_7Y[,c("Date","Price")]
ger_10Y <- ger_10Y[,c("Date","Price")]
ger_20Y <- ger_20Y[,c("Date","Price")]
ger_30Y <- ger_30Y[,c("Date","Price")]
ger_3M <- ger_3M[,c("Date","Price")]
ger_6M <- ger_6M[,c("Date","Price")]

ger_1Y$Date <- as.Date(ger_1Y$Date, format = "%m/%d/%Y")
ger_3Y$Date <- as.Date(ger_3Y$Date, format = "%m/%d/%Y")
ger_5Y$Date <- as.Date(ger_5Y$Date, format = "%m/%d/%Y")
ger_7Y$Date <- as.Date(ger_7Y$Date, format = "%m/%d/%Y")
ger_10Y$Date <- as.Date(ger_10Y$Date, format = "%m/%d/%Y")
ger_20Y$Date <- as.Date(ger_20Y$Date, format = "%m/%d/%Y")
ger_30Y$Date <- as.Date(ger_30Y$Date, format = "%m/%d/%Y")
ger_3M$Date <- as.Date(ger_3M$Date, format = "%m/%d/%Y")
ger_6M$Date <- as.Date(ger_6M$Date, format = "%m/%d/%Y")


#put all data frames into one
df_list <- c("ger_3M","ger_6M","ger_1Y", "ger_3Y","ger_5Y","ger_7Y","ger_10Y",
             "ger_20Y","ger_30Y")      

ger_yield <- ger_3M

for(i in df_list){
    ger_yield <- merge(ger_yield,eval(parse(text = i)), by = "Date", suffixes =c("",i))
}
ger_yield <- ger_yield[,-2]
colnames(ger_yield)[-1] <- df_list
head(ger_yield)
ger_yield[nrow(ger_yield),]

ger_yield <- ger_yield[ger_yield$Date <= "2023-12-31",]

colnames(ger_yield_new) <- colnames(ger_yield)
# last date 2024-02-18 
ger_yield_all <- rbind(ger_yield, ger_yield_new[ger_yield_new$Date > "2023-12-31",])
ger_yield <- ger_yield_all
ger_yield[911,2] <- ger_yield[910,2] + ger_yield[910,2]*(ger_yield[909,3]- ger_yield[910,3])/ger_yield[909,3]


#write.csv(ger_yield,file ="Data_libraries/ger_yields2.csv", sep = ',', col.names= TRUE)

# make long_format for plots
ger_yield_plot <- ger_yield %>% pivot_longer(-Date, names_to = 'Maturities', values_to = 'Price')

ger_yield_plot$Maturities <- factor(ger_yield_plot$Maturities, levels = df_list)
levels(ger_yield_plot$Maturities) <- c('3M','6M','1Y','3Y','5Y','7Y','10Y','20Y','30Y')

ger_yield_plot$Date <- as.Date(ger_yield_plot$Date, format = "%m/%d/%Y")

cc <- scales::seq_gradient_pal("#00897D", "#FF6B6B", "Lab")(seq(0,1,length.out=10))

# Yield rates 

plot_yc <- ggplot(data = ger_yield_plot, aes(x = Date, y = Price, group = Maturities, color = Maturities)) + 
  geom_line() + 
  geom_point(size = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept = as.Date("2023-03-15"), linetype = "dashed", alpha = 0.6) +  # Fix your xint if needed
  scale_colour_manual(values = cc) +
  scale_x_date(breaks = scales::breaks_pretty(20)) +
  labs(y = "Value in %", x = "") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4, shape = 15))) +  # Smaller points in legend
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
plot_yc

ggplot2::ggsave(paste0("Graphs_new/yield_curves.png"),plot_yc,dpi = 300, width = 7, height = 5)

DF_long <- list(Time = ger_yield[,1], Tau = c(1/4,1/2,1,3,5,7,10,20,30), Value = as.matrix(ger_yield[,-1]))

colorscales = list(c(0, 1), c( "#00897D","#FF6B6B"))
# Create 3D surface plot
plot_ger_yield <- plot_ly(z = ~ DF_long$Value, x = ~DF_long$Tau, y = ~DF_long$Time, type = "surface", colorscale = colorscales) %>%
  layout(scene = list(
    xaxis = list(title = "Maturity"),
    yaxis = list(title = "Time"),
    zaxis = list(title = "Values in %")
  ))



# make long_format for plots
plot_of_curves <- ger_yield[c(1,100,120,151,900,950,908),]
rownames(plot_of_curves) <- plot_of_curves[,1]
plot_of_curves <- plot_of_curves[,-1]
plot_of_curves <- as.data.frame(t(plot_of_curves))
plot_of_curves$Maturity <- c(1/4,1/2,1,3,5,7,10,20,30)

plot_of_curves <- plot_of_curves %>% pivot_longer(-Maturity ,names_to = 'Date', values_to = 'Price')

plot_of_curves$Date <- factor(plot_of_curves$Date)

cc <- scales::seq_gradient_pal("#00897D", "#FF6B6B", "Lab")(seq(0,1,length.out=7))

# Yield rates 
plot_shapes <-ggplot(data = plot_of_curves, aes(x = Maturity, y = Price, group = Date, color = Date)) + 
  geom_point(size = 1) +  # smaller points
  geom_line() +
  scale_colour_manual(values = cc) +
  scale_x_continuous(breaks = scales::breaks_pretty(20)) +
  labs(y = "Value in %", x = "") +
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4, shape = 15))) +  # Adjust legend symbols
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_line(color = "grey80", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggplot2::ggsave(paste0("Graphs_new/yield_shapes.png"),plot_shapes,dpi = 300, width = 7, height = 5)


# mean value and volatilities
mean_yield <- apply(ger_yield[,-1], 2, mean)
vol_yield <- apply(ger_yield[,-1], 2, sd)
min_yield <-  apply(ger_yield[,-1], 2, min)
max_yield <-  apply(ger_yield[,-1], 2, max)
Ac1 <-  apply(ger_yield[,-1], 2, function(x) acf(x, lag.max = 252)$acf[2])
Ac4 <-  apply(ger_yield[,-1], 2, function(x) acf(x, lag.max = 252)$acf[5])
Ac52 <-  apply(ger_yield[,-1], 2, function(x) acf(x, lag.max = 252)$acf[53])
Ac104 <-  apply(ger_yield[,-1], 2, function(x) acf(x, lag.max = 252)$acf[104])

a <- acf(ger_yield[,2], lag.max = 252)$acf

Stat_yield <- data.frame(Mean = mean_yield,
                         Volatility = vol_yield,
                         Min = min_yield,
                         Max = max_yield,
                         Ac1 = Ac1,
                         Ac4 = Ac4,
                         Ac52 = Ac52,
                         Ac104 = Ac104)

rownames(Stat_yield) <- c("3", "6", "12","36","60","84","120","240","360")
Stat_yield <- round(Stat_yield, digits =3 )

Hmisc::latex(Stat_yield, file = '', caption = "Mean and volatilities of German yield rates")


