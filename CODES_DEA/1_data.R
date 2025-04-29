# data
# import stock names 
symbols <- read.table("C:/Users/alexb/Desktop/DEA_TS_MODELS/data/symbols.txt",
                      quote="\"", comment.char="")
symbols <- symbols[,1]


# first and last day of data - 4 years of data
start <- as.Date("2018-01-01")
end <- as.Date("2021-12-31")

# creating time series variable 
stocks<- xts() 

# number of stocks 
num.st <- length(symbols)

# progress bar
pb <- txtProgressBar(min = 0, max = num.st, style=3)

# importing data from yahoo.finance
for(i in 1:num.st) {
  symbols[i]-> symbol
  # specify the "from" date to desired start date
  tryit <- try(getSymbols(symbol, from = start, to = end, src='yahoo',
                          auto.assign = FALSE))
  if(inherits(tryit, "try-error")){
    i <- i+1
  } else {
    # specify the "from" date to desired start date
    data <- getSymbols(symbol, from = start, to = end, src='yahoo',
                       auto.assign = FALSE)
    stocks <- merge(stocks, Ad(data))
    rm(symbol)
  }
  setTxtProgressBar(pb, i)
}

#  plotting data
head(stocks)
for( ind in 1:num.st ) 
  {
    a <- plot(stocks[, ind], type = "l", 
              main = paste(ind, colnames(stocks)[ind]))
    print(a)
  }

# 47, 39, 34, 2 - not enough data for those 4
symbols <- symbols[-c(2,34,39,47)]
stocks <- stocks[,-c(2,34,39,47)]
num.st <- num.st - 4

# checking NA in columns - maximum is 17 - total days is equal to 771
which(colSums(is.na(stocks)) > 10)
nrow(stocks)

sum(is.na(rowSums(stocks)))
# addjust NA values becuase of dividend-days\ holidays
stocks.km <- apply(stocks, 2, 
                 function (x) na_kalman(x, model = "auto.arima", smooth = TRUE))

# checking results from interpolation
for( i in 1:num.st)
{
  na.data <- which(is.na(stocks[,i]))
  ind <- c(na.data, na.data -1, na.data + 1)
  ind <- ind[order(ind)]
  ind <- unique(ind)
  
  print(cbind(stocks[ind,i],stocks.km[ind,i]))
  
}

# calculating logaritmic returns
stocks.lr <- log(stocks.km[2:nrow(stocks.km), ]) - log(stocks.km[1:(nrow(stocks.km)-1), ])

#  plotting data logaritmic returns
head(stocks.lr)
for( ind in 1:num.st ) 
{
  a <- plot(stocks.lr[, ind], type = "l", 
            main = paste(ind, colnames(stocks.lr)[ind]))
  print(a)
}

# 
colnames(stocks.lr)  <- str_remove(colnames(stocks.lr), ".Adjusted")
colnames(stocks.km)  <- str_remove(colnames(stocks.km), ".Adjusted")

# export data as R.data
save(stocks.lr, file = "data/stocks_lr.RData")
save(stocks.km, file = "data/stocks_km.RData")
