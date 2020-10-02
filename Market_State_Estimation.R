
library(quantmod)
library(TTR)
library(tree)

# Obtain Stock data of Apple July 30th, 2002 and Dec 31st, 2019



ML_data <- function(ticker){
  apple <- getSymbols(ticker, from = "2002-07-30", to = "2020-08-31", auto.assign = F)
  
  weekly.rtn <- weeklyReturn(apple, type = "arithmetic")
  weekly.rtn <- weekly.rtn[-c(1,2,3),]
  
  daily_ret = dailyReturn(apple[,4])
  
  #RSI_Return = db[start_i:(end_i),]
  
  rsi_5 = RSI(apple[,4], n = 5)
  rsi_14 = RSI(apple[,4], n = 14)
  
  macd_5_10 = MACD(apple[,4],nFast = 5, nSlow = 10)
  macd_12_26 = MACD(apple[,4],nFast = 12, nSlow = 26)
  
  
  return_forecast = data.frame()
  
  for (i in index(weekly.rtn)){
    target = as.numeric(weekly.rtn$weekly.returns[which(index(weekly.rtn) == i)])
    print(target)
    print(as.Date(i))
    
    if ((i-7) %in% index(daily_ret)){
      end = which(index(daily_ret) == (i-7))
      
      
    } else if ((i-8) %in% index(daily_ret)){
      end = which(index(daily_ret) == (i-8))
      
      
    } else if ((i-9) %in% index(daily_ret)){
      end = which(index(daily_ret) == (i-9))
      
    }
    
    print(end)
    start = (end - 5)
    print(start)
    week_date = index(daily_ret)[end]
    avg = round(mean(daily_ret$daily.returns[start:end]),6)
    var = round(var(daily_ret$daily.returns[start:end]),6)
    t_1 = daily_ret$daily.returns[end]
    t_2 = daily_ret$daily.returns[end-1]
    t_3 = daily_ret$daily.returns[end-2]
    v_1 = apple[,5][end]
    v_2 = apple[,5][end-1]
    v_3 = apple[,5][end-2]
    v_4 = apple[,5][end-3]
    v_5 = apple[,5][end-4]
    
    tmp_rsi_1 = rsi_5$rsi[end]
    tmp_rsi_2 = rsi_14$rsi[end]
    avg_rsi_1  = mean(rsi_5$rsi[(end-3):end])
    avg_rsi_2  = mean(rsi_14$rsi[(end-3):end])
    
    
    tmp_macd_1 = macd_5_10$macd[end]
    tmp_macd_2 = macd_12_26 $macd[end]
    
    
    return_forecast = rbind(return_forecast,c(i,target,week_date,avg,var,t_1,t_2,t_3,
                                              v_1,v_2,v_3,v_4,v_5,tmp_rsi_1,tmp_rsi_2,
                                              tmp_macd_1,tmp_macd_2, avg_rsi_1,avg_rsi_2))
  }
  
  colnames(return_forecast) <- c("Rtn_Date","Target","Week_date","Average","Variance",
                                 "t_1", "t_2","t_3","v_1","v_2","v_3","v_4","v_5","RSI_1", 
                                 "RSI_2","MACD_1","MACD_2","Avg_RSI_1","Avg_RSI_2")
  return_forecast$Rtn_Date <- as.Date(return_forecast$Rtn_Date)
  
  d = is.na(return_forecast$MACD_2)
  rows = which(d == 1)
  
  return_forecast <- return_forecast[-rows,]
  
  
  return(return_forecast)
}


########################################################################################
tickers = c("MSFT","GOOG","AAPL","FB","PYPL","AMZN","INTC","HPQ","DELL","SNE","IBM")

ML_database <- data.frame()

for (i in 1:length(tickers)){
  tick = tickers[i]
  print(tick)
  #start_i = start_end[i,1]
  #end_i = start_end[i,2]
  tmp = ML_data(tick) #,RSI_database,start_i,end_i)
  print(dim(tmp))
  print(colnames(tmp))
  ML_database = rbind(ML_database,tmp)
  
}


ML_database$Week_date <- as.Date(ML_database$Week_date)


#################################################################################################


cor(ML_database$Target,ML_database$RSI_1)
cor(ML_database$Target,ML_database$RSI_2)

cor(ML_database$Target,ML_database$MACD_1)
cor(ML_database$Target,ML_database$MACD_2)


cor(ML_database$Target,ML_database$Avg_RSI_1)
cor(ML_database$Target,ML_database$Avg_RSI_2)


######################################################



RSI_1 <- tree(Target ~ RSI_1, ML_database)
RSI_2 <- tree(Target ~ RSI_2, ML_database)

MACD_1 <- tree(Target ~ MACD_1, ML_database)
MACD_2 <- tree(Target ~ MACD_2, ML_database)

Avg_RSI_1 <- tree(Target ~ Avg_RSI_1, ML_database)
Avg_RSI_2 <- tree(Target ~ Avg_RSI_2, ML_database)

plot(RSI_1)
plot(RSI_2)
plot(Avg_RSI_1)
plot(Avg_RSI_2)
plot(MACD_1)
plot(MACD_2)

#########################################################################

# Create fresh table for predicting return

write.csv(ML_database,"Return_Data.csv", row.names = FALSE)




