
library(quantmod)
library(TTR)
library(tree)

# Obtain Stock data of Apple July 30th, 2002 and Dec 31st, 2019



RSI_data <- function(ticker){
  apple <- getSymbols(ticker, from = "2002-07-30", to = "2019-12-31", auto.assign = F)

  weekly.rtn <- weeklyReturn(apple, type = "arithmetic")
  
  rsi_5 = RSI(apple[,4], n = 5)
  rsi_14 = RSI(apple[,4], n = 14)
  
  macd_5_10 = MACD(apple[,4],nFast = 5, nSlow = 10)
  macd_12_26 = MACD(apple[,4],nFast = 12, nSlow = 26)
  
  # Create a Table with RSI and weekly return data
  
  RSI_Return = data.frame()
  
  weekly.rtn <- weekly.rtn[-c(1,2),]
  
  for(i in index(weekly.rtn)){
    name = ticker
    rtn_ind = which(index(weekly.rtn) == i)
    rtn = weekly.rtn$weekly.returns[rtn_ind]
    a = i-7
    if (a-3 %in% index(rsi_5)){
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      avg_rsi_1  = mean(rsi_5$rsi[(ind-3):ind])
      avg_rsi_2  = mean(rsi_14$rsi[(ind-3):ind])
      
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      
    }else if ((a-4) %in% index(rsi_5)){
      #print("a")
      a = a-1
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      avg_rsi_1  = mean(rsi_5$rsi[(ind-3):ind])
      avg_rsi_2  = mean(rsi_14$rsi[(ind-3):ind])
      
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      #tmp_obv = apple_obv$obv[ind]
      
    }else if ((a-2) %in% index(rsi_5)){
      a = a-2
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      avg_rsi_1  = mean(rsi_5$rsi[(ind-3):ind])
      avg_rsi_2  = mean(rsi_14$rsi[(ind-3):ind])
      
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      #tmp_obv = apple_obv$obv[ind]
    
    }else if ((a-3) %in% index(rsi_5)){
      a = a-3
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      avg_rsi_1  = mean(rsi_5$rsi[(ind-3):ind])
      avg_rsi_2  = mean(rsi_14$rsi[(ind-3):ind])
      
        
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
        
        
    }else{
      #print("c")
      tmp_rsi_1 = NA
      tmp_macd_1 = NA
      tmp_rsi_2 = NA
      tmp_macd_2 = NA
      avg_rsi_1  = NA
      avg_rsi_2  = NA
      
      
      #tmp_obv = 0
    }
    #print(avg_rsi)
    RSI_Return = rbind(RSI_Return,c(i,rtn,a,tmp_rsi_1,tmp_rsi_2,tmp_macd_1,tmp_macd_2, 
                                    avg_rsi_1,avg_rsi_2))
    
  }
  
  colnames(RSI_Return) <- c("Rtn_Date","Return","RSI_Date","RSI_1", "RSI_2",
                            "MACD_1","MACD_2","Avg_RSI_1","Avg_RSI_2")
  RSI_Return$Rtn_Date <- as.Date(RSI_Return$Rtn_Date)
  RSI_Return$RSI_Date <- as.Date(RSI_Return$RSI_Date)
  
  d = is.na(RSI_Return$MACD_2)
  rows = which(d == 1)
  
  RSI_Return <- RSI_Return[-rows,]

  return(RSI_Return)
}

#################################################################################################

tickers = c("MSFT","GOOG","AAPL","FB","PYPL","AMZN","INTC","HPQ","DELL","SNE","IBM")

start_end = data.frame()

RSI_database <- data.frame()

for (tick in tickers){
  start_t = dim(RSI_database)[1]+1
  RSI_database = rbind(RSI_database,RSI_data(tick))
  end_t = dim(RSI_database)[1]
  start_end = rbind(start_end,list(start_t,end_t))
}


cor(RSI_database$Return,RSI_database$RSI_1)
cor(RSI_database$Return,RSI_database$RSI_2)

cor(RSI_database$Return,RSI_database$MACD_1)
cor(RSI_database$Return,RSI_database$MACD_2)


cor(RSI_database$Return,RSI_database$Avg_RSI_1)
cor(RSI_database$Return,RSI_database$Avg_RSI_2)



#########################################################################

# Create fresh table for predicting return

ML_data <- function(ticker, db,start_i,end_i){
  apple <- getSymbols(ticker, from = "2002-07-30", to = "2019-12-31", auto.assign = F)
  
  daily_ret = dailyReturn(apple[,4])
  
  RSI_Return = db[start_i:(end_i),]
  
  rsi_5 = RSI(apple[,4], n = 5)
  rsi_14 = RSI(apple[,4], n = 14)
  
  macd_5_10 = MACD(apple[,4],nFast = 5, nSlow = 10)
  macd_12_26 = MACD(apple[,4],nFast = 12, nSlow = 26)
  
  
  return_forecast = data.frame()
  
  for (i in RSI_Return$Rtn_Date){
    target = RSI_Return$Return[which(RSI_Return$Rtn_Date == i)]
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
  
  return(return_forecast)
}


########################################################################################
tickers = c("MSFT","GOOG","AAPL","FB","PYPL","AMZN","INTC","HPQ","DELL","SNE","IBM")

ML_database <- data.frame()

for (i in 1:length(tickers)){
  tick = tickers[i]
  print(tick)
  start_i = start_end[i,1]
  end_i = start_end[i,2]
  tmp = ML_data(tick,RSI_database,start_i,end_i)
  print(dim(tmp))
  print(colnames(tmp))
  ML_database = rbind(ML_database,tmp)
  
}


ML_database$Week_date <- as.Date(ML_database$Week_date)


write.csv(ML_database,"Return_Data.csv", row.names = FALSE)




