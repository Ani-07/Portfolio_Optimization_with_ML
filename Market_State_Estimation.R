
library(quantmod)
library(TTR)

# Obtain Stock data of Apple July 30th, 2002 and Dec 31st, 2019



RSI_data <- function(ticker){
  apple <- getSymbols(ticker, from = "2002-07-30", to = "2019-12-31", auto.assign = F)

  weekly.rtn <- weeklyReturn(apple, type = "log")
  
  rsi_5 = RSI(apple[,4], n = 5)
  rsi_14 = RSI(apple[,4], n = 14)
  
  macd_5_10 = MACD(apple[,4],nFast = 5, nSlow = 10)
  macd_12_26 = MACD(apple[,4],nFast = 12, nSlow = 26)
  
  # Create a Table with RSI and weekly return data
  
  RSI_Return = data.frame()
  
  for(i in index(weekly.rtn)){
    name = ticker
    rtn_ind = which(index(weekly.rtn) == i)
    rtn = weekly.rtn$weekly.returns[rtn_ind]
    a = i-1
    if (a %in% index(rsi_5)){
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      
    }else if ((a-1) %in% index(rsi_5)){
      #print("a")
      a = a-1
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      #tmp_obv = apple_obv$obv[ind]
      
    }else if ((a-2) %in% index(rsi_5)){
      a = a-2
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
      
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
      #tmp_obv = apple_obv$obv[ind]
    
    }else if ((a-3) %in% index(rsi_5)){
      a = a-3
      ind = which(index(rsi_5) == a)
      tmp_rsi_1 = rsi_5$rsi[ind]
      tmp_rsi_2 = rsi_14$rsi[ind]
        
      tmp_macd_1 = macd_5_10$macd[ind]
      tmp_macd_2 = macd_12_26 $macd[ind]
        
        
    }else{
      #print("c")
      tmp_rsi_1 = NA
      tmp_macd_1 = NA
      tmp_rsi_2 = NA
      tmp_macd_2 = NA
      #tmp_obv = 0
    }
    #print(avg_rsi)
    RSI_Return = rbind(RSI_Return,c(i,rtn,a,tmp_rsi_1,tmp_rsi_2,tmp_macd_1,tmp_macd_2))
    
  }
  
  colnames(RSI_Return) <- c("Rtn_Date","Return","RSI_Date","RSI_1", "RSI_2",
                            "MACD_1","MACD_2")
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


plot(RSI_database$Return,RSI_database$RSI)
plot(RSI_database$Return,RSI_database$Avg_RSI)
plot(RSI_database$Return,RSI_database$Signal)
plot(RSI_database$Return,RSI_database$OBV)


RSI_1 <- tree(Return ~ RSI_1, RSI_database)
RSI_2 <- tree(Return ~ RSI_2, RSI_database)

MACD_1 <- tree(Return ~ MACD_1, RSI_database)
MACD_2 <- tree(Return ~ MACD_2, RSI_database)


pred_r1 <- predict(RSI_1, newdata = RSI_database)
error_r1 = abs((pred_r1-RSI_1$y)*100)
print(sum(error_r1)/length(pred_r1))

pred_r2 <- predict(RSI_2, newdata = RSI_database)
error_r2 = abs((pred_r2-RSI_2$y)*100)
print(sum(error_r2)/length(pred_r2))

pred_m1 <- predict(MACD_1, newdata = RSI_database)
error_m1 = abs((pred_m1-MACD_1$y)*100)
print(sum(error_m1)/length(pred_m1))


pred_m2 <- predict(MACD_2, newdata = RSI_database)
error_m2 = abs((pred_m2-MACD_2$y)*100)
print(sum(error_m2)/length(pred_m2))



######################################################################################################

plot(RSI_1)
text(RSI_1)


RSI_splits = c(0,100)

for(i in RSI_1$frame$splits[,1]){
  if(i != ""){
    tmp = as.numeric(chartr(old = "<", new = " ",i))
    RSI_splits = c(RSI_splits,tmp)
    
  }
  
}

RSI_splits = sort(RSI_splits)
labels = 1:(length((RSI_splits))-1)

############################################################################

# Filter based on RSI Categories

row.names(RSI_database) <- NULL

RSI_database$RSI_cat <- cut(RSI_database$RSI_1,breaks = RSI_splits, labels = labels)


cat_mean = NULL

for(i in 1:length(RSI_1$frame$yval)){
  if(i%%2 == 1){
    cat_mean = c(cat_mean, RSI_1$frame$yval[i])
  }
  
}

cat_mean <- RSI_1$frame$yval[which(RSI_1$frame$var == "<leaf>")]

#########################################################################

# Create fresh table for predicting return

ML_data <- function(ticker, db,start_i,end_i){
  apple <- getSymbols(ticker, from = "2002-07-30", to = "2019-12-31", auto.assign = F)
  
  daily_ret = dailyReturn(apple[,4])

  RSI_Return = db[start_i:(end_i),]
  
  return_forecast = data.frame()
  
  for (i in RSI_Return$Rtn_Date){
    target = RSI_Return$Return[which(RSI_Return$Rtn_Date == i)]
    cat = RSI_Return$RSI_cat[which(RSI_Return$Rtn_Date == i)]
    exp_ret = cat_mean[RSI_Return$RSI_cat[which(RSI_Return$Rtn_Date == i)]]
    end = which(index(daily_ret) == i)
    start = (end - 5)
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
    
    return_forecast = rbind(return_forecast,c(i,target,exp_ret,avg,var,t_1,t_2,t_3,v_1,v_2,v_3,v_4,v_5,cat))
  }
  
  colnames(return_forecast) <- c("Rtn_Date","Target","Estimated","Average","Variance",
                                 "t_1", "t_2","t_3","v_1","v_2","v_3","v_4","v_5","Cat")
  return_forecast$Rtn_Date <- as.Date(return_forecast$Rtn_Date)
  
  return(return_forecast)
}


########################################################################################
tickers = c("MSFT","GOOG","AAPL","FB","PYPL","AMZN","INTC","HPQ","DELL","SNE","IBM")

ML_database <- data.frame()

for (i in 1:length(tickers)){
  tick = tickers[i]
  start_i = start_end[i,1]
  end_i = start_end[i,2]
  tmp = ML_data(tick,RSI_database,start_i,end_i)
  print(dim(tmp))
  print(colnames(tmp))
  ML_database = rbind(ML_database,tmp)
  
}


write.csv(ML_database,"ML_database.csv")



