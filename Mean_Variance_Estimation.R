library(quantmod)
library(TTR)
library(gbm)
library(quadprog)


#################################################################################
ML_data <- read.csv("Training.csv")

pred_mod = gbm(Target ~.,data = ML_data[,-c(16)],distribution="gaussian",n.trees=283,
               interaction.depth=2, shrinkage = 0.0401)

# Create fresh table for predicting return

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
  
  return(return_forecast)
}


########################################################################################
tickers = c("AAPL","IBM")

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

#####################################################################################


##########################################################################################

ML_database$chg_v2 <- (ML_database$v_2 - ML_database$v_1)/ML_database$v_1
ML_database$chg_v3 <- (ML_database$v_3 - ML_database$v_2)/ML_database$v_2
ML_database$chg_v4 <- (ML_database$v_4 - ML_database$v_3)/ML_database$v_3
ML_database$chg_v5 <- (ML_database$v_5 - ML_database$v_4)/ML_database$v_4

ML_database$vol_cat <- as.factor(ifelse(ML_database$chg_v5>0 & ML_database$chg_v4 > 0 & ML_database$chg_v3 > 0,
                                        1, ifelse(ML_database$chg_v5>0 | ML_database$chg_v4 > 0,
                                                  0,-1)))

##################################################################################





print(colnames(ML_database))
feat_cols <- c(2, 4:8,14:24)

AAPL_features <- ML_database[1:941,feat_cols]
IBM_features <- ML_database[942:1882,feat_cols]



#Normalize the data first

Normalize=function(Data){
  NewData=Data
  
  for(i in 1:length(colnames(NewData))) {
    if(class(NewData[,i]) == "numeric" || class(NewData[,i]) == "integer") {
      NewData[,i] <- as.vector(scale(NewData[,i])) }
  }
  return(NewData)
}


AAPL_ML <- cbind(Normalize(AAPL_features[,2:16]), AAPL_features$vol_cat, AAPL_features$Target)
colnames(AAPL_ML) <- c(colnames(AAPL_features[2:16]),"Volume_Cat", "Target")


IBM_ML <- cbind(Normalize(IBM_features[,2:16]), IBM_features$vol_cat, IBM_features$Target)
colnames(IBM_ML) <- c(colnames(IBM_features[2:16]),"Volume_Cat", "Target")

##############################################################################



AAPL_ML$pred = predict(pred_mod, newdata = AAPL_ML[,-c(16)],n.trees=283)
IBM_ML$pred = predict(pred_mod, newdata = IBM_ML[,-c(16)],n.trees=283)

AAPL_ML$pred <- AAPL_ML$pred*100
AAPL_ML$Target <- AAPL_ML$Target*100

IBM_ML$pred <- IBM_ML$pred*100
IBM_ML$Target <- IBM_ML$Target*100


AAPL_ML$diff <- AAPL_ML$pred - AAPL_ML$Target
IBM_ML$diff <- IBM_ML$pred - IBM_ML$Target

AAPL_ML$covs <- AAPL_ML$diff * IBM_ML$diff

# Covariance matrix

# Portfolio Performance Matrix

# From 01 January 2020 to 31 Aug 2020


ML_portfolio <- cbind.data.frame(ML_database$Rtn_Date[908:941],
                                 AAPL_ML$Target[908:941], IBM_ML$Target[908:941], 
                                 AAPL_ML$pred[908:941], IBM_ML$pred[908:941])

colnames(ML_portfolio) <- c("Rtn_date", "AAPL_Rtn","IBM_Rtn","AAPL_Pred","IBM_Pred")

st = 907

AAPL_var <- c()
IBM_var <- c()
covariance <- c()

for(i in 1:dim(ML_portfolio)[1]){
  tmp_1 <- (sum(AAPL_ML$diff[1:st]^2)/st)
  tmp_2 <- (sum(IBM_ML$diff[1:st]^2)/st)
  tmp_3 <- (sum((AAPL_ML$covs[1:st]))/st)
  print(tmp_1)
  print(tmp_3)
  AAPL_var <- c(AAPL_var,tmp_1)
  IBM_var <- c(IBM_var,tmp_2)
  covariance <- c(covariance,tmp_3)
  st = st + 1

}

ML_portfolio <- cbind.data.frame(ML_portfolio, AAPL_var, IBM_var, covariance)

####################################################################################


# Covariance Matrix

weights_solver <- function(var_1, var_2, cov_1,mu1,mu2){
  
  Dmat=matrix(c(var_1, cov_1, cov_1, var_2), ncol =2, byrow=TRUE)
  
  # List of returns
  Amat <- matrix(c(1,1,mu1,mu2),2,2, byrow=FALSE)
  
  #Target Return
  bvec  <- c(1, 0.25)
  
  # Optimization
  opt_sol=solve.QP(Dmat, c(0,0), Amat, bvec, meq=2, factorized=FALSE)
  weights <- opt_sol$solution
  return(weights)
  
}

weights.df <- data.frame()

for(i in 1:dim(ML_portfolio)[1]){
  tmp <- weights_solver(ML_portfolio[i,6],ML_portfolio[i,7],ML_portfolio[i,8],
                        ML_portfolio[i,4],ML_portfolio[i,5])
  
  weights.df <- rbind(weights.df,tmp)

}

colnames(weights.df) <- c("We_A","We_B")

ML_portfolio <- cbind(ML_portfolio, weights.df)

ML_portfolio$return <- ML_portfolio$AAPL_Rtn*ML_portfolio$We_A + 
  ML_portfolio$IBM_Rtn*ML_portfolio$We_B


##################################################################################

Base_portfolio <- cbind.data.frame(ML_database$Rtn_Date[908:941],
                                   AAPL_ML$Target[908:941], IBM_ML$Target[908:941])

colnames(Base_portfolio) <- c("Rtn_date", "AAPL_Rtn","IBM_Rtn")

st = 907

AAPL_mean <- c()
IBM_mean <- c()
AAPL_var <- c()
IBM_var <- c()
covariance <- c()

for(i in 1:dim(ML_portfolio)[1]){
  tmp_1 <- mean(AAPL_ML$Target[1:st])
  tmp_2 <- mean(IBM_ML$Target[1:st])
  
  tmp_3 <- var(AAPL_ML$Target[1:st])
  tmp_4 <- var(IBM_ML$Target[1:st])
  
  tmp_5 <- cov(AAPL_ML$Target[1:st],IBM_ML$Target[1:st])
  
  AAPL_mean <- c(AAPL_mean,tmp_1)
  IBM_mean <- c(IBM_mean,tmp_2)
  
  AAPL_var <- c(AAPL_var,tmp_3)
  IBM_var <- c(IBM_var,tmp_4)
  covariance <- c(covariance,tmp_5)
  st = st + 1
  
}

Base_portfolio <- cbind.data.frame(Base_portfolio, AAPL_mean, IBM_mean, AAPL_var, 
                                   IBM_var, covariance)

weights.df <- data.frame()

for(i in 1:dim(Base_portfolio)[1]){
  tmp <- weights_solver(Base_portfolio[i,6],Base_portfolio[i,7],Base_portfolio[i,8],
                        Base_portfolio[i,4],Base_portfolio[i,5])
  
  weights.df <- rbind(weights.df,tmp)
  
}

colnames(weights.df) <- c("We_A","We_B")

Base_portfolio <- cbind(Base_portfolio, weights.df)

Base_portfolio$return <- Base_portfolio$AAPL_Rtn*Base_portfolio$We_A + 
  Base_portfolio$IBM_Rtn*Base_portfolio$We_B



# Cases of extreme weights over the limit 0f 3 times investment amount shall be removed

rows = which(ML_portfolio$We_A > 3 | ML_portfolio$We_A < -2)

ML_portfolio <- ML_portfolio[-rows,]

Base_portfolio <- Base_portfolio[-rows,]


sum(Base_portfolio$return)
sum(ML_portfolio$return)


write.csv(Base_portfolio,"Base_Portfolio.csv")

write.csv(ML_portfolio,"ML_Portfolio.csv")
