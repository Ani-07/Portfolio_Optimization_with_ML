library(boot)
library(glmnet)
library(gbm)
library(neuralnet)
library(car)


#######################################################

ML_features <- read.csv("features_returns.csv")

colnames(ML_features)

###############################################################################

#Normalize the data first

Normalize=function(Data){
  NewData=Data
  
  for(i in 1:length(colnames(NewData))) {
    if(class(NewData[,i]) == "numeric" || class(NewData[,i]) == "integer") {
      NewData[,i] <- as.vector(scale(NewData[,i])) }
  }
  return(NewData)
}


norm_data <- cbind(Normalize(ML_features[,2:16]), ML_features$vol_cat, ML_features$Target)

colnames(norm_data) <- c(colnames(ML_features[2:16]),"Volume_Cat", "Target")

#norm_data$Target <- norm_data$Return-norm_data$Estimate

#State_1 = norm_data[which(norm_data$Cat == 1),-c(10,12,13)]
#State_2 = norm_data[which(norm_data$Cat == 2),-c(10,12,13)]
#State_3 = norm_data[which(norm_data$Cat == 3),-c(10,12,13)]
#State_4 = norm_data[which(norm_data$Cat == 4),-c(10,12,13)]
#State_5 = norm_data[which(norm_data$Cat == 5),-c(10,12,13)]

##############################################################################

ML_data = norm_data[,]

write.csv(ML_data,"Training.csv", row.names = FALSE)


Model_error = data.frame(stringsAsFactors = FALSE)

#Model_error = rbind(Model_error, state_model_er, stringsAsFactors = FALSE)

# Cost Function for model comparison

error_calc  = function(pred,test){
  error = ((pred-test)*100)**2
  return(sum(error)/length(pred))
}


# Model 1 - Stepwise Linear Regression
# Model 2 - Ridge Linear Regression
# Model 3 - Random Forests
# Model 4 - Boosting

###############################################################################

# State 3 Model

#s3 = dim(State_3)[1]
#s3_train = sample(1:s3,0.80*s3)

print("Linear Regression")

null <- glm(Target ~ 1, data=ML_data[,-16])
full <- glm(Target ~ ., data=ML_data[,-16])
s3_lr_model1 <- step(null, scope=formula(full),
                 direction="both", k = 2)
summary(s3_lr_model1)
s3_lr_error1 = cv.glm(ML_data[,-16],s3_lr_model1,K=5)$delta[1]*(100*100)



#s3_lr_pred1 <- predict.lm(s3_lr_model1, newdata = State_3[-s3_train,-10])
#s3_lr_error1 = error_calc(s3_lr_pred1,State_3[-s3_train,11])
#########################################################################

#s3_lr_model2 <- lm(Target ~., data = State_3[s3_train,-c(6:9)])
null <- glm(Target ~ 1, data=ML_data[,-c(12:15)])
full <- glm(Target ~ ., data=ML_data[,-c(12:15)])
s3_lr_model2 <- step(null, scope=formula(full),
                     direction="both", k = 2)

summary(s3_lr_model2)
s3_lr_error2 = cv.glm(ML_data[,-c(12:15)],s3_lr_model2,K=5)$delta[1]*(100*100)

model_1 = c("Lin_Reg_1",s3_lr_error1,0)
model_2 = c("Lin_Reg_2",s3_lr_error2,0)
Model_error = rbind(Model_error,model_1,model_2, stringsAsFactors = FALSE)

#s3_lr_pred2 <- predict.lm(s3_lr_model2, newdata = State_3[-s3_train,-c(6:9)])
#s3_lr_error2 = error_calc(s3_lr_pred2,State_3[-s3_train,11])


# Model 2 - Lasso Linear Regression

print("Lasso")

lambda = 10^seq(-4,0,by=.001)

s3_train = as.matrix(ML_data[,-c(16,17)])
s3_test = as.matrix(ML_data[,17])
#11
s3_las_mod1 = glmnet(s3_train,s3_test,alpha=1,lambda=lambda)
s3_las_cv1 = cv.glmnet(s3_train,s3_test,alpha=1,lambda=lambda, type.measure = "mse")
s3_las_lam1 = s3_las_cv1$lambda.min
s3_lasso_error1 <- s3_las_cv1$cvm[which(s3_las_cv1$lambda == s3_las_lam1)]*(100*100)

s3_train = as.matrix(ML_data[,-c(12:15,17)])
s3_test = as.matrix(ML_data[,17])

s3_las_mod2 = glmnet(s3_train,s3_test,alpha=1,lambda=lambda)
s3_las_cv2 = cv.glmnet(s3_train,s3_test,alpha=1,lambda=lambda, type.measure = "mse")
s3_las_lam2 = s3_las_cv2$lambda.min
s3_lasso_error2 <- s3_las_cv2$cvm[which(s3_las_cv2$lambda == s3_las_lam2)]*(100*100)


model_1 = c("Lasso_1",s3_lasso_error1, s3_las_lam1)
model_2 = c("Lasso_2",s3_lasso_error2, s3_las_lam2)
Model_error = rbind(Model_error,model_1,model_2, stringsAsFactors = FALSE)

#lassocoef=glmnet(x,y,alpha=1,lambda=lambda)
#predict(lassocoef,type="coefficients",s=bestlam)

las_hyp_1 <- cbind.data.frame(s3_las_cv1$lambda,s3_las_cv1$cvm)
colnames(las_hyp_1) <- c("Lambda", "CV Error")
file_name = paste("Lasso_1_Hyp", ".csv", sep = "")
write.csv(las_hyp_1, file_name, row.names = FALSE)



las_hyp_2 <- cbind.data.frame(s3_las_cv2$lambda,s3_las_cv2$cvm)
colnames(las_hyp_2) <- c("Lambda", "CV Error")
file_name = paste("Lasso_2_Hyp",".csv", sep = "")
write.csv(las_hyp_2, file_name, row.names = FALSE)





# Model 3 - Boosting

# Hyperparameters Tuning

# We will take learning rates between 0.0001 and 0.15 with 1000 trees. We will identify the
# lowest cross validated error 


print("Boosting")

Sys.sleep(420)

boost_learn = seq(0.0001,0.1, by = 0.005)


# Model 1

s3_boost_errors = data.frame()

for (i in boost_learn){
  print(i)
  s3_boost = gbm(Target ~.,data = ML_data[,-c(16)], distribution = "gaussian",
                   n.trees=1000,interaction.depth=2, shrinkage = i, cv.folds = 5)
  
  tree <- which(s3_boost$cv.error == min(s3_boost$cv.error))
  s3_error <- c(i,min(s3_boost$cv.error),tree)
  s3_boost_errors = rbind(s3_boost_errors,s3_error)
  
}

colnames(s3_boost_errors) <- c("Shrinkage","Error", "Tree")

s3_ind = which(s3_boost_errors$Error == min(s3_boost_errors$Error))
s3_boost_min = c(s3_boost_errors$Shrinkage[s3_ind],s3_boost_errors$Tree[s3_ind])

s3_boost_final = gbm(Target ~.,data = ML_data[,-c(16)], distribution = "gaussian",
               n.trees=s3_boost_min[2],interaction.depth=2, shrinkage = s3_boost_min[1], 
               cv.folds = 5)

min(s3_boost_final$cv.error)

s3_boost_error1 <- min(s3_boost_errors$Error)*(100*100)

####################################################################

Sys.sleep(420)

# Model 2

s3_boost_errors2 = data.frame()

for (i in boost_learn){
  print(i)
  s3_boost = gbm(Target ~.,data = ML_data[,-c(12:15)], distribution = "gaussian",
                 n.trees=1000,interaction.depth=2, shrinkage = i, cv.folds = 5)
  
  tree <- which(s3_boost$cv.error == min(s3_boost$cv.error))
  s3_error <- c(i,min(s3_boost$cv.error),tree)
  s3_boost_errors2 = rbind(s3_boost_errors2,s3_error)
  
}

colnames(s3_boost_errors2) <- c("Shrinkage","Error", "Tree")

s3_ind2 = which(s3_boost_errors2$Error == min(s3_boost_errors2$Error))
s3_boost_min2 = c(s3_boost_errors2$Shrinkage[s3_ind2],s3_boost_errors2$Tree[s3_ind2])

s3_boost_final2 = gbm(Target ~.,data = ML_data[,-c(12:15)], distribution = "gaussian",
                     n.trees=s3_boost_min2[2],interaction.depth=2, shrinkage = s3_boost_min2[1], 
                     cv.folds = 5)

min(s3_boost_final2$cv.error)

s3_boost_error2 <- min(s3_boost_errors2$Error)*(100*100)

##########################################################################

# Model Error

model_1 = c("Boosting_1",s3_boost_error1, paste(s3_boost_min[1],s3_boost_min[2], sep  = " "))
model_2 = c("Boosting_2",s3_boost_error2, paste(s3_boost_min2[1],s3_boost_min2[2], sep = ""))
Model_error = rbind(Model_error,model_1,model_2, stringsAsFactors = FALSE)




file_name = paste("Boost_1_Hyp", ".csv", sep = "")
write.csv(s3_boost_errors, file_name, row.names = FALSE)


file_name = paste("Boost_2_Hyp", ".csv", sep = "")
write.csv(s3_boost_errors2, file_name, row.names = FALSE)




#####################################################################################

# Neural Nets

print("Neural Nets")

Sys.sleep(420)

n = dim(ML_data)[1]

layers = 1:2
nodes = 5:7


s3_train = ML_data[,-c(16)]
s3_test = ML_data[,17]

hidden_cv_1 = data.frame()
for (l in layers){
  for (k in nodes){
    hidden_l = NULL
    for(j in 1:l){
      hidden_l = c(hidden_l,k)
  }
    print(hidden_l)
    cv = split(1:n, sample(1:5, n, replace=T))
    errors <- NULL
    for(i in  1:5){
      train1 = 1:n
      train1 = train1[-(unlist(cv[i]))]
      s3_nn <- neuralnet(Target ~., ML_data[train1,-c(16)], hidden=hidden_l, threshold=0.01)
      nn_pred <- compute(s3_nn, ML_data[-train1,-c(16,17)])
      result = mean((nn_pred$net.result - ML_data[-train1,17])^2) # measuring error rate
      errors <- c(errors,result)
    }
    print(hidden_l)
    if (length(hidden_l) == 1){
      hidden_l = c(hidden_l,0)
    }
    print(hidden_l)
    hidden_er = c(hidden_l, mean(errors)*(100*100))
    print(hidden_er)
    
    hidden_cv_1 = rbind(hidden_cv_1, hidden_er)
    
  } 
}

colnames(hidden_cv_1) <- c("Layer_1","Layer_2", "Error")

s3_nn_error1 <- min(hidden_cv_1$Error)

#######################################################################################
#10
Sys.sleep(420)

s3_train = ML_data[,-c(12:15)]
s3_test = ML_data[,17]

hidden_cv_2 = data.frame()
for (l in layers){
  for (k in nodes){
    hidden_l = NULL
    for(j in 1:l){
      hidden_l = c(hidden_l,k)
    }
    print(hidden_l)
    cv = split(1:n, sample(1:5, n, replace=T))
    errors <- NULL
    for(i in  1:5){
      train1 = 1:n
      train1 = train1[-(unlist(cv[i]))]
      s3_nn <- neuralnet(Target ~., ML_data[train1,-c(12:15)], hidden=hidden_l, threshold=0.01)
      nn_pred <- compute(s3_nn, ML_data[-train1,-c(12:15,17)])
      result = mean((nn_pred$net.result - ML_data[-train1,17])^2) # measuring error rate
      errors <- c(errors,result)
    }
    print(hidden_l)
    if (length(hidden_l) == 1){
      hidden_l = c(hidden_l,0)
    }
    print(hidden_l)
    hidden_er = c(hidden_l, mean(errors)*(100*100))
    print(hidden_er)
    
    hidden_cv_2 = rbind(hidden_cv_2, hidden_er)
    
  } 
}

colnames(hidden_cv_2) <- c("Layer_1","Layer_2", "Error")

s3_nn_error2 <- min(hidden_cv_2$Error)

model_1 = c("NeuralNets_1",s3_nn_error1, which(hidden_cv_1$Error == min(hidden_cv_1$Error)))
model_2 = c("NeuralNets_2",s3_nn_error2, which(hidden_cv_1$Error == min(hidden_cv_1$Error)))
Model_error = rbind(Model_error,model_1,model_2, stringsAsFactors = FALSE)

###########################################

file_name = paste("NN_1_Hyp", ".csv", sep = "")
write.csv(hidden_cv_1, file_name, row.names = FALSE)


file_name = paste("NN_2_Hyp", ".csv", sep = "")
write.csv(hidden_cv_2, file_name, row.names = FALSE)


###################################################################################

colnames(Model_error) <- c("Model Type", "Cross_Validated_Error", "Hyperparameters")

file_name = paste("ML_Model_Error", ".csv", sep = "")

write.csv(Model_error, file_name, row.names = FALSE)
