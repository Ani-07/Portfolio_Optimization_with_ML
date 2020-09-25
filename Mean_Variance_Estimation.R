library(boot)


ML_features <- read.csv("ML_features.csv")

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


norm_data <- cbind(Normalize(ML_features[,3:11]),ML_features$Estimate, 
                   ML_features$vol_cat_3, ML_features$Target,ML_features$Cat)

colnames(norm_data) <- c(colnames(ML_features[3:11]),"Estimate", "Volume_Cat", 
                         "Return","Category")

norm_data$Target <- norm_data$Return-norm_data$Estimate


var((ML_features$Target)*100)

State_1 = norm_data[which(norm_data$Cat == 1),-c(10,12,13)]
State_2 = norm_data[which(norm_data$Cat == 2),-c(10,12,13)]
State_3 = norm_data[which(norm_data$Cat == 3),-c(10,12,13)]
State_4 = norm_data[which(norm_data$Cat == 4),-c(10,12,13)]
State_5 = norm_data[which(norm_data$Cat == 5),-c(10,12,13)]

##############################################################################

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

null <- glm(Target ~ 1, data=State_3[,-10])
full <- glm(Target ~ ., data=State_3[,-10])
s3_lr_model1 <- step(null, scope=formula(full),
                 direction="both", k = 2)
summary(s3_lr_model1)
s3_lr_error1 = cv.glm(State_3[,-10],s3_lr_model1,K=5)$delta[1]*(100*100)

#s3_lr_pred1 <- predict.lm(s3_lr_model1, newdata = State_3[-s3_train,-10])
#s3_lr_error1 = error_calc(s3_lr_pred1,State_3[-s3_train,11])
#########################################################################

#s3_lr_model2 <- lm(Target ~., data = State_3[s3_train,-c(6:9)])
null <- glm(Target ~ 1, data=State_3[,-c(6:9)])
full <- glm(Target ~ ., data=State_3[,-c(6:9)])
s3_lr_model2 <- step(null, scope=formula(full),
                     direction="both", k = 2)

summary(s3_lr_model2)
s3_lr_error2 = cv.glm(State_3[,-c(6:9)],s3_lr_model2,K=5)$delta[1]*(100*100)

#s3_lr_pred2 <- predict.lm(s3_lr_model2, newdata = State_3[-s3_train,-c(6:9)])
#s3_lr_error2 = error_calc(s3_lr_pred2,State_3[-s3_train,11])


# Model 2 - Lasso Linear Regression

library(glmnet)

lambda = 10^seq(-4,0,by=.001)

s3_train = as.matrix(State_3[,-c(10,11)])
s3_test = as.matrix(State_3[,11])

s3_las_mod1 = glmnet(s3_train,s3_test,alpha=1,lambda=lambda)
s3_las_cv1 = cv.glmnet(s3_train,s3_test,alpha=1,lambda=lambda, type.measure = "mse")
s3_las_lam1 = s3_las_cv1$lambda.min
s3_lasso_error1 <- s3_las_cv1$cvm[which(s3_las_cv1$lambda == s3_las_lam1)]*(100*100)

as.vector(coef(cv.out, s = bestlam))

s3_train = as.matrix(State_3[,-c(6,9,11)])
s3_test = as.matrix(State_3[,11])

s3_las_mod2 = glmnet(s3_train,s3_test,alpha=1,lambda=lambda)
s3_las_cv2 = cv.glmnet(s3_train,s3_test,alpha=1,lambda=lambda, type.measure = "mse")
s3_las_lam2 = s3_las_cv2$lambda.min
s3_lasso_error2 <- s3_las_cv2$cvm[which(s3_las_cv2$lambda == s3_las_lam2)]*(100*100)


#lassocoef=glmnet(x,y,alpha=1,lambda=lambda)
#predict(lassocoef,type="coefficients",s=bestlam)


# Model 3 - Boosting

library(gbm)

# Hyperparameters Tuning

# We will take learning rates between 0.0001 and 0.15 with 1000 trees. We will identify the
# lowest cross validated error 

boost_learn = seq(0.0001,0.1, by = 0.005)

s3_boost_errors = data.frame()

for (i in boost_learn){
  print(i)
  s3_boost = gbm(Target ~.,data = State_3[,-c(10)], distribution = "gaussian",
                   n.trees=1000,interaction.depth=2, shrinkage = i, cv.folds = 5)
  
  tree <- which(s3_boost$cv.error == min(s3_boost$cv.error))
  s3_error <- c(i,min(s3_boost$cv.error),tree)
  s3_boost_errors = rbind(s3_boost_errors,s3_error)
  
}

colnames(s3_boost_errors) <- c("Shrinkage","Error", "Tree")

s3_ind = which(s3_boost_errors$Error == min(s3_boost_errors$Error))

s3_boost_min = c(s3_boost_errors$Shrinkage[s3_ind],s3_boost_errors$Tree[s3_ind])

s3_boost_final = gbm(Target ~.,data = State_3[,-c(10)], distribution = "gaussian",
               n.trees=s3_boost_min[2],interaction.depth=2, shrinkage = s3_boost_min[1], 
               cv.folds = 5)

min(s3_boost_final$cv.error)

#####################################################################################

# Neural Nets
library(neuralnet)

s3_train = State_3[,-c(10)]
s3_test = State_3[,11]

n = dim(State_3)[1]

layers = 1:3



cv = split(1:n, sample(1:5, n, replace=T))
errors <- NULL
for(i in  1:5){
  train1 = 1:n
  train1 = train1[-(unlist(cv[i]))]
  s3_nn <- neuralnet(Target ~., State_3[train1,-c(10)], hidden=c(10,10), threshold=0.01)
  nn_pred <- compute(s3_nn, State_3[-train1,-c(10,11)])
  result = mean((nn_pred$net.result - State_3[-train1,11])^2) # measuring error rate
  errors <- c(errors,result)
}
mean(errors)*(100*100)



s3_nn <- neuralnet(Target ~., State_3[,-c(10)], hidden=c(10,10), threshold=0.01)
compute(s3_nn, )









#S2 Model


s2 = dim(State_2)[1]
s2_train = sample(1:s2,0.80*s2)


s2_lr_model <- lm(Target ~., data = State_2[s2_train,-10])

s2_lr_pred <- predict.lm(s2_lr_model, newdata = State_2[-s2_train,-10])

error_calc(s2_lr_pred,State_2[-s2_train,11])

summary(s2_lr_model)


null <- lm(Target ~ 1, data=State_2[s2_train,-c(6:9)])
full <- lm(Target ~ ., data=State_2[s2_train,-c(6:9)])


stepwise <- step(null, scope=formula(full),
                 direction="both", k = 2)

summary(stepwise)

library(car)

vif(s2_lr_model)

library(boot)

?cv.glm()

