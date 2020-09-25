library(tree)

ML_database <- read.csv("ML_db.csv")

###########################################################################

# WHile the previous day prices and average prices may have a good relationship
# with the target, the same may not be true for VOlume. THus, we create different
# transformationns of the volume variable to see if there is any association

ML_database$chg_v2 <- (ML_database$v_2 - ML_database$v_1)/ML_database$v_1
ML_database$chg_v3 <- (ML_database$v_3 - ML_database$v_2)/ML_database$v_2
ML_database$chg_v4 <- (ML_database$v_4 - ML_database$v_3)/ML_database$v_3
ML_database$chg_v5 <- (ML_database$v_5 - ML_database$v_4)/ML_database$v_4


##############################################################################

# Prior to proceeding with our analysis, we check for outliers

plot(ML_database$Target)
plot(ML_database$Average)
plot(ML_database$Variance)
plot(ML_database$t_1)
plot(ML_database$t_2)
plot(ML_database$t_3)
plot(ML_database$chg_v2)
plot(ML_database$chg_v3)
plot(ML_database$chg_v4)
plot(ML_database$chg_v5)

# Only change in volume 3 has a significant outlier, which we shall remove

ML_database <- ML_database[-which(ML_database$chg_v3 > 390),]

plot(ML_database$chg_v3)

#########################################################################

ML_database$vol_cum <- ML_database$chg_v4 + ML_database$chg_v5

ML_database$vol_cum_2 <- ML_database$chg_v4 + ML_database$chg_v5 + ML_database$chg_v3

ML_database$vol_cat <- as.factor(ifelse(ML_database$chg_v5>0 & ML_database$chg_v4 > 0 & ML_database$chg_v3 > 0,
                                        1, ifelse(ML_database$chg_v5>0 | ML_database$chg_v4 > 0,
                                                  0,-1)))

ML_database$vol_cat_2 <- as.factor(ifelse(ML_database$vol_cum > 0,1,0))

ML_database$vol_cat_3 <- as.factor(ifelse(ML_database$vol_cum_2 > 0,1,0))

ML_database$vol_cat_4 <- as.factor(ifelse(ML_database$vol_cum > 0.5,1,
                                          ifelse(ML_database$vol_cum < 0,0,-1)))

ML_database$vol_cat_5 <- as.factor(ifelse(ML_database$vol_cum_2 > 0.5,1,
                                          ifelse(ML_database$vol_cum_2 < 0,0,-1)))

##############################################################################

# We will be training separate models for each category and therefore, we will split them
# into 5 different datasets

State_1 = ML_database[which(ML_database$Cat == 1),]
State_2 = ML_database[which(ML_database$Cat == 2),]
State_3 = ML_database[which(ML_database$Cat == 3),]
State_4 = ML_database[which(ML_database$Cat == 4),]
State_5 = ML_database[which(ML_database$Cat == 5),]

################################################################################

overall_cor = cor(ML_database[,2:20])
state1_cor = cor(State_1[,2:20])
state2_cor = cor(State_2[,2:20])
state3_cor = cor(State_3[,2:20])
state4_cor = cor(State_4[,2:20])
state5_cor = cor(State_5[,2:20])

cor_comp_tar = rbind(overall_cor[,1],state1_cor[,1],state2_cor[,1],state3_cor[,1],
                     state4_cor[,1],state5_cor[,1])


#################################################################################

overall_cor_sp = cor(ML_database[,2:20], method = "spearman")
state1_cor_sp = cor(State_1[,2:20], method = "spearman")
state2_cor_sp = cor(State_2[,2:20], method = "spearman")
state3_cor_sp = cor(State_3[,2:20],method = "spearman")
state4_cor_sp = cor(State_4[,2:20], method = "spearman")
state5_cor_sp = cor(State_5[,2:20], method = "spearman")

cor_comp_sp = rbind(overall_cor_sp[,1],state1_cor_sp[,1],state2_cor_sp[,1],state3_cor_sp[,1],
                     state4_cor_sp[,1],state5_cor_sp[,1])


######################################################################################


vol <- aov(Target ~ vol_cat, data = ML_database)
summary(vol)
vol <- aov(Target ~ vol_cat_2, data = ML_database)
summary(vol)
vol <- aov(Target ~ vol_cat_3, data = ML_database)
summary(vol)
vol <- aov(Target ~ vol_cat_4, data = ML_database)
summary(vol)
vol <- aov(Target ~ vol_cat_5, data = ML_database)
summary(vol)


# We thus see that vol_cat_3 has the highest statsitical significance and thus we keep that
# variable.


# Based on the above analysis, we shall drop the following variables - all volume variables
# and volume categories except for Vol_cat_3

print(colnames(ML_database))
feat_cols <- c(1:7,15:18,23,13,14)

ML_features <- ML_database[,feat_cols]

write.csv(ML_features,"ML_features.csv", row.names = FALSE)
