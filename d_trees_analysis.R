library(tidyverse)
library(rpart)
library(lubridate)
library(pROC)
load("database4_20s.Rda")


database4 <- database4 %>% mutate(dl1_field.sensor.humidity = fields.sensor.humidity - dplyr::lag(fields.sensor.humidity, n = 1),
                                  dl1_field.sensor.temperature = fields.sensor.temperature - dplyr::lag(fields.sensor.temperature, n = 1),
                                  dl1_field.sensor.soundavg = fields.sensor.soundavg - dplyr::lag(fields.sensor.soundavg, n = 1),
                                  dl1_field.sensor.soundpeak = fields.sensor.soundpeak - dplyr::lag(fields.sensor.soundpeak, n = 1),
                                  df1_field.sensor.humidity = fields.sensor.humidity - dplyr::lead(fields.sensor.humidity, n = 1),
                                  df1_field.sensor.temperature = fields.sensor.temperature - dplyr::lead(fields.sensor.temperature, n = 1),
                                  df1_field.sensor.soundavg = fields.sensor.soundavg - dplyr::lead(fields.sensor.soundavg, n = 1),
                                  df1_field.sensor.soundpeak = fields.sensor.soundpeak - dplyr::lead(fields.sensor.soundpeak, n = 1))

database4 <- database4 %>% mutate(is_weekend = 1*(wday(int_start(intervals), week_start = 1) %in% c(6,7))) %>%
  filter(is_weekend == 0 & ((hour(int_start(intervals)) == 5 &
                               minute(int_start(intervals)) >= 21)  |
                              (hour(int_start(intervals)) == 6 &
                                 minute(int_start(intervals)) < 14))) %>% dplyr::select(-is_weekend)

create_lags_2D <- function(database, n, colss){
  A <- array(0, dim = c(nrow(database), (2*n+1)*length(colss)))
  dat <- database[, colss]
  B <- list()
  B[[n+1]] <- as.matrix(dat)
  for(i in 1:n){
    B[[n+1-i]] <- as.matrix(dat %>% mutate_all(lag, n = i))
    B[[n+1+i]] <- as.matrix(dat %>% mutate_all(lead, n = i))
  }
  A <- do.call(cbind, B)
  return(A)
}

A <- create_lags_2D(database4, 12, 3:14)
X <- A[minute(int_start(database4$intervals)) >=25 | minute(int_start(database4$intervals)) <10,]
Y <- 1*database4$shower[minute(int_start(database4$intervals)) >=25 | minute(int_start(database4$intervals)) <10]

# 135 per day for 227 days, we split for train test with 180 days train, 47 days test

X_train <- X[1:(135*180),]
X_test <- X[(135*180+1):nrow(X),]
Y_train <- Y[1:(135*180)]
Y_test <- Y[(135*180+1):nrow(X)]

c(train_ratio =  sum(Y_train)/length(Y_train), test_ratio = sum(Y_test)/length(Y_test))

weights <- numeric(length(Y_train))
weights[Y_train == 0] <- 1
weights[Y_train == 1] <- 4

data_train <- data.frame(shower = factor(Y_train), X_train)
data_test <- data.frame(shower = factor(Y_test), X_test)

set.seed(10)
rf  <- ranger(shower ~ ., data = data_train, num.trees = 500, probability = TRUE)
dd <- predict(rf, data_test)$predictions[,2]
Predicted_main = dd > 0.5
TA <- table(True = Y_test, Predicted = Predicted_main)
FPR <- TA[1,2]/(TA[1,2]+TA[1,1])
FNR <- TA[2,1]/(TA[2,1]+TA[2,2])
TA2 <- table(Main = Predicted_main, Predicted = Predicted_main)
mcnemar <- mcnemar.test(TA2)$p.value
AUC <- roc(Y_test, c(dd))$auc
sum(dd) ;  sum(Y_test)


ddmat <- matrix(dd, nrow = 135)
testmat <- matrix(Y_test, nrow = 135)

Error_main <- colSums(ddmat)-colSums(testmat)
#wilcox.test(Error_main, y = Error_main, alternative = "two.sided", paired = TRUE)$p.value
wilcox <- NaN

main <- c(sum(Predicted_main==Y_test)/length(Y_test), FPR, FNR, AUC, mcnemar, wilcox,  abs((sum(dd) - sum(Y_test))/3), mean(abs(colSums(ddmat)-colSums(testmat))/3), sd(abs(colSums(ddmat)-colSums(testmat))/3))
Error_mat <- matrix(0 , ncol = 1, nrow = 47)
Error_mat[,1] <- Error_main



solo <- sapply(c("humidity","temperature", "soundavg", "soundpeak"), function(sensor){
  X_train_temp <- X_train[,grep(paste0("^.*",sensor,".*$"), dimnames(X_test)[[2]])]
  X_test_temp <- X_test[,grep(paste0("^.*",sensor,".*$"), dimnames(X_test)[[2]])]
  data_train_temp <- data.frame(shower = factor(Y_train), X_train_temp)
  data_test_temp <- data.frame(shower = factor(Y_test), X_test_temp)
  
  set.seed(10)
  rf  <- ranger(shower ~ ., data = data_train_temp, num.trees = 500, probability = TRUE, case.weights = weights, class.weights = c(1, 4), classification = TRUE)
  dd <- predict(rf, data_test)$predictions[,2]
  AUC <- roc(Y_test, c(dd))$auc
  Predicted = dd > 0.5
  TA <- table(True = Y_test, Predicted = Predicted)
  FPR <- TA[1,2]/(TA[1,2]+TA[1,1])
  FNR <- TA[2,1]/(TA[2,1]+TA[2,2])
  TA2 <- table(Main = Predicted_main, Predicted = Predicted)
  mcnemar <- mcnemar.test(TA2)$p.value
  ddmat <- matrix(dd, nrow = 135)
  testmat <- matrix(Y_test, nrow = 135)
  Error <- colSums(ddmat)-colSums(testmat)
  Error_mat <<- cbind(Error_mat, Error)
  wilcox <- wilcox.test(Error, y = Error_main, alternative = "two.sided", paired = TRUE)$p.value
  c(sum(Predicted==Y_test)/length(Y_test), FPR, FNR, AUC, mcnemar, wilcox,  abs((sum(dd) - sum(Y_test))/3), mean(abs(colSums(ddmat)-colSums(testmat))/3), sd(abs(colSums(ddmat)-colSums(testmat))/3))
})


sens <- c("humidity","temperature", "soundavg", "soundpeak")
combos <- t(outer(sens, sens, paste))[t(upper.tri(outer(sens, sens, paste)))]
duo <- sapply(combos, function(sensor){
  X_train_temp <- X_train[,grep(paste0(paste0("(",gsub(" ", "|", sensor),")")), dimnames(X_test)[[2]])]
  X_test_temp <- X_test[,grep(paste0(paste0("(",gsub(" ", "|", sensor),")")), dimnames(X_test)[[2]])]
  data_train_temp <- data.frame(shower = Y_train, X_train_temp)
  data_test_temp <- data.frame(shower = Y_test, X_test_temp)
  
  set.seed(10)
  hmm_model  <- depmix(shower ~ ., data = data_train_temp, nstates = 2, family = binomial(), verbose = TRUE)
  fit <-  fit(hmm_model, verbose = TRUE)
  dd <- predict(fit, data_test_temp, type = "response")
  AUC <- roc(Y_test, c(dd))$auc
  Predicted = dd > 0.5
  TA <- table(True = Y_test, Predicted = Predicted)
  FPR <- TA[1,2]/(TA[1,2]+TA[1,1])
  FNR <- TA[2,1]/(TA[2,1]+TA[2,2])
  TA2 <- table(Main = Predicted_main, Predicted = Predicted)
  mcnemar <- mcnemar.test(TA2)$p.value
  ddmat <- matrix(dd, nrow = 135)
  testmat <- matrix(Y_test, nrow = 135)
  Error <- colSums(ddmat)-colSums(testmat)
  Error_mat <<- cbind(Error_mat, Error)
  wilcox <- wilcox.test(Error, y = Error_main, alternative = "two.sided", paired = TRUE)$p.value
  c(sum(Predicted==Y_test)/length(Y_test), FPR, FNR, AUC, mcnemar, wilcox,  abs((sum(dd) - sum(Y_test))/3), mean(abs(colSums(ddmat)-colSums(testmat))/3), sd(abs(colSums(ddmat)-colSums(testmat))/3))
})



sens <- c("humidity","temperature", "soundavg", "soundpeak")
sens2 <- sapply(4:1, function(sensor) paste(sens[-sensor], collapse = " ") )
tripple <- sapply(sens2, function(sensor){
  X_train_temp <- X_train[,grep(paste0(paste0("(",gsub(" ", "|", sensor),")")), dimnames(X_test)[[2]])]
  X_test_temp <- X_test[,grep(paste0(paste0("(",gsub(" ", "|", sensor),")")), dimnames(X_test)[[2]])]
  data_train_temp <- data.frame(shower = Y_train, X_train_temp)
  data_test_temp <- data.frame(shower = Y_test, X_test_temp)
  
  set.seed(10)
  hmm_model  <- depmix(shower ~ ., data = data_train_temp, nstates = 2, family = binomial(), verbose = TRUE)
  fit <-  fit(hmm_model, verbose = TRUE)
  dd <- predict(fit, data_test_temp, type = "response")
  AUC <- roc(Y_test, c(dd))$auc
  Predicted = dd > 0.5
  TA <- table(True = Y_test, Predicted = Predicted)
  FPR <- TA[1,2]/(TA[1,2]+TA[1,1])
  FNR <- TA[2,1]/(TA[2,1]+TA[2,2])
  TA2 <- table(Main = Predicted_main, Predicted = Predicted)
  mcnemar <- mcnemar.test(TA2)$p.value
  ddmat <- matrix(dd, nrow = 135)
  testmat <- matrix(Y_test, nrow = 135)
  Error <- colSums(ddmat)-colSums(testmat)
  Error_mat <<- cbind(Error_mat, Error)
  wilcox <- wilcox.test(Error, y = Error_main, alternative = "two.sided", paired = TRUE)$p.value
  c(sum(Predicted==Y_test)/length(Y_test), FPR, FNR, AUC, mcnemar, wilcox,  abs((sum(dd) - sum(Y_test))/3), mean(abs(colSums(ddmat)-colSums(testmat))/3), sd(abs(colSums(ddmat)-colSums(testmat))/3))
})

res = rbind("full model" = main, t(solo), t(duo), t(tripple))
knitr::kable(res[,-7], format = "latex", digits = 3)
#saveRDS(res, "res_glmnet.RDS")
#saveRDS(Error_mat, "Error_mat_glmnet.RDS")