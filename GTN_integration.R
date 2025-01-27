library(tidyverse)
library(keras)
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




database4 <- database4 %>% mutate(is_weekend = 1*(wday(int_start(intervals), week_start = 1) %in% c(6,7))) 

#now we filter data from 5:00 am to 6:00 am in working days and 6 - 9:00 during weekends
#database4 <- database4 %>% filter((is_weekend == 0 & hour(int_start(intervals)) == 5) | (is_weekend == 1 & hour(int_start(intervals)) >= 6 & hour(int_start(intervals)) <= 8))

# filter only working days from 5:20 to 6:20 and remove the is_weekend variable 
database4 <- database4 %>% filter(is_weekend == 0 & ((hour(int_start(intervals)) == 5 & minute(int_start(intervals)) >= 21)  |  (hour(int_start(intervals)) == 6 & minute(int_start(intervals)) < 14)) ) %>% select(-is_weekend)



create_lags <- function(database, n, colss){
  A <- array(0, dim = c(nrow(database), 2*n+1, length(colss)))
  dat <- database[, colss]
  A[,n+1,] <- as.matrix(dat)
  for(i in 1:n){
    A[,n+1-i,] <- as.matrix(dat %>% mutate_all(lag, n = i))
    A[,n+1+i,] <- as.matrix(dat %>% mutate_all(lead, n = i))
  }
  return(A)
}

A <- create_lags(database4, 12, 3:14)
X <- A[minute(int_start(database4$intervals)) >=25 | minute(int_start(database4$intervals)) <10,,]
Y <- 1*database4$shower[minute(int_start(database4$intervals)) >=25 | minute(int_start(database4$intervals)) <10]


X_train <- X[1:(135*180),,]
X_test <- X[(135*180+1):nrow(X),,]
Y_train <- Y[1:(135*180)]
Y_test <- Y[(135*180+1):nrow(X)]


library(reticulate)
XT = aperm(X_train, c(1,3,2))
YT = Y_train+1
XTE = aperm(X_test, c(1,3,2))
YTE = Y_test+1
np$save("Y_train.npy",r_to_py(YT))
np$save("Y_test.npy",r_to_py(YTE))
np$save("X_train.npy",r_to_py(XT))
np$save("X_test.npy",r_to_py(XTE))



files <- list.files(".//GNT//", pattern = ".npy")
names(files) <- gsub(".npy", "",gsub("Shower", "", files))
main <- files[7]
files <- files[-7]

dd <- np$load(paste0(".//GNT//", main))
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
res <- main

transformer <- function(dd){
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
  
}

for (f in files){
  dddd = np$load(paste0(".//GNT//", f))
  tr <- transformer(dddd)
  res = rbind(res, tr)
}

rownames(res) <- c("full model" ,names(files))
colnames(Error_mat) <- c("full model" ,names(files))

#saveRDS(res, "res_GNT2.RDS")
#saveRDS(Error_mat, "Error_mat_GNT2.RDS")

dddd = np$load("result.npy")
tr <- transformer(dddd)
res = rbind(res, Transformer = tr)

labels <- tibble(data.frame(x= c(0.95, 3.5, 8.5, 13.5, 16.05), y = c(40, 40, 40, 40, 40), text = c("Full Model", "Single Sensor", "Dual Sensor", "Tripple Sensor", "GTF")))

Errordat <- as.data.frame(Error_mat) 
colnames(Errordat) <- rownames(res)
Errordat <- gather(tibble(Errordat), key = "model", value = "error")
Errordat$model <- factor(Errordat$model, levels = rownames(res))
Errordat %>% ggplot() +
  geom_boxplot(aes(x = model, y = error, fill = model))  +
  theme(axis.text.x = element_blank()) + labs(title = "Error distribution of different models", x = "Model", y = "Error") +
  geom_label(data = labels, aes(x = x, y = y, label = text), size = 5) +
  annotate("rect", xmin = 1.5, xmax = 5.5, ymin = -30, ymax = 50, alpha = .05,fill = "blue") +
  annotate("rect", xmin = 5.5, xmax = 11.5, ymin = -30, ymax = 50, alpha = .05,fill = "red") +
  annotate("rect", xmin = 11.5, xmax = 15.5, ymin = -30, ymax = 50, alpha = .05,fill = "green") +
  annotate("rect", xmin = 15.5, xmax = 16.6, ymin = -30, ymax = 50, alpha = .05,fill = "#800080") +
  annotate("rect", xmin = 0.4, xmax = 1.5, ymin = -30, ymax = 50, alpha = .05,fill = "yellow") +
  coord_cartesian(ylim = c(-25,45), xlim = c(1,16)) +
  scale_fill_manual(values=c("#ffffa6", "#bfcfff", "#809fff", "#4d7aff", "#3655b3","#fff5f5", "#ffe6e6", "#ffcccc", "#ff9999", "#e58989", "#b36b6b", 
                             "#e6ffe6", "#ccffcc", "#99ff99", "#6bb36b", "#d580ff"))

                                                                                                                                                                                  
