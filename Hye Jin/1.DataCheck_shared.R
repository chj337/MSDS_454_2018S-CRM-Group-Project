library(package = "lattice")
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(Hmisc) 
library(psych)
library(corrplot)

library(woeBinning)


orange_small_train <- read.delim("C:/CHJ/MSPA/NW/454/data/KDD/orange_small_train.data", header=TRUE)
orange_small_train_appetency <- read.table("C:/CHJ/MSPA/NW/454/data/KDD/orange_small_train_appetency.labels", quote="\"", comment.char="")
orange_small_train_upselling <- read.table("C:/CHJ/MSPA/NW/454/data/KDD/orange_small_train_upselling.labels", quote="\"", comment.char="")
orange_small_train_churn <- read.table("C:/CHJ/MSPA/NW/454/data/KDD/orange_small_train_churn.labels", quote="\"", comment.char="")

orange_small_train_all <- bind_cols(orange_small_train,orange_small_train_appetency,orange_small_train_upselling,orange_small_train_churn )

summary(orange_small_train_all)

Hmisc::describe(orange_small_train_all)
str(orange_small_train_all)
sum_data2 <- data.frame(sapply(orange_small_train_all, class))
sum_data <- data.frame(sapply(orange_small_train, class))
plot(sum_data, col="red", main="Types of Predictor Variable in 'orage_small_train' Data", lwd = 1, ylim = c(0, 200))


## correlation
num_dt<-select_if(orange_small_train_all, is.numeric) ##numeric variable only
input <- num_dt[,1:174]

## numeric missing replaced by median

f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
test=data.frame(apply(input,2,f))

target_app<-num_dt[,175]
target_ups<-num_dt[,176]
target_chu<-num_dt[,177]

library(dplyr)

dplyr::select(num_dt, V1)
target_app <- select(num_dt, "V1") 
target_ups <- select(num_dt,"V11") 
target_chu <- select(num_dt,"V12")

target_app <- dplyr::select(num_dt, V1)
target_ups <- dplyr::select(num_dt, V11)
target_chu <- dplyr::select(num_dt, V12)

corr_app <-round(cor(test,target_app),5)
corr_app_dt <- data.frame(corr_app)
corr_ups <-round(cor(test,target_ups),5)
corr_ups_dt <- data.frame(corr_ups)
corr_chu <-round(cor(test,target_chu),5)
corr_chu_dt <- data.frame(corr_chu)


library(tibble)
corr_app_dt2<-rownames_to_column(corr_app_dt,var="input")
corr_ups_dt2<-rownames_to_column(corr_ups_dt,var="input")
corr_chu_dt2<-rownames_to_column(corr_chu_dt,var="input")


corr_app_high <- corr_app_dt2 %>% select(input,V1) %>% filter(V1>0.01 | V1< -0.01)  
barplot(corr_app_high$V1, main="correlated variables(±0.01) with target - appetency", width=0.1, names.arg =corr_app_high$input, col="orange", ylim = c(-0.15, 0.10), axis.lty = 1, cex=0.6, cex.names=0.6, las=2)

corr_ups_high <- corr_ups_dt2 %>% select("input","V11") %>% filter(V11>0.015 | V11< -0.015) 
barplot(corr_ups_high$V11, main="correlated variables(±0.015) with target - upsell", width=0.1, names.arg =corr_ups_high$input, col="light blue", ylim = c(-0.15, 0.10), axis.lty = 1, cex=0.6, cex.names=0.6, las=2)


corr_chu_high <- corr_chu_dt2 %>% select("input","V12") %>% filter(V12>0.015 | V12< -0.015)
barplot(corr_chu_high$V12, main="correlated variables(±0.015) with target - upsell", width=0.1, names.arg =corr_chu_high$input, col="yellow", ylim = c(-0.15, 0.10), axis.lty = 1, cex=0.6, cex.names=0.6, las=2)


## CREATE NUMERIC VARS DATA

app_dt <- cbind(test,target_app)
ups_dt <- cbind(test,target_ups)
chu_dt <- cbind(test,target_chu)

app_dt$target <- as.numeric(app_dt$V1 == 1)
ups_dt$target <- as.numeric(ups_dt$V11 == 1)
chu_dt$target <- as.numeric(chu_dt$V12 == 1)

app_dt <-app_dt[,-175]
ups_dt <-ups_dt[,-175]
chu_dt <-chu_dt[,-175]

## CREATE FACTOR VARS DATA

fact_dt<-select_if(orange_small_train_all, is.factor) ##Char variable only

app_fact_dt <- cbind(fact_dt,target_app)
app_fact_dt$target <- as.numeric(app_fact_dt$V1 == 1)
ups_fact_dt <- cbind(fact_dt,target_ups)
ups_fact_dt$target <- as.numeric(ups_fact_dt$V11 == 1)
chu_fact_dt <- cbind(fact_dt,target_chu)
chu_fact_dt$target <- as.numeric(chu_fact_dt$V12 == 1)

app_fact_dt <-app_fact_dt[,-39]
ups_fact_dt <-ups_fact_dt[,-39]
chu_fact_dt <-chu_fact_dt[,-39]

