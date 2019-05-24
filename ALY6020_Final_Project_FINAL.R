#getwd()
#setwd("~/Predictive Analytics")

library(corrplot)
library(data.table)
library(reshape2)
library(caret)
library(caTools)
library(tibble)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tseries)
library(zoo)
library(forecast)
library (gtrendsR)
detach("package:plyr", unload=TRUE) 



# Read in the data sets 

salesTrain <- read.csv("salesTrain.csv")
salesTest <- read.csv("test.csv")
sample_submission <- read.csv("sample_submission.csv")
items <- read.csv("items.csv")
categories <- read.csv("item_categories.csv")
shops <- read.csv("shops.csv")

# Merge salesTrain with items on item_id to add the category id to the data set.
tmp <-merge(salesTrain, items, by="item_id")
salesTrain <- tmp

# Create a new column called item revenue
salesTrain$item_revenue <- salesTrain$item_cnt_day * salesTrain$item_price
hist(salesTrain$item_revenue)
sum(salesTrain$item_revenue)/1000000    #Millions of Rubles in Revenue  (3.4B)

# Save file by writing as a .csv
#  write.csv(salesTrain, file = "salesTrain.csv")
#  write.csv(MyData, file = "MyData.csv")
#  str(salesTrain)
#  temp <- read.csv("salesTrain.csv")
#  dim(temp)
#  temp <- temp[ -c(1) ]

# 3M lines is too large a dataset to iterate quickly.   I will create a smaller 
# dataset called 'fast'with randomly selected data with 300,000 rows or 10% 
# of the full training set.
# library(caTools)
# set.seed(200)

# split = sample.split(salesTrain, SplitRatio = 0.90)
# big= subset(salesTrain, split == TRUE)
#  fast = subset(salesTrain, split==FALSE)

# Now back to the main data sets...what are the column names of each df?
colnames(salesTrain)
colnames(salesTest)
colnames(sample_submission)
colnames(shops)
colnames(items)
colnames(categories)
hist(items$item_category_id)
max(sample_submission$ID)



summary(salesTrain)
str(salesTrain)
# Which items are the highest count items?   
# Need to detach plyr so commands aren't confused and duplicated

detach("package:plyr", unload=TRUE) 

# Which shops sell the most number of items?   Shops31,25,54,28,42,57
tmp <- salesTrain %>%
  group_by(shop_id ) %>%
  summarize(item_sum = sum(item_cnt_day)) %>%
  arrange(desc(item_sum))
head(tmp, n = 10)
tmp


# Which items are sold in the highest quantities?  20949  The Corporate T-Shirt; most items by far.
tmp <- salesTrain %>%
  group_by(item_id, item_name_translated) %>%
  summarize(item_sum = sum(item_cnt_day))%>%
  arrange(desc(item_sum))
tmp <- head(tmp, n = 10)
tmp

# Which categories are the highest count categories?  This does not tell us much. 
tmp <- salesTrain %>%
  group_by(item_category_id)%>%
  summarize(count = n())%>%
  arrange(desc(count))
tmp <- head(tmp, n = 15)
tmp
hist(tmp$item_category_id)



# Which items sell in the highest quantities?    Corpporate T-Shirts, Diablo III, Grand Theft Auto V, etc.
tmp <- salesTrain %>%
  group_by(item_id, item_name_translated) %>%
  summarize(sum_itemcount = sum(item_cnt_day, sum_itemcount = sum(item_cnt_day, tmp$item_name_translated))) %>%
  arrange(desc(sum_itemcount)) %>%
  top_n(10)
tmp <- head(tmp, n = 10)
tmp 

# Take out item name translated
tmp<- tmp[, -2]
names(tmp)<- c("item_id", "sum_itemcount")

tmp$item_id <- as.factor(tmp$item_id)


ggplot(tmp, aes(x = item_id, y = sum_itemcount)) +
  geom_bar(stat = "identity") +  ggtitle("Items with the highest item counts sold") +
  xlab("Item Id") + ylab("Total Item Counts Sold")


# Which shops generate the most revenue?  Shops 31,25,28,42,54,27,57,12,6,18

tmp <- salesTrain %>%
  group_by(shop_id) %>%
  summarize(sum_revenue = sum(item_revenue, item_sum = sum(item_cnt_day))) %>%
  arrange(desc(sum_revenue))

tmp
ggplot(tmp, aes(shop_id, sum_revenue)) + geom_bar(stat = "identity") +
  ggtitle("Total Revenue by Shop Number") +
  xlab("Shop Number") + ylab("Total Revenue")



# Create Bins of prices to see distribution.
# The vast majority of prices are below 50,000.   Probably won't pursue any pricing questions
tmp <- unique(salesTrain$item_price)
tmp <- as.numeric(tmp)
max(tmp)
tmp <- cut(tmp, breaks = c(0,100, 500, 1000,2000,5000,50000,100000,250000,315000))
summary(tmp)
barplot(summary(tmp), main = "Distribution of Unique Item Prices",
        xlab="Price Bins",
        ylab="Count of Unique Prices") 




# Which date blocks have the most revenue?
# Shows seasonal high points at end of year- a retail-like pattern.
# Not really growing over time.
tmp <- salesTrain %>%
  group_by(date_block_num) %>%
  summarize(sum_revenue = sum(item_revenue)) %>%
  arrange(date_block_num)
tmp

ggplot(tmp, aes(date_block_num, sum_revenue)) + geom_line() +
  ggtitle("TOTAL Monthly Revenue, by Date Block Number (Month)") +
  xlab("Month number") + ylab("Monthly Revenue")

# Which date blocks have the highest item counts?
# Shows seasonal high points at end of year- a retail-like pattern.
# Numbers of items sold are decreasing over time.

tmp <- salesTrain %>%
  group_by(date_block_num) %>%
  summarize(item_sum = sum(item_cnt_day)) %>%
  arrange(date_block_num)

tmp


ggplot(tmp, aes(date_block_num, item_sum)) + geom_line() +
  ggtitle("TOTAL Item Counts, by Date Block Number (Month)") +
  xlab("Month number") + ylab("Number of Items Sold Per Month")




# Which shop_id's have the most item counts?
# Only a few shops sell high numbers of items:  6,7,12,15,16,18,19,21,22,24
tmp <- salesTrain %>%
  group_by(item_id  ,item_name_translated, shop_id) %>%
  summarize(item_sum = sum(item_cnt_day)) %>%
  arrange(desc(item_sum))
head(tmp, n = 50)
tmp

ggplot(tmp, aes(shop_id, item_sum)) + geom_bar(stat = "identity") +
  ggtitle("Item Counts by Shop Number") +
  xlab("Shop Number") + ylab("Item Counts")


# Which 50 items generate the most revenue?  Sony PlayStation 4, Grand Theft Auto V top the list
tmp <- salesTrain %>%
  group_by(item_id, item_name_translated ) %>%
  summarize(sum_revenue = sum(item_revenue)) %>%
  arrange(desc(sum_revenue))
tmp <- head(tmp, n = 50)
tmp
max(tmp$sum_revenue)




# _________________________________________________

# Let's subset out the data for shop 31, which has the most sales

shop31 <- salesTrain %>% filter(shop_id==31)


# Which 50 items generate the most revenue, in Shop 31? PlayStation 4, Grand Theft Auto V
tmp <- shop31 %>%
  group_by(item_id, item_name_translated ) %>%
  summarize(sum_revenue = sum(item_revenue)) %>%
  arrange(desc(sum_revenue))
tmp <- head(tmp, n = 50)
tmp
max(tmp$sum_revenue)


# Which date blocks have the most revenue?
# Shows seasonal high points at end of year- a retail-like pattern.
# Not really growing over time.
tmp <- shop31 %>%
  group_by(date_block_num) %>%
  summarize(sum_revenue = sum(item_revenue)) %>%
  arrange(date_block_num)
tmp

ggplot(tmp, aes(date_block_num, sum_revenue)) + geom_line() +
  ggtitle("Shop 31 Monthly Revenue, by Date Block Number (Month)") +
  xlab("Month number") + ylab("Monthly Revenue")
################################## Aditi's Section #############################################

#Revenue by shop number
salesTrain$shop_id <- as.factor(salesTrain$shop_id)
rev_shop <- aggregate(salesTrain$item_revenue, list(salesTrain$shop_id), sum)
barplot(rev_shop$x, xlab = "Shop ID", ylab = "Total Revenue", names.arg = rev_shop$Group.1, main = "Total Revenue By Shop")

#Total item count by shop number
item_shop <- aggregate(salesTrain$item_cnt_day, list(salesTrain$shop_id), sum)
barplot(item_shop$x, xlab = "Shop ID", ylab = "Total # Items", names.arg = item_shop$Group.1, main = "Item Count By Shop")






##############################    Lucinda  #######################################################################
# How about the Revenues of the Top 5 Shops?  31,42,25,54,28    
Top5shops <- salesTrain %>% filter(shop_id==31 | shop_id==42 | shop_id==25 | shop_id==54 | shop_id==28) 

keeps <- c("shop_id", "item_id", "date_block_num", "item_cnt_day", "item_revenue" )

Top5shops <- Top5shops[keeps]  
head(Top5shops,10)
length(unique(Top5shops$shop_id))   # Correct- we have 5 shops: 31,42,25,54,28

Top5shops <- Top5shops %>%
  group_by(date_block_num) %>%
  summarize(sum_revenue = sum(item_revenue)) %>%
  arrange(date_block_num)
tmp

ggplot(tmp, aes(date_block_num, sum_revenue)) + geom_line() +
  ggtitle("Top 5 Shops Monthly Revenue, by Date Block Number (Month)") +
  xlab("Month number") + ylab("Monthly Revenue")


#  If we have time, add Average Price Per Item over time (date_block_num)



#  Let's try the model tseries to predict  Item Counts using time series algorithm- tseries
#  We need to make the sloping, seasonal item counts into a more stationary plot
#  The plot looks pretty straight  
#  I tried differentiating twice but it did not make the model much better, so for now
#  let's keep it at on differentiation.    









# ______________________Create the time series model for sums of items sold each month_____________________________________

# # First get the data for the Top 5 shops, group by date_block_num (Month # for time series)
# Plot total item count, by date_block_num,
# then  keep only the columns we care about.



# In the code below, we'll train the model on date blocks 0-31.  We'll predict blocks 32-34 
# and compare those predictions to the actual item sum counts for blocks 32-34 (August - October 2015).
par(mfrow=c(1,1))   

Top5shops <- salesTrain %>% filter(shop_id==31 | shop_id==42 | shop_id==25 | shop_id==54 | shop_id==28) 

keeps <- c("shop_id", "item_id", "date_block_num", "item_cnt_day", "item_revenue" )

Top5shops <- Top5shops[keeps]  
head(Top5shops,10)
length(unique(Top5shops$shop_id))   # Correct- we have 5 shops: 31,42,25,54,28

Top5shops <- Top5shops %>%
  group_by(date_block_num) %>%
  summarize(item_sum = sum(item_cnt_day)) %>%
  arrange(date_block_num)

Top5shops <- as.data.frame(Top5shops)


ggplot(Top5shops, aes(date_block_num, item_sum)) + geom_line() +
  ggtitle("Top 5 shops Monthly Item Counts, by Date Block Number (Month)") +
  xlab("Month number") + ylab("Number of Items Sold Per Month")


# Notice that the item counts slope downward over time. Yet Revenues are flat (except for seasonality).
# To make the data series more stationary, we differentiate it once.
# This mostly gets rid of the slope.  When I tried differentiating twice,
# The model did not get better- so for now we'll leave differentiation at d = 1.
library("tseries")
par(mfrow=c(1,2)) 
plot(Top5shops$item_sum, type = "l", col = "blue", lwd = 2, xlab = "Month Number",
     ylab = "Monthly Items in Top 5 Shops", main = "Items Sold Each Month in Top 5")

plot(diff(Top5shops$item_sum),type = "l", col = "blue", lwd = 2, 
     xlab = "Month Number",
     ylab = "Monthly Items Sold Top 5 Shops", main = "First Diff of Monthly Items Graph")    

# d = 1 because we differentiated once

par(mfrow=c(1,1)) 

# Format the data as a time series and create item_sum_ts which is the properly
# formatted time series vector.  

item_sum_ts <- ts(Top5shops$item_sum, start = c(2013,1),frequency = 12) 
item_sum_ts <- as.ts(item_sum_ts) 
item_sum_ts#Creates series formated by Jan, Feb etc. columns and rows by year!

# Create Train Data set January 2013 to July 2015, excluding August, September and October 
item_sum_ts_train <- ts(Top5shops$item_sum, start = c(2013,1),end = c(2015,7),frequency = 12) 
item_sum_ts_train <- as.ts(item_sum_ts_train) 
item_sum_ts_train 

# Create last 15 entries, for plotting later  
library("zoo")
last3<-tail(as.zoo( item_sum_ts),3)
last3

# Create the test set August to October 2015 (choose one of the two formats below)
#  item_sum_ts_test <- ts(Top5shops$item_sum, start = c(2015,8),end = c(2015,10), frequency = 12)
item_sum_ts_test <- item_sum_ts[32:34]
item_sum_ts_test <- as.ts(item_sum_ts_test)
class(item_sum_ts_test)  
item_sum_ts_test

# Map out the seasonality, trend and residual components of the item count sales
components <- stl(item_sum_ts, s.window = "period")
plot(components, col = 'blue', main = "Seasonal, trend and remainder components of Total Items for Top 5 Shops")

# Seasonplot plots all 2.5 years of data in the train dataset.
seasonplot(item_sum_ts_train, xlab = "Month ",
           ylab = "Items Sold Each Month in Shop 31", main = "Items Sold Each Year, by Month,in Shop 31",
           type = c("b"),pch=1,col = 1:3)
legend("top", 
       legend = c("2013", "2014","2015"),
       col = 1:3, pch=1)





# Use auto.arima to choose p,d and q for time series ARIMA model.
# Model chosen is (0,0,0) for data and (0,1,0) for seasonality
fit1 <- auto.arima(item_sum_ts_train, D = 1,  seasonal = TRUE,  trace=TRUE)


#    fit2 <- auto.arima(item_sum_ts_train, d = 1,  seasonal = TRUE,  trace=TRUE)   DID NOT WORK
#  auto.arima(tts, D=1, stepwise=FALSE, seasonal=TRUE,approximation=FALSE,trace=TRUE)  

summary(fit1)


# forecasting  
forecast1 <- as.vector(c(0,0,0)) 
forecast1 <- forecast(fit1,3)   
forecast1
last3


#  predict(fit2, n.ahead=3)  
forecast(fit1, 3)
plot(forecast(fit1, 3), main = "Forecast vs. Actual Item Counts" )  
lines(last3,type="l",col="red",lwd = 3 )
legend("top", legend = c("Red = Actual", "Blue = Forecast"), 
       col = c("red","blue"))



# Use accuracy() for several types of error measurements

accuracy(fit1)  # Shows only the error stats ME, RSME, MAE, MPE, MAPE, MASE, ACF1
accuracy(forecast1, last3)





#    #######    ##################   NOW try making my own ARIMA models to see if I can beat auto.arima RMSE



# Apply the ARIMA model for p,d,q = c(0,1,0)   


fit31 <- arima(item_sum_ts_train, c(0,1,0), 
                 seasonal = list(order = c(0,1,0), period = 12))
summary(fit31)
fit31
accuracy(fit31)



#  predict(fit2, n.ahead=3)  
forecast31 <- as.vector(c(0,0,0)) 
forecast31 <- forecast(fit31,3)   
forecast31
last3
plot(forecast(fit1, 3), main = "Forecast vs. Actual Item Counts" )  
lines(last3,type="l",col="red",lwd = 3 )
legend("top", legend = c("Red = Actual", "Blue = Forecast"), 
       col = c("red","blue"))



# Use accuracy() for several types of error measurements

accuracy(fit31)  # Shows only the error stats ME, RSME, MAE, MPE, MAPE, MASE, ACF1
accuracy(forecast31, last3)



################################## Aditi's Section #############################################

#Revenue by shop number
salesTrain$shop_id <- as.factor(salesTrain$shop_id)
rev_shop <- aggregate(salesTrain$item_revenue, list(salesTrain$shop_id), sum)
barplot(rev_shop$x, xlab = "Shop ID", ylab = "Total Revenue", names.arg = rev_shop$Group.1, main = "Total Revenue By Shop")

#Total item count by shop number
item_shop <- aggregate(salesTrain$item_cnt_day, list(salesTrain$shop_id), sum)
barplot(item_shop$x, xlab = "Shop ID", ylab = "Total # Items", names.arg = item_shop$Group.1, main = "Item Count By Shop")

#Splitting the "date" column and creating a month/year column
library(stringr)
date <- data.frame(do.call('rbind', strsplit(as.character(salesTrain$date),'.',fixed=TRUE)))
# View(date)
salesTrain$"day" <- date$X1
salesTrain$"month" <- date$X2
salesTrain$"year" <- date$X3
salesTrain$"year/month" <- paste(date$X3,date$X2, sep = "/")

rev_year_month <- aggregate.data.frame(salesTrain$item_revenue, list(salesTrain$`year/month`), sum)

#ARIMA Model by hand
acf(rev_year_month$x)
acf(diff(log(rev_year_month$x)))
plot(pacf(diff(log(rev_year_month$x))))

plot(diff(log(rev_year_month$x)), type = "l", col = "blue", main = "First Diff of Monthly Revenue", xlab = "Month Number", ylab = "diff(log(revenue))")
plot(rev_year_month$x, type = "l", col = "blue", main = "Monthly Revenue for All Shops", xlab = "Month Number", ylab = "Revenue")

fit <- arima(log(rev_year_month$x), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
pred <- predict(fit, n.ahead = 3)
pred1 <- 2.718^pred$pred
datawide <- ts(rev_year_month$x, frequency = 12)
library(forecast)
fit1 <- arima(log(datawide), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
pred2 <- predict(fit1, n.ahead = 3)
pred3 <- 2.718^pred2$pred
data1 <- head(pred3, 12)

data1    #Forecasted for next 3 months: Nov, Dec, and Jan


#Auto ARIMA Model
interest_ts <- ts(rev_year_month$x, start = c(2013, 1), frequency = 12)
components <- stl(interest_ts, s.window = 12)
train_ts <- window(interest_ts, start = c(2013,1), end = c(2015,7))
test_ts <- window(interest_ts, start = c(2015,7), end = c(2015,9))

fit = auto.arima(train_ts, D = 1, seasonal = TRUE, trace = TRUE)

fcast <- forecast(fit, h=6)

plot(fcast, main="Forecast vs. Actual Revenue", ylab = "Total Revenue", xlab = "Month")
lines(test_ts, col = "red")

#RMSE values very high, we believe it could be because revenues already start in the millions
#Prices for various items vary greatly
accuracy(fcast, test_ts)
#Predictions are within the 95% confidence interval
fcast
#The auto ARIMA model produced a MUCH higher forecasting


#################################   PAVAN'S SECTION   ##################################


#install.packages('sqldf')
#install.packages("hydroGOF")

library(sqldf)
library(forecast)
library(ggplot2)

# loading data

# arranging according to shop and total_revenue per month
salesTrain <- sqldf('select date_block_num as Month,shop_id,sum(item_price) as total_rev
                     from salesTrain group by date_block_num,shop_id
                     order by shop_id asc')


# shop 4

#Shop_4 
shop_4 = sqldf('select total_rev from salesTrain where shop_id = 4')
plot(shop_4$total_rev, type='l', main = 'shop 4 revenue per month')

shop4_train <- shop_4[1:25,]
shop4_train

shop4_test <- shop_4[26:34,]
shop4_test

shop4_train_ts = ts(shop4_train, frequency = 12)

decomp = stl(shop4_train_ts, s.window = 1)
plot(decomp)

acf(shop4_train_ts)
pacf(shop4_train_ts)

fit_4 = auto.arima(shop4_train_ts)
fcast_4 = forecast(fit_4)
fcast_4
plot(fcast_4,main = 'shop 4 revenue per month')

df_rmse <- fcast_4$mean
df_rmse <- data.frame(df_rmse)
df_rmse_sub<- df_rmse[1:9,]
df_rmse_sub
df_rmse_sub2 = df_rmse[10:12,]


RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(shop4_test, df_rmse_sub)

names(shop_4)<-"revenue"
names(df_rmse_sub2)<-"revenue"

shop_4<- rbind(shop_4,df_rmse_sub2)
shop_4_ts = ts(shop_4$revenue, frequency = 12)
mean(shop_4$revenue)


decomp = stl(shop_4_ts, s.window = 1)
plot(decomp)

acf(shop_4_ts)
pacf(shop_4_ts)

fit_4 = auto.arima(shop_4_ts)
fcast_4 = forecast(fit_4)
plot(fcast_4,main = 'shop 4 revenue per month')
#error <- (shop4_test$total_rev - df)




# shop 31 

shop_31 = sqldf('select total_rev from salesTrain where shop_id = 31')
plot(shop_31$total_rev, type='l', main = 'shop 31 revenue per month')

shop31_train <- shop_31[1:25,]
shop31_train

shop31_test <- shop_31[26:34,]
shop31_test

shop31_train_ts = ts(shop31_train, frequency = 12)

decomp = stl(shop31_train_ts, s.window = 1)
plot(decomp)

acf(shop31_train_ts)
pacf(shop31_train_ts)

fit_31 = auto.arima(shop31_train_ts)
fcast_31 = forecast(fit_31)
fcast_31
plot(fcast_31,main = 'shop 31 revenue per month')

df_rmse <- fcast_31$mean

df_rmse <- data.frame(df_rmse)
df_rmse_sub<- df_rmse[1:9,]
df_rmse_sub
df_rmse_sub31 = df_rmse[10:12,]


RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(shop31_test, df_rmse_sub)





#shop 25
shop_25 = sqldf('select total_rev from salesTrain where shop_id = 25')
plot(shop_25$total_rev, type='l', main = 'shop 25 revenue per month')

shop25_train <- shop_25[1:25,]
shop25_train

shop25_test <- shop_25[26:34,]
shop25_test

shop25_train_ts = ts(shop25_train, frequency = 12)

decomp = stl(shop25_train_ts, s.window = 1)
plot(decomp)

acf(shop25_train_ts)
pacf(shop25_train_ts)

fit_25 = auto.arima(shop25_train_ts)
fcast_25_rmse = forecast(fit_25)
fcast_25_rmse
plot(fcast_25_rmse,main = 'shop 25 revenue per month')

df_rmse <- fcast_25_rmse$mean
df_rmse <- data.frame(df_rmse)
df_rmse_sub<- df_rmse[1:9,]
df_rmse_sub
df_rmse_sub25 = df_rmse[10:12,]


RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(shop25_test, df_rmse_sub25)


names(shop_25)<-"revenue"
names(df_rmse_sub25)<-"revenue"

shop_25<- rbind(shop_25,df_rmse_sub25)
shop_25_ts = ts(shop_25$revenue, frequency = 12)
mean(shop_25$revenue)


decomp = stl(shop_25_ts, s.window = 1)
plot(decomp)

acf(shop_25_ts)
pacf(shop_25_ts)

fit_25 = auto.arima(shop_25_ts)
fcast_25 = forecast(fit_25)
plot(fcast_25,main = 'shop 25 revenue per month')


#28

shop_28 = sqldf('select total_rev from salesTrain where shop_id = 28')
plot(shop_28$total_rev, type='l', main = 'shop 28 revenue per month')

shop28_train <- shop_28[1:25,]
shop28_train

shop28_test <- shop_28[26:34,]
shop28_test

shop28_train_ts = ts(shop28_train, frequency = 12)

decomp_28 = stl(shop28_train_ts, s.window = 1)
plot(decomp_28)


acf(shop28_train_ts)
pacf(shop28_train_ts)

fit_28 = auto.arima(shop28_train_ts)
fcast_28 = forecast(fit_28)
plot(fcast_28,main = 'shop 28 revenue per month')

df_rmse_28 <- fcast_28$mean
df_rmse_28 <- data.frame(df_rmse_28)
df_rmse_sub28<- df_rmse_28[1:9,]
df_rmse_sub28
df_rmse_sub28 = df_rmse_28[10:12,]


RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(shop28_test, df_rmse_sub28)

names(shop_28)<-"revenue"
names(df_rmse_sub28)<-"revenue"

shop_28<- rbind(shop_28,df_rmse_sub28)
shop_28_ts = ts(shop_28$revenue, frequency = 12)
mean(shop_28$revenue)


decomp = stl(shop_28_ts, s.window = 1)
plot(decomp)

acf(shop_28_ts)
pacf(shop_28_ts)

fit_28 = auto.arima(shop_28_ts)
fcast_28 = forecast(fit_28)
fcast_28
plot(fcast_28,main = 'shop 28 revenue per month')

##################





















