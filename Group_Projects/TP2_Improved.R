remove(list=ls())
#Needed Packages
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) ) 
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed <- c('data.table', 'lubridate', 'tidyverse', 
            'biglm', 'randomForest', 'geosphere', 'xgboost', 'gbm', 'lubridate')  
installIfAbsentAndLoad(needed)

#See some people doing it with read.csv. read.csv is slow to read a large dataset. 
#Fread is fast and convenient! 
set.seed(1)
NY_Train <- fread("train.csv", nrows = 3000000)  #Read 3M rows of the trainig dataset
NY_Test <- read.csv("test.csv")
summary(NY_Train)
summary(NY_Test)

#Remove key column:
NY_Train <- NY_Train %>% select(-key)
NY_Test <- NY_Test %>% select(-key)

#Create place holder column in NY_test for fare_amount: 
NY_Test$fare_amount <- NA

#Drop NAs in train data:
sum(is.na(NY_Train)) 
NY_Train <- na.omit(NY_Train)
#Filter out unreasonable price data:
NY_Train<- filter(NY_Train, fare_amount > 0, fare_amount < 700) 
#Filter our unreaonable passenger data:
NY_Train <- filter(NY_Train,NY_Train$passenger_count < 8)
NY_Train%>%
  filter(passenger_count==0)%>%
  nrow()

NY_Train <- filter(NY_Train, passenger_count >0, passenger_count <= 10)

#Filter out geo data that is not in NYC boroughs: 
NY_Train<-NY_Train%>%  
  filter(pickup_longitude > -80 & pickup_longitude < -70) %>%
  filter(pickup_latitude > 35 & pickup_latitude < 45) %>%
  filter(dropoff_longitude > -80 & dropoff_longitude < -70) %>%
  filter(dropoff_latitude > 35 & dropoff_latitude < 45)

#Use package lubridate to extract various time components: 
combined<-data.frame(rbind(NY_Train, NY_Test))

combined<-combined%>%
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    year = as.factor(year(pickup_datetime)),
    month = as.factor(month(pickup_datetime)),
    day = as.numeric(day(pickup_datetime)),
    dayOfWeek = as.factor(wday(pickup_datetime)),
    hour = as.numeric(hour(pickup_datetime)),
    timeOfDay = as.factor(ifelse(hour >= 3 & hour < 9,
                                 "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                   ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
  )%>%
  select(-pickup_datetime)

#Picking out exact locations:
#jfk
jfk_lat<-40.6413
jfk_long<--73.7781
jfk<-c(jfk_long, jfk_lat)
#newark
nwk_lat<-40.6895
nwk_long<--74.1745
nwk<-c(nwk_long, nwk_lat)
#laguardia
lag_lat<-40.779
lag_long<--73.8740
lag<-c(lag_long, lag_lat)
#MSG
msg_lat<-40.7505
msg_long<--73.9934
msg<-c(msg_long, msg_lat)
#times square
ts_lat<-40.7589
ts_long<--73.9851
ts<-c(ts_long, ts_lat)
#freedom tower
freedom_lat<-40.7127
freedom_long<--74.0134
freedom<-c(freedom_long, freedom_lat)
#empire state building
esb_lat<-40.7484
esb_long<--73.9857
esb<-c(esb_long, esb_lat)
#grand central
grand_lat<-40.7527
grand_long<--73.9772
grand<-c(grand_long, grand_lat)
#bronx
bronx_lat <- (40.837048 * pi)/180
bronx_long <- (-73.865433 * pi)/180
bronx<-c(bronx_long, bronx_lat)
nyc<-c(-74.0063889, 40.7141667)

combined<-combined%>%
  mutate(
    dist = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude), r = 6371),
    to_jfk = distHaversine(cbind(pickup_longitude, pickup_latitude), jfk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), jfk, r = 6371),
    to_nkw = distHaversine(cbind(pickup_longitude, pickup_latitude), nwk, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nwk, r = 6371),
    to_lag = distHaversine(cbind(pickup_longitude, pickup_latitude), lag, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), lag, r = 6371),
    to_msg = distHaversine(cbind(pickup_longitude, pickup_latitude), msg, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), msg, r = 6371),
    to_ts = distHaversine(cbind(pickup_longitude, pickup_latitude), ts, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), ts, r = 6371),
    to_freedom = distHaversine(cbind(pickup_longitude, pickup_latitude), freedom, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), freedom, r = 6371),
    to_grand = distHaversine(cbind(pickup_longitude, pickup_latitude), grand, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), grand, r = 6371),
    to_bronx = distHaversine(cbind(pickup_longitude, pickup_latitude), bronx, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), bronx, r = 6371),
    to_nyc = distHaversine(cbind(pickup_longitude, pickup_latitude), nyc, r = 6371) + distHaversine(cbind(dropoff_longitude, dropoff_latitude), nyc, r = 6371)
  )

#Now resplit train and test: 
NY_Test <- combined[is.na(combined$fare_amount), ]
NY_Train <- combined[!is.na(combined$fare_amount), ]

#Average fare price: 
print(paste("The mean fair price in the training data set is $", round(mean(NY_Train$fare_amount),2), ".", sep=""))

#Visualize data:
m<-geom_point(stat = "summary", fun.y = "mean", col = "blue", size = 5)
med<-geom_point(stat = "summary", fun.y = "median", col = "red", size = 5)
manLegend<-scale_color_manual(name = "Summary Stat", values = c("mean"="blue", "median"="red"))

#Year and price:
ggplot(NY_Train, aes(as.factor(year), fare_amount))+
  m+
  med+
  manLegend+
  ggtitle("Fare Amount by Year")+
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom")

#Distribution of prices: 
ggplot(NY_Train, aes(fare_amount)) + 
  geom_histogram(breaks=seq(0, 100, by=2), 
                 col="red", 
                 fill="green", 
                 alpha=.2)

#Fare amount by day of week: 
ggplot(NY_Train, aes(as.factor(dayOfWeek), fare_amount))+
  m+
  med+
  manLegend+
  ggtitle("Fare Amount by Day of Week")+
  theme(plot.title = element_text(hjust = .5), legend.position = "bottom")

#Passengers by time of day: 
ggplot(NY_Train, aes(timeOfDay, fill = timeOfDay))+
  geom_bar(stat = "count", aes(y = ..count..))+
  scale_x_discrete(limits=c("Morning", "Mid-Day", "Evening", "Night"))+
  ggtitle("Number of Passengers by timeOfDay")+
  theme(plot.title = element_text(hjust = .5))

#Model: 
#Create a validation set: 
size = floor(.8*nrow(NY_Train))

xx <-sample(1:nrow(NY_Train), size)
NY_Valid<-NY_Train[-xx,]
NY_Train<-NY_Train[xx,]

#Convert to matrices for XGBoost algorithm to work:
dvalid <- xgb.DMatrix(data = data.matrix(NY_Valid[,-1]), label = NY_Valid[,1])
dtrain <- xgb.DMatrix(data = data.matrix(NY_Train[,-1]), label = NY_Train[,1])
dtest<-xgb.DMatrix(data = data.matrix(NY_Test[,-1]))


p <- list(objective = "reg:linear",
          eval_metric = "rmse",
          max_depth = 8 ,
          eta = .05, 
          subsample=1,
          colsample_bytree=0.8,
          num_boost_round=250,
          nrounds = 500)

#Train the model:
set.seed(1)
m_xgb <- xgb.train(p, dtrain, p$nrounds, list(val=dvalid), print_every_n = 10, early_stopping_rounds = 10)

#Feature importance: 
(impt_matrix <- xgb.importance(colnames(dtrain), model = m_xgb))

xgb.plot.importance(impt_matrix)

#Make predictions:
preds <- predict(m_xgb, dtest)

#Make Kaggle Submission to get RMSE:
read.csv("sample_submission.csv")%>%
  mutate(fare_amount = preds)%>%
  write.csv("team12_sub.csv", row.names = F)

#Test RMSE: Obtained by submitting to Kaggle
RMSE <- 3.07516
print(paste("The RMSE for our model is $", round(RMSE,2), ".", sep=""))
print(paste("This means that, on average, the model misestimated the fare price by $" ,round(RMSE,2), ".", sep="")) 



  