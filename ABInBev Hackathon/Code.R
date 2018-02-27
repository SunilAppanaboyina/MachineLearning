
demographics <- read.csv("demographics.csv")
length(unique(demographics$Agency))
str(demographics)
sapply(demographics[,2:3],function(x) sum(!is.finite(x)))
sum(is.na(demographics$Agency))

event <- read.csv("event_calendar.csv")
str(event)
sapply(event,function(x) sum(!is.finite(x)))
library(stringi)
stri_sub(event[,"YearMonth"],5,2) <- " "
event$YearMonth <- as.Date(paste(event[,"YearMonth"]," 01",sep=""),"%Y %m %d")
event$Events <- rowSums(event[,-1])

histVolume <- read.csv("historical_volume.csv")
str(histVolume)
length(unique(histVolume$Agency))
sapply(histVolume[,1:2],function(x) sum(is.na(x)))
sapply(histVolume[,3:4],function(x) sum(!is.finite(x)))
demographics[!(unique(demographics$Agency) %in% unique(histVolume$Agency)),] # Agency_06 & Agency_14 not present in histVolume

sodaSales <- read.csv("industry_soda_sales.csv")
str(sodaSales)
sapply(sodaSales,function(x) sum(!is.finite(x)))

indusVolume <- read.csv("industry_volume.csv")
str(indusVolume)
sapply(indusVolume,function(x) sum(!is.finite(x)))

priceSalesPromo <- read.csv("price_sales_promotion.csv")
str(priceSalesPromo)
length(unique(priceSalesPromo$Agency))
sapply(priceSalesPromo[,1:2],function(x) sum(is.na(x)))
sapply(priceSalesPromo[,-c(1,2)],function(x) sum(!is.finite(x)))
demographics[!(unique(demographics$Agency) %in% unique(priceSalesPromo$Agency)),] # Agency_06 & Agency_14 not present in histVolume
stri_sub(priceSalesPromo[,"YearMonth"],5,2) <- " "
priceSalesPromo$YearMonth <- as.Date(paste(priceSalesPromo[,"YearMonth"]," 01",sep=""),"%Y %m %d")


weather <- read.csv("weather.csv")
str(weather)
sapply(weather[,-2],function(x) sum(!is.finite(x)))
sum(is.na(weather$Agency))
stri_sub(weather[,"YearMonth"],5,2) <- " "
weather$YearMonth <- as.Date(paste(weather[,"YearMonth"]," 01",sep=""),"%Y %m %d")

volume_forecast <- read.csv("volume_forecast.csv")
str(volume_forecast)
demographics[!(unique(demographics$Agency) %in% unique(volume_forecast$Agency)),] # Agency_06 & Agency_14 not present in histVolume
volume_forecast[which((volume_forecast$Agency==histVolume$Agency) && (volume_forecast$SKU != histVolume$SKU)),1:2]
volume_forecast[!(volume_forecast$SKU %in% histVolume$SKU)]

library(dplyr)
View(volume_forecast %>% group_by(Agency) %>% summarise(unique_count = n_distinct(SKU)))
View(histVolume[,1:2] %>% group_by(Agency) %>% summarise(unique_count = n_distinct(SKU)))


df_merged <- histVolume
stri_sub(df_merged[,"YearMonth"],5,2) <- " "
df_merged$YearMonth <- as.Date(paste(df_merged[,"YearMonth"]," 01",sep=""),"%Y %m %d")
df_merged <- merge(df_merged,demographics,by="Agency")
df_merged <- merge(df_merged,event[,c("YearMonth","Events")],by="YearMonth")
df_merged <- merge(df_merged,weather,by=c("YearMonth","Agency"))
df_merged <- merge(df_merged,priceSalesPromo[,c(1:3,6)],by=c("YearMonth","Agency","SKU")) 

train <- df_merged[which(df_merged$YearMonth < "2017-01-01"),]
train$YearMonth <- as.factor(train$YearMonth) 
test <- df_merged[which(df_merged$YearMonth >= "2017-01-01"),]
test$YearMonth <- as.factor(test$YearMonth)

library(gbm)
set.seed(123)
model_1 <- gbm(Volume~YearMonth + Agency + SKU + Avg_Population_2017, data = train, n.trees = 100000,n.cores = 4)
fit_1 <- predict(model_1,test,n.trees = 10000)
forecastAccuracy_1 <- 1 - (sum(abs(test$Volume - fit_1))/sum(test$Volume)) # 0.13

#library(randomForest)
#set.seed(123)
#model_rf <- randomForest(Volume ~ .,data = train,ntree=10000,do.trace=TRUE) # cannot handle predictors with more than 53 categories


library(h2o)
h2o.init(nthreads=-1,max_mem_size='8G')
#model_rf <- h2o.randomForest(c("Agency","SKU","YearMonth","Avg_Population_2017"),"Volume",training_frame = as.h2o(train))
set.seed(123)
model_rf <- h2o.randomForest(c(1:3,5:7),4,training_frame = as.h2o(train)) 
fit_rf <- h2o.predict(model_rf,as.h2o(test))
forecastAccuracy_rf <- 1 - (sum(abs(as.h2o(test$Volume) - fit_rf))/sum(as.h2o(test$Volume))) # 0.71

volume_forecast2 <- volume_forecast
volume_forecast2 <- merge(volume_forecast2,demographics,by="Agency")
volume_forecast2$YearMonth <- as.factor("2018-01-01")
volume_forecast2$Events <- 1

submit_rf <- h2o.predict(model_rf,as.h2o(volume_forecast2))
volume_forecast2$Volume <- as.data.frame(submit_rf)[,1]
write.csv(volume_forecast2[,1:3],"volume_forecast.csv",row.names = F)





#------------------------------------------------------------------------------------------------------------------------------------------------------------

# SKU RECOMMENDATION


library(ggplot2)
ggplot(demographics, aes(x=Avg_Population_2017, y=Avg_Yearly_Household_Income_2017, col=Agency, label=Agency)) + geom_text()

promo_agg <- aggregate(Promotions~Agency,train,sum)
promo_agg[order(promo_agg$Promotions),]

maxTemp_agg <- aggregate(Avg_Max_Temp~Agency,train,mean)
maxTemp_agg[order(maxTemp_agg$Avg_Max_Temp),]

df_recom <- demographics
df_recom$Avg_Population_2017 <- scale(df_recom$Avg_Population_2017)
df_recom$Avg_Yearly_Household_Income_2017 <-scale(df_recom$Avg_Yearly_Household_Income_2017)

r_sq<- rnorm(20)
for (number in 1:20){clus <- kmeans(df_recom[,2:3], centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

plot(r_sq)

clus5 <- kmeans(df_recom[,2:3], centers = 8, iter.max = 50, nstart = 50)
clus5$betweenss/clus5$totss

df_recom <- cbind(df_recom,clus5$cluster)
colnames(df_recom)[4] <- "Cluster"

ggplot(df_recom, aes(x=Avg_Population_2017, y=Avg_Yearly_Household_Income_2017, col=Cluster, label=Agency)) + geom_text() + scale_color_gradientn(colours = rainbow(8))


df_recom[which(df_recom$Agency=="Agency_06"),4]
df_recom[which(df_recom$Cluster==8),1]
clust_agency_06 <- histVolume[which(histVolume$Agency==c("Agency_51","Agency_55","Agency_60")),]
clust_agency_06_sum <- aggregate(Volume~.,clust_agency_06[,c(2,4)],sum)
clust_agency_06_sum <- clust_agency_06_sum[order(-clust_agency_06_sum$Volume),]

clust_promo_agency_06 <- priceSalesPromo[which(priceSalesPromo$Agency==c("Agency_51","Agency_55","Agency_60")),c(1,2,5)]
clust_promo_agency_06_avg <- aggregate(Sales~.,clust_promo_agency_06[,c(2:3)],mean)
clust_promo_agency_06_avg <- clust_promo_agency_06_avg[order(-clust_promo_agency_06_avg$Sales),]

merge_06 <- merge(clust_agency_06_sum,clust_promo_agency_06_avg,by="SKU")
merge_06$TotalSales <- merge_06$Volume * merge_06$Sales
merge_06 <- merge_06[order(-merge_06$TotalSales),]

df_recom[which(df_recom$Agency=="Agency_14"),4]
df_recom[which(df_recom$Cluster==4),1]
clust_agency_14 <- histVolume[which(histVolume$Agency==c("Agency_46","Agency_56","Agency_12","Agency_48","Agency_09","Agency_27","Agency_03")),]
clust_agency_14_sum <- aggregate(Volume~.,clust_agency_14[,c(2,4)],sum)
clust_agency_14_sum <- clust_agency_14_sum[order(-clust_agency_14_sum$Volume),]

clust_promo_agency_14 <- priceSalesPromo[which(priceSalesPromo$Agency==c("Agency_46","Agency_56","Agency_12","Agency_48","Agency_09","Agency_27","Agency_03")),c(1,2,5)]
clust_promo_agency_14_avg <- aggregate(Sales~.,clust_promo_agency_14[,c(2:3)],mean)
clust_promo_agency_14_avg <- clust_promo_agency_14_avg[order(-clust_promo_agency_14_avg$Sales),]

merge_14 <- merge(clust_agency_14_sum,clust_promo_agency_14_avg,by="SKU")
merge_14$TotalSales <- merge_14$Volume * merge_14$Sales
merge_14 <- merge_14[order(-merge_14$TotalSales),]
