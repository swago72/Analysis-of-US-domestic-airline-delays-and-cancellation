
#Loading DF
library(readxl)
DF <- read.csv("C:\\Users\\raksh\\OneDrive\\Desktop\\BA with R\\DelayedFlights.csv",sep=',')
#DF <- read.csv("DelayedFlights.csv")
View(DF)
str(DF) #get a understanding of what the structure of the DFset is
#cleaning DF: drop the first column
DF <- DF[,-1]
View(DF)
max(DF$DepDelay)
DF$UniqueCarrier = factor(DF$UniqueCarrier)
DF$TailNum = factor(DF$TailNum)
DF$Origin = factor(DF$Origin)
DF$Dest = factor(DF$Dest)
DF$CancellationCode = factor(DF$CancellationCode)
DF$DayOfWeek = factor(DF$DayOfWeek)
#create a missmap then visualize the missing value
library(Amelia)
ggplot(DF)
missmap(DF,
        main="Delayed Flights",
        y.labels = NULL,
        y.at = NULL,
        #here, the standard ylab should be y.label in Amelia. 
        #Also, y.label should always appear with y.at
        col=c("yellow", "black"),
        legend = TRUE)
#get rid of all the NA values, and we assign this DFset without NA value to DF1
DF <- na.omit(DF)
any(is.na(DF))
rm(DF)
#get rid of the ones that are over 180 (We get this value by US. department of transportation: "For flights landing at U.S. airports, airlines are required to provide passengers with an opportunity to safely get off of the airplane before 3 hours for domestic flights and 4 hours for international flights.")
DF <- subset(DF, DepDelay <= 180)
nrow(DF1)
#Visualize the missing values after the cleaning
library(Amelia)
missmap(DF1,
        main="Delayed Flights",
        y.labels = NULL,
        y.at = NULL,                    #here, the standard ylab should be y.label in Amelia. Also, y.label should always appear with y.at
        col=c("yellow", "black"),
        legend = TRUE)
#Random Sampling 10000 
library(dplyr)
DF <-DF %>% sample_n(10000)
DF
rm(DF1)
# create a boxplot to visulize DayOfWeek and DepDelay
install.packages("lattice")
library("lattice")
library(ggplot2)
attach(DF_sampled)
LogDepDelay <- log(DepDelay) # logarize the DepDeplay column
box_DF1 <- ggplot(DF_sampled, aes(x = LogDepDelay)) + geom_boxplot(aes(fill = factor(DayOfWeek)))
print(box_DF1)

#Split the DF 
library(dplyr)
library(caTools)
set.seed(101)
split <- sample.split(DF, SplitRatio = 0.8)
head(split)
split
train = subset(DF, split == TRUE)
test = subset(DF, split == FALSE)
head(train)
head(test)
View(DF)
head(DF)

#Split 2
set.seed(101)   # for reproducible results
train <- sample(1:nrow(DF), (0.8)*nrow(DF))
train.df <- DF[train,]
test.df <- DF[-train,]

#example
actual <- test.df$`default payment next month`
knnPredict <- predict(knnFit, test.df)
cm_knn <- table(knnPredict, actual)
cm_knn

##fitting SVM
DeDelay ~ Diverted
classifier <- svm(formula = Diverted ~ DepDelay,
                  data = train.df,
                  type = 'C-classification',
                  kernel = 'linear')
classifier ##classifying values
actual <- test.df1$Diverted
pred <- predict(classifier, newdata = test.df)
pred #predecting values
str(pred)
str(test.df[-700,])
sample(test.df, 1306)
test.df1 <- test.df %>% sample_n(1306)
str(test.df1)
cm_svm <- table(pred, actual)
cm_svm
testing_values <-  table(test.df1, pred)
table(testing_values)

#Build a regression model
model <- lm(DepDelay~DayOfWeek, DF = train)
summary(model)
scatter <- ggplot(test, aes(DayOfWeek, DepDelay, color = DayOfWeek)) + geom_point() + stat_smooth(method = "lm")
scatter
# # Grab residuals
# res <- residuals(model)
# # Convert to DFFrame for gglpot
# res <- as.DF.frame(res)
# head(res)
# 
# # Histogram of residuals
# ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
# #prediction
DepDelay.predictions <- round(predict(model,test))
DepDelay.predictions
results <- cbind(DepDelay.predictions,test$DepDelay) 
results
colnames(results) <- c('pred','real')
colnames(results)
results <- as.data.frame(results)
print(head(results))
str(results)


##EDA ROUND 2

DF$UniqueCarrier <- factor(DF$UniqueCarrier)
DF$Year <- factor(DF$Year)
DF$Month <- factor(DF$Month)
DF$Origin <- factor(DF$Origin)
DF$Diverted <- factor(DF$Diverted)

summary(DF)
ggplot(DF, aes(Month, fill = UniqueCarrier)) + geom_bar(position = "dodge")
#month-wise airlines delay plot

ggplot(DF, aes(UniqueCarrier, log(ArrDelay), fill = UniqueCarrier)) + 
  geom_boxplot(position = "dodge")
#airlines-wise delay plot


#another plot coming up unique carrier vs cancelled flights
Carrier_Cancelled <- DF %>% select(UniqueCarrier, Cancelled)
only_cancelledflights <- subset(Carrier_Cancelled, Cancelled == 1)
head(only_cancelledflights)
EDA4 <- ggplot(only_cancelledflights, aes(UniqueCarrier, fill=Cancelled))+ 
  geom_bar()
EDA4

#using the cancellation code parameter
Carrier_Cancelled <- DF %>% select(UniqueCarrier, CancellationCode)
only_cancelledflights <- subset(Carrier_Cancelled, CancellationCode == 'N')
EDA4 <- ggplot(only_cancelledflights, aes(CancellationCode, fill=UniqueCarrier))+ 
  geom_bar()
EDA4


#fill = factor(only_cancelledflights$Cancelled)
p <- subset(DF, subset=UniqueCarrier==Cancelled)
q <- subset(DF, subset= Cancelled>0)

+ (ggplot(DF, aes(y=Cancelled)) + 
     geom_bar())

##another plot part 2
Diversion_Delay <- DF %>% select(Diverted, DepDelay)
SVM_Delay <- subset(Diversion_Delay, Diverted == '1')
head(SVM_Delay)
??scale

#trying
ggplot(DF, aes(x=UniqueCarrier, y=CarrierDelay)) + geom_bar()
a <- DF$UniqueCarrier
b <- DF$CarrierDelay
c<- c(a,b)
Month_Cancelled <- DF %>% select(Month, Cancelled)
Month_Cancelled
EDA3_only_cancellations <- subset(Month_Cancelled, Cancelled > 0)
# getting the count of cancellation per month
EDA3 <- ggplot(data=EDA3_only_cancellations, aes(x=Month, y=Cancelled)) +
  geom_bar(stat="identity", fill = factor(EDA3_only_cancellations$Month)) +
  theme_minimal()
EDA3
table(only_cancellations) # get the exact number of the cancellations each month



##testing
head(DF)
DF$DayOfWeek
DF$Month

DF[DF,DF$CancellationCode!='N']

##taking a look at day-wise and month-wise DF
DF$DayOfWeek <- DF$DayOfWeek
DF$Month <- DF$Month

DF$DayOfWeek[DF$DayOfWeek == 1] = 'Monday'
DF$DayOfWeek[DF$DayOfWeek == 2] = 'Tuesday'
DF$DayOfWeek[DF$DayOfWeek == 3] = 'Wednesday'
DF$DayOfWeek[DF$DayOfWeek == 4] = 'Thursday'
DF$DayOfWeek[DF$DayOfWeek == 5] = 'Friday'
DF$DayOfWeek[DF$DayOfWeek == 6] = 'Saturday'
DF$DayOfWeek[DF$DayOfWeek == 7] = 'Sunday'

DF$Month[DF$Month == 1] = 'January'
DF$Month[DF$Month == 2] = 'February'
DF$Month[DF$Month == 3] = 'March'
DF$Month[DF$Month == 4] = 'April'
DF$Month[DF$Month == 5] = 'May'
DF$Month[DF$Month == 6] = 'June'
DF$Month[DF$Month == 7] = 'July'
DF$Month[DF$Month == 8] = 'August'
DF$Month[DF$Month == 9] = 'September'
DF$Month[DF$Month == 10] = 'October'
DF$Month[DF$Month == 11] = 'November'
DF$Month[DF$Month == 12] = 'December'

head(DF$DayOfWeek)

##trying to find he most common day and month for flight delays
##using the pipe operator

DF %>% group_by(DF$DayOfWeek) %>% tally %>% arrange(desc(n))

DF %>% group_by(DF$Month) %>% tally %>% arrange(desc(n))

##the most common day for flight delays is on friday, 
##which isn't surprising as it's also one of the most common days to fly.



DF$Cancelled[DF$Cancelled == 0] = 'No'
DF$Cancelled[DF$Cancelled == 1] = 'Yes'

qplot(factor(Cancelled), data=DF, geom="bar", fill=factor(Cancelled))


DF %>% group_by(Cancelled) %>%
  tally %>% arrange(desc(n))

##assigining the variables

DF$CancellationCode[DF$CancellationCode == 'A'] = 'Carrier'
DF$CancellationCode[DF$CancellationCode == 'B'] = 'Weather'
DF$CancellationCode[DF$CancellationCode == 'C'] = 'NAS'
DF$CancellationCode[DF$CancellationCode == 'D'] = 'Security'

DF %>% filter(CancellationCode != 'N') %>%
  group_by(CancellationCode) %>%
  tally %>%
  arrange(desc(n))

#check the month-wise data based on the cancellation code
CancelledSubset = subset(DF, CancellationCode != 'N')
ggplot(CancelledSubset,aes(Month,fill=CancellationCode)) + geom_bar()


##the most frequent cause of flight cancellations is bad weather, 
##and the dataset contains no examples of cancellations due to security concerns. 
##The months of November and December see the most cancellations. 
##The timing of the cancellations suggests that 
##some carrier-related cancellations might possibly be weather-related.

#summary of the data based on delay variable
DF %>% filter(CarrierDelay != 'NA',WeatherDelay != 'NA',
              NASDelay != 'NA', SecurityDelay != 'NA', LateAircraftDelay != 'NA') %>% 
  select(CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay) %>%
  summarize(CarrierDelay = mean(CarrierDelay),
            WeatherDelay = mean(WeatherDelay),
            SecurityDelay = mean(SecurityDelay),
            LateAircraftDelay = mean(LateAircraftDelay)) %>% 
              gather(var,mean)
            
            
