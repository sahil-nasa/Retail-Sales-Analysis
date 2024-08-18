#Project : RETAIL SALES ANALYSIS by Sahil Nasa

#Data reading

wlm <- read.csv("Walmart_Store_sales.csv",header = TRUE)

#Checking for missing values

x <- is.na.data.frame(wlm)

#Sub-setting to see only those entries which have missing attributes

wlm_missing <- subset(wlm,x==TRUE) 

#Data manipulation

View(wlm)
str(wlm)

#Data conversion to desired format

wlm$Holiday_Flag <- as.factor(wlm$Holiday_Flag)

library('lubridate')
wlm$Date <-  dmy(wlm$Date)
wlm_copy <- wlm

#---------Max sales--------
#Which store has max sales

library('dplyr')

wlm_max_sales <- summarize(group_by(wlm,Store),total_sales=sum(Weekly_Sales))
View(wlm_max_sales)
res <- arrange(wlm_max_sales,desc(total_sales))


# By seeing the output, we can see Store 20 has max sales.

#---------Max Stdev--------------------------------------
#Which store has max stdev 

wlm_maxstdev <- summarize(group_by(wlm,Store),stdev=sd(Weekly_Sales))
View(wlm_maxstdev)
res <- arrange(wlm_maxstdev,desc(stdev))
  
# By seeing the output, we can see Store 14 has the most variation

#Find out coeff of mean to stdev or better to say COEFF OF VARIATION

wlm_cv <- summarize(group_by(wlm,Store),CV=sd(Weekly_Sales)/mean(Weekly_Sales))
View(wlm_cv)

#---------Quarterly Growth Rate------------------------------------
#-----Quarterly Growth Rate : (Current quarter sales/Previous Quarter Sales)*100

wlm$qtr <- quarter(wlm$Date, with_year = TRUE)

wlm_qgr <- wlm %>% group_by(Store,qtr) %>% summarise(qtr_sales=sum(Weekly_Sales))
View(wlm_qgr)

#lag() function gives the previous quarter values as here we applied on qtr_sales

wlm_qgr$lag1 <- lag(wlm_qgr$qtr_sales,1)

#Using formula for Quarterly Growth rate =  
# ((This quarter Sales - previous quarter sales)/Previous quar. sales)*100

wlm_qgr$QGR <- ((wlm_qgr$qtr_sales - wlm_qgr$lag1)*100/wlm_qgr$lag1)
wlm_qgr <- filter(wlm_qgr,qtr=="2012.3")
wlm_qgr <- arrange(wlm_qgr,desc(QGR))
wlm_qgr
 
#Seeing the Weekly sales of a quarter for each store, Store 7 has the max 
#quarterly growth rate which is 13.3% followed by store 16 and 35.
#----------Holidays Impact------------------
#Holidays which have negative impact on sales
#Average sales on non-holidays i.e Holiday_flag is zero

nhol <- filter(wlm,Holiday_Flag==0)
View(nhol)
nhol <- summarize(nhol,Avg_sales=mean(Weekly_Sales))


#Average sales on Holidays (getting those whose weekly sales are 
#higher than the sales in non-holiday days)

hol <- filter(wlm,Holiday_Flag==1,Weekly_Sales>1041256)
View(hol)

#This 'hol' dataset contains all the holidays which have above average sales

#----------Monthly and semester view of sales------------------
#Provide a monthly and semester view of sales in units and give insights

wlm_copy <- wlm
wlm_copy <- mutate(wlm_copy,Year=year(Date),Month=month(Date),Semester = semester(Date))
View(wlm_copy)

#Calculating monthly sales for Walmart (all stores together)

wlm_mon <- summarize(group_by(wlm_copy,Month),Monthly_Sales=sum(Weekly_Sales))
View(wlm_mon)

library("ggplot2")
ggplot(wlm_mon ,aes(x=Month,y=Monthly_Sales)) + geom_point() + geom_line() +
  scale_x_continuous(name="Month",limits = c(0,12), breaks = seq(1,12,1)) +
  scale_y_continuous(name="Total Sales", limits = c(300000000,700000000))

options(scipen=999)  #To remove scientific value in plots

#INSIGHTS (Monthly sales) :  Sales is least in JAN but shoots up in FEB by a lot.
# Main insight is Sales decreases a lot in NOV but increases a lot in DEC. 
  
#Calculating Semester sales

wlm_sem <- summarize(group_by(wlm_copy,Year,Semester),Semester_Sales=sum(Weekly_Sales))
View(wlm_sem)
wlm_sem$Semester <- as.factor(wlm_sem$Semester)

#Using grouped barplot to visualize

ggplot(wlm_sem,aes(fill=Semester, y=Semester_Sales, x= Year)) +
  geom_bar(position = "dodge", stat = "identity")

#INGIGHTS (Semester wise) : Sales increases in every second semester except
#  for year 2012.

#----------Linear regression model------------------------


#Checking for outliers

boxplot(wlm$Temperature, horizontal = TRUE) #Outliers <10
boxplot(wlm$Fuel_Price, horizontal = TRUE)  #No outliers
boxplot(wlm$CPI, horizontal = TRUE)         #No outliers
boxplot(wlm$Unemployment, horizontal = TRUE) #Outliers <4.5 and >10

#Dropping outliers
wlm_new <- filter(wlm, Temperature>10,Unemployment>4.5,Unemployment<10)

#Change dates into days 
wlm_new <- mutate(wlm_new,Year=year(Date),Month=month(Date),Day = day(Date))

#Filtering data of store 1 only 
wlm_new <- filter(wlm_new,Store==1)
wlm_new <- arrange(wlm_new,Date)

#Cross validation (train-test data)
library('caTools')
sel <- sample.split(wlm_new$Store,SplitRatio = 0.7)
wlm_train <- subset(wlm_new,sel==TRUE)

wlm_test <- subset(wlm_new,sel==FALSE)


wlm_model <- lm(Weekly_Sales ~ Fuel_Price+Temperature+Holiday_Flag+CPI+Unemployment, data = wlm_new)
summary(wlm_model)

#Dropping non-significant variables seeing the p-values : Holiday,Day,Year
wlm_model <- lm(Weekly_Sales ~ Temperature+CPI, data = wlm_new)
summary(wlm_model)

#Testing
wlm_test$Pred_Sales <- predict(wlm_model,newdata=wlm_test)
View(wlm_test)

#----------Time-series modelling -----------------------

wlmts <- summarize(group_by(wlm,Date),Weekly_Sales = sum(Weekly_Sales))

plot(wlmts)
#We can see a Seasonal Trend in the plot 

class(wlmts)
str(wlmts)
wlmts$Date =as.Date(wlmts$Date,format=c("%m/%d/%Y"))

#Creating a dataset having month-wise sales for all the years
wlmts = mutate(wlmts,Year =format(Date,"%Y"), Day=format(Date,"%d"),Month =format(Date,"%m"))
str(wlmts)
wlmts$Year <- as.numeric(wlmts$Year)
wlmts$Month <- as.numeric(wlmts$Month)
wlmts$Day <- as.numeric(wlmts$Day)
str(wlmts)
View(wlmts)

wlmts = select(wlmts,Year,Month,Day,Weekly_Sales)
View(wlmts)

#Sorting date wise
Wlmts = arrange(wlmts,Year,Month,Day)
View(Wlmts)

# creating a Column with month and year of sale
wlmts = mutate(Wlmts,Time_Of_Sale = as.Date(paste(Year,"-",Month,"-",1,sep="")))
View(wlmts)
#Removing unwanted columns
wlmts <- wlmts[,-(1:3)]
wlmts <- select(wlmts,Time_Of_Sale,Weekly_Sales)
wlmts <- summarize(group_by(wlmts,Time_Of_Sale),Weekly_Sales = sum(Weekly_Sales))

library("tsbox")
ts_xts(wlmts)
#Modelling
library(forecast)

#Using auto-arima to get best values of p,d,f values making our model
#more efficient

Walmart_ARIMA = arima(wlmts[1:30,2], order = c(2,1,2), method = "ML")
summary(Walmart_ARIMA)
#Forecasting for next year
Forecasted_Sale = forecast(Walmart_ARIMA,h=12)
plot(Forecasted_Sale)

Forecasted_Sale <- as.data.frame(Forecasted_Sale)
View(Forecasted_Sale)
Frcst <- Forecasted_Sale[,1]
View(Frcst)

Actual_Sales = wlmts[31:42,]

# concatenating forecast and actual
Actual_vs_Forecst_last = cbind(Forecasted_Sale,Actual_Sales)
View(Actual_vs_Forecst_last)
Actual_vs_Forecst_last_deviation = transform(Actual_vs_Forecst_last, 
                                             Errors = abs(Forecasted_Sale-Weekly_Sales)/Weekly_Sales)
Actual_vs_Forecst_last_deviation <- select(Actual_vs_Forecst_last_deviation,Point.Forecast,Time_Of_Sale,Weekly_Sales)
View(Actual_vs_Forecst_last_deviation)

