rm(list = ls())
setwd("C:/Users/Harshita Prasad/Desktop/predicting bike rental counts")
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", 
      "Information", "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', "psych", "usdm")
lapply(x, require, character.only = TRUE)
rm(x)
original_data = read.csv('day.csv',header = T,na.strings = c(""," ","NA"))
df = original_data
str(df)
summary(df)
df$dteday <- format(as.Date(df$dteday,format="%Y-%m-%d"), "%d")
df$instant <- NULL
df$casual <- NULL
df$registered <- NULL
apply(df, 2,function(x) length(table(x)))
names(df)[names(df) == 'dteday'] <- 'date'
names(df)[names(df) == 'yr'] <- 'year'
names(df)[names(df) == 'mnth'] <- 'month'
names(df)[names(df) == 'hum'] <- 'humidity'
names(df)[names(df) == 'weathersit'] <- 'weather'
names(df)[names(df) == 'cnt'] <- 'count'
categorical_var = c('date', 'season', 'year', 'month', 'holiday', 'weekday', 'workingday','weather')
numerical_var = c('temp', 'atemp', 'humidity', 'windspeed','count')
typ_conv = function(df,var,type){
  df[var] = lapply(df[var], type)
  return(df)
}
df = typ_conv(df,categorical_var, factor)
str(df)

#visualizations
multi.hist(df$temp, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "temp",ylab = "density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$atemp, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "atemp",ylab = "density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$humidity, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "humidity",ylab = "density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$windspeed, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "windspeed",ylab = "density"),
           dlty = c("solid", "solid"), bcol = "linen")
multi.hist(df$count, main = NA, dcol = c("blue", "red"), title(main = NULL,xlab = "count",ylab = "density"),
           dlty = c("solid", "solid"), bcol = "linen")

#scatter plots
#bivariate analysis
#lets check distribution between target and continuous variables
plot(df$temp,df$count,main = 'Scatter Plot for count of bikes as per temp',
     xlab = 'temp', ylab = 'count')
abline(lm(count ~ temp, data = df), col = 'red')
plot(df$atemp,df$count,main = 'Scatter Plot for count of bikes as per atemp',
     xlab = 'atemp', ylab = 'count')
abline(lm(count ~ atemp, data = df), col = 'red')
plot(df$humidity,df$count,main = 'Scatter Plot for count of bikes as per humidity',
     xlab = 'humidity', ylab = 'count')
abline(lm(count ~ humidity, data = df), col = 'red')
plot(df$windspeed,df$count,main = 'Scatter Plot for count of bikes as per windspeed',
     xlab = 'windspeed', ylab = 'count')
abline(lm(count ~ windspeed, data = df), col = 'red')

#lets check categorical variables
for(i in 1:length(categorical_var))
{
  assign(paste0("b",i),ggplot(aes_string(y='count',x = (categorical_var[i])),
                              data=subset(df))+
           geom_bar(stat = "identity",fill = "green") +
           ggtitle(paste("Count of bikes with respect to",categorical_var[i])))+
    theme(axis.text.x = element_text( color="red", size=8))+
    theme(plot.title = element_text(face = "old"))
}
#lets plot between categorical and target variables
gridExtra::grid.arrange(b1,ncol=1)
gridExtra::grid.arrange(b2,ncol=1)
gridExtra::grid.arrange(b3,ncol=1)
gridExtra::grid.arrange(b4,ncol=1)
gridExtra::grid.arrange(b5,ncol=1)
gridExtra::grid.arrange(b6,ncol=1)
gridExtra::grid.arrange(b7,ncol=1)
gridExtra::grid.arrange(b8,ncol=1)

#missing value analysis
apply(df, 2, function(x) {sum(is.na(x))})

#outlier analysis
for (i in 1:length(numerical_var))
{
  assign(paste0("bp",i), ggplot(aes_string(x="count",y = (numerical_var[i])), d=df)+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="blue", fill = "indianred3" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(x="Count of Bike", y=numerical_var[i])+
           ggtitle(paste("Box plot of count of bikes with",numerical_var[i])))
}
gridExtra::grid.arrange(bp1,ncol=1)
gridExtra::grid.arrange(bp2,ncol=1)
gridExtra::grid.arrange(bp3,ncol=1)
gridExtra::grid.arrange(bp4,ncol=1)
gridExtra::grid.arrange(bp5,ncol=1)

#correlation
corrgram(df[,numerical_var], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
df = subset(df, select=-c(atemp))

#chi-squared test
categorical_df = df[,categorical_var]
for (i in categorical_var){
  for (j in categorical_var){
    print(i)
    print(j)
    print(chisq.test(table(categorical_df[,i], categorical_df[,j]))$p.value)
  }
}

#ANOVA
anova_date =(lm(count ~ date, data = df))
summary(anova_date)
anova_season =(lm(count ~ season, data = df))
summary(anova_season)
anova_year =(lm(count ~ year, data = df))
summary(anova_year)
anova_month =(lm(count ~ month, data = df))
summary(anova_month)
anova_holiday =(lm(count ~ holiday, data = df))
summary(anova_holiday)
anova_weekday =(lm(count ~ weekday, data = df))
summary(anova_weekday)
anova_workingday =(lm(count ~ workingday, data = df))
summary(anova_workingday)
anova_weather =(lm(count ~ weather, data = df))
summary(anova_weather)

df = subset(df, select=-c(date, holiday, weekday, workingday))

#multicollinearity
vif(df)

#Normalization of count
min(df$count)
max(df$count)
hist(df$count)
df$new_count = (df$count - min(df$count)) / (max(df$count) - min(df$count))

df = subset(df, select=-c(count))

#clean environment
rmExcept(c("original_data",'df'))

#Divide the data intro train and test
set.seed(123)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test = df[-train_index,]

#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}

##Decision Tree Regression
fit = rpart(new_count ~ ., data = train, method = "anova")
DT_predictions = predict(fit, test[,-8])
MAPE(test[,8], DT_predictions)  #error = 28.25% or accuracy = 71.75%

##Random Forest
RF_model = randomForest(new_count ~ ., train, importance = TRUE, ntree = 100)
RF_predictions = predict(RF_model, test[,-8])
MAPE(test[,8], RF_predictions) #Error = 20.30% and Accuracy = 79.70%

RF_model = randomForest(new_count ~ ., train, importance = TRUE, ntree = 500)
RF_predictions = predict(RF_model, test[,-8])
MAPE(test[,8], RF_predictions) #Error = 19.86% and Accuracy = 80.14%

#Linear Regression
vif(df[,-8])
vifcor(df[,c(5,6,7)])
lm_model = lm(new_count ~., data = train)
summary(lm_model)
LR_predictions = predict(lm_model, test[,1:7])
MAPE(test[,8], LR_predictions) #Error = 21.87% and Accuracy = 78.13%

regr.eval(test[,8], DT_predictions, stats = c('mae', 'rmse', 'mape', 'mse'))
regr.eval(test[,8], RF_predictions, stats = c('mae', 'rmse', 'mape', 'mse'))
regr.eval(test[,8], LR_predictions, stats = c('mae', 'rmse', 'mape', 'mse'))

write.csv(df, "df.csv", row.names = F)
