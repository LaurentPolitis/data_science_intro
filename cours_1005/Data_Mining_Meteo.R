### DATAMINING ### 
library(readxl)
library(lubridate)
library(Nippon)
library(dplyr)
library(xgboost)
library(ggplot2)
library(caret)
library(maptools)


### Data Mining  -------

DATA_Demand <- read_excel("DATA_Demand.xlsx")
DATA_Demand$weekend = ifelse(lubridate::wday(DATA_Demand$Dates) %in% c(1, 7),1,0)

DATA_Demand$holiday = as.numeric(is.jholiday(as.Date(DATA_Demand$Dates)))
#" sunrise solarnoon sunset day_length"
#36.2048° N, 138.2529° E


list_daylength = do.call(args = (lapply(as.Date(DATA_Demand$Dates),FUN =
                            function(x){
                                ephemeris(lat = 36.2048,lon =138.2529,date = x)[,c("date","sunrise", "solarnoon","sunset","day_length")]
                            })),what = "rbind")

colnames(list_daylength) = c("Dates","sunrise", "solarnoon","sunset","day_length")

DATA_Demand_lag = DATA_Demand %>% mutate(Dates = as.Date(Dates),Total_lag1 = lag(Total), Total_lag2 = lag(lag(Total)),Total_lag3 = lag(lag(lag(Total))),Total_lag4= lag(lag(lag(lag(Total))))) %>%
                    select(-c("TEPCO",  "KEPCO", "TOHOKU",  "CHUBU")) %>% full_join(y =list_daylength,by = "Dates" )

DATA_Demand_lag = DATA_Demand_lag[complete.cases(DATA_Demand_lag), ]


XGB_model <- xgboost::xgboost(data = as.matrix(DATA_Demand_lag[,-c(1,6)]),
                              label = (as.matrix(DATA_Demand_lag[,"Total"])),
                              objective = "reg:linear",
                              eval_metric = "mae",
                              learning_rate = 0.05,
                              subsample = 0.5, seed = 1, # subsample default value=1. Setting to 0.5 means that XGBoost randomly collected half of the data instances to grow trees and this will prevent overfitting.
                              nrounds = 3000, 
                              max.depth = 2, eta = 1, nthread = 3)

xgb.importance(model = XGB_model )


XGB_cv <- xgboost::xgb.cv(data = as.matrix(DATA_Demand_lag[,-c(1,6)]),
                          label = (as.matrix(DATA_Demand_lag[,"Total"])),
                          objective = "reg:linear",
                          eval_metric = "mae",
                          learning_rate = 0.05,
                          subsample = 0.5, seed = 1, # subsample default value=1. Setting to 0.5 means that XGBoost randomly collected half of the data instances to grow trees and this will prevent overfitting.
                          nrounds = 3000, 
                          max.depth = 2, eta = 1, nthread = 3,nfold = 8,)


DATA_Demand_lag$forecast = predict(XGB_model,as.matrix(DATA_Demand_lag[,-c(1,6)]))

qplot(DATA_Demand_lag$forecast,DATA_Demand_lag$Total)+geom_abline(slope=1,color="red",size=2,alpha=0.5)

ggplot(XGB$evaluation_log)+ geom_point(aes(x=iter,y=test_mae_mean,color="test"))+
    geom_point(aes(x=iter,y=train_mae_mean,color="train"))+ scale_y_log10()

##### Second Parts with matrice grid search #####




set.seed(100)  # For reproducibility

# Create index for testing and training data
inTrain <- createDataPartition(y = DATA_Demand_lag$Total, p = 0.8, list = FALSE)

# subset power_plant data to training
training <- DATA_Demand_lag[inTrain,]


# subset the rest to test
testing <- DATA_Demand_lag[-inTrain,]

### Convert testing sets -------
X_train = xgb.DMatrix(as.matrix(training %>% select(-Total,-Dates)))

y_train = training$Total


X_test = xgb.DMatrix(as.matrix(testing %>% select(-Total,-Dates)))
y_test = testing$Total


xgb_trcontrol = trainControl(
    method = "cv",
    number = 5,  
    allowParallel = TRUE,
    verboseIter = FALSE,
    returnData = FALSE
)




xgbGrid <- expand.grid(nrounds =seq(10, 2000,by= 5),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)


set.seed(0) 

xgb_model = train(
    X_train, y_train,  
    trControl = xgb_trcontrol,
    tuneGrid = xgbGrid,
    method = "xgbTree"
)

xgb_model$bestTune


predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
MAPE = (mean(residuals^2/y_test))


cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
# The root mean square error of the test data is  2.856 
y_test_mean = mean(y_test)

# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )

# Calculate residual sum of squares
rss =  sum(residuals^2)

# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')
# The R-square of the test data is  0.972 
# Plotting actual vs predicted

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))

# Plot predictions vs test data

ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
    geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
    xlab("Predicted Power Output ") + ylab("Observed Power Output") + 
    theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
          axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
          axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


### 




