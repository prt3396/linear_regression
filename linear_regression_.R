library(readxl)
library(dplyr)
##---------------------loading data set-------------------
data <- read_excel("data.xlsx")
View(data)
str(data)
attach(data)
##--------------------scatter plot------------------------
plot(data)
cor(data)
abline(lm(scores~hours),col="blue")
##-------------------dividing the data set----------------
set.seed(123)
s <- sample(1:nrow(data),0.75*nrow(data))
train <- data[s,]
test <- data[-s,]
##--------------------creating linear model---------------
mod <- lm(scores~.,data = train)
summary(mod)
##--------------------predicting from train dataset-------
predd <- predict(mod,newdata=train)
#finding RMSE of train dataset
mean((train$scores-predd)**2) %>%
  sqrt()
##--------------------predicting from test dataset--------
prd <- predict(mod,newdata = test)
#finding the RMSE of test dataset
mean((test$scores-prd)**2) %>%
  sqrt()
