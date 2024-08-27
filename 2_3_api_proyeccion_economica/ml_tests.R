source(file = "2_3_api_proyeccion_economica/utils.R")














j







































































































































































































{set.seed(123)
pr <- data.frame(
  y = rnorm(1000, 50, 15),
  x1 = rnorm(1000, 50, 15),
  x2 = rnorm(1000, 50, 15),
  x3 = runif(1000, min = 10,40))
}

{set.seed(123)
index <- createDataPartition(pr$y, times = 1, p = .7, list = F)
test <- pr[-index,]
train <- pr[index,]}


m1 <- caret::train(y~.,method = "rf", data = train)
m1

train <- train %>% 
  mutate(
    yp = predict(m1,train, type = "response"))
test <- test %>% 
  mutate(
    yp = predict(m1,test, type = "response"))
  
caret::modelLookup(m1)

postResample(pred = train$yp, obs = train$y)
postResample(pred = test$yp, obs = test$y)
