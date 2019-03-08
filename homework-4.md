Homework 4: Bags, Forests, Boosts, oh my
================
Yi Liu
3/8/2019

Problem 1
---------

Problem 7 from Chapter 8 in the text. To be specific, please use a sequence of `ntree` from 25 to 500 in steps of 25 and `mtry` from 3 to 9 for by 1.

Answer 1
--------

``` r
set.seed(1234)
df <- tbl_df(Boston)

for (k in 1:20){
  inTraining <- createDataPartition(df$medv, p = .75, list = F)
  training <- df[inTraining, ]
  testing <- df[-inTraining, ]
  mtry <- c(3:9)
  ntree <- seq(25, 500, len = 20)
  results <- tibble(trial = rep(NA, 140),
  mtry = rep(NA, 140),
  ntree = rep(NA, 140),
  mse = rep(NA, 140)) 
  for(i in 1:7){
    cat(sprintf('Trial: %s, mtry: %s --- %s\n', k, mtry[i], Sys.time()))
    for(j in 1:20){ 
      rf_train <- randomForest(medv ~ .,
                               data = training,
                               mtry = mtry[i],
                               ntree = ntree[j])
      mse <- mean((predict(rf_train, newdata = testing) - testing$medv)^2)
      results[(i-1)*20 + j, ] <- c(k, mtry[i], ntree[j], mse)
    }
  }
  if(exists("results_total")){
  results_total <- bind_rows(results_total, results)
  }
  else(
  results_total <- results
  )
}
```

    ## Trial: 1, mtry: 3 --- 2019-03-08 12:34:33
    ## Trial: 1, mtry: 4 --- 2019-03-08 12:34:38
    ## Trial: 1, mtry: 5 --- 2019-03-08 12:34:44
    ## Trial: 1, mtry: 6 --- 2019-03-08 12:34:50
    ## Trial: 1, mtry: 7 --- 2019-03-08 12:34:57
    ## Trial: 1, mtry: 8 --- 2019-03-08 12:35:06
    ## Trial: 1, mtry: 9 --- 2019-03-08 12:35:15
    ## Trial: 2, mtry: 3 --- 2019-03-08 12:35:26
    ## Trial: 2, mtry: 4 --- 2019-03-08 12:35:30
    ## Trial: 2, mtry: 5 --- 2019-03-08 12:35:36
    ## Trial: 2, mtry: 6 --- 2019-03-08 12:35:42
    ## Trial: 2, mtry: 7 --- 2019-03-08 12:35:50
    ## Trial: 2, mtry: 8 --- 2019-03-08 12:35:58
    ## Trial: 2, mtry: 9 --- 2019-03-08 12:36:08
    ## Trial: 3, mtry: 3 --- 2019-03-08 12:36:18
    ## Trial: 3, mtry: 4 --- 2019-03-08 12:36:23
    ## Trial: 3, mtry: 5 --- 2019-03-08 12:36:28
    ## Trial: 3, mtry: 6 --- 2019-03-08 12:36:35
    ## Trial: 3, mtry: 7 --- 2019-03-08 12:36:42
    ## Trial: 3, mtry: 8 --- 2019-03-08 12:36:51
    ## Trial: 3, mtry: 9 --- 2019-03-08 12:37:00
    ## Trial: 4, mtry: 3 --- 2019-03-08 12:37:10
    ## Trial: 4, mtry: 4 --- 2019-03-08 12:37:15
    ## Trial: 4, mtry: 5 --- 2019-03-08 12:37:21
    ## Trial: 4, mtry: 6 --- 2019-03-08 12:37:27
    ## Trial: 4, mtry: 7 --- 2019-03-08 12:37:35
    ## Trial: 4, mtry: 8 --- 2019-03-08 12:37:44
    ## Trial: 4, mtry: 9 --- 2019-03-08 12:37:53
    ## Trial: 5, mtry: 3 --- 2019-03-08 12:38:04
    ## Trial: 5, mtry: 4 --- 2019-03-08 12:38:08
    ## Trial: 5, mtry: 5 --- 2019-03-08 12:38:14
    ## Trial: 5, mtry: 6 --- 2019-03-08 12:38:21
    ## Trial: 5, mtry: 7 --- 2019-03-08 12:38:28
    ## Trial: 5, mtry: 8 --- 2019-03-08 12:38:36
    ## Trial: 5, mtry: 9 --- 2019-03-08 12:38:46
    ## Trial: 6, mtry: 3 --- 2019-03-08 12:38:56
    ## Trial: 6, mtry: 4 --- 2019-03-08 12:39:00
    ## Trial: 6, mtry: 5 --- 2019-03-08 12:39:06
    ## Trial: 6, mtry: 6 --- 2019-03-08 12:39:13
    ## Trial: 6, mtry: 7 --- 2019-03-08 12:39:20
    ## Trial: 6, mtry: 8 --- 2019-03-08 12:39:29
    ## Trial: 6, mtry: 9 --- 2019-03-08 12:39:38
    ## Trial: 7, mtry: 3 --- 2019-03-08 12:39:48
    ## Trial: 7, mtry: 4 --- 2019-03-08 12:39:53
    ## Trial: 7, mtry: 5 --- 2019-03-08 12:39:59
    ## Trial: 7, mtry: 6 --- 2019-03-08 12:40:05
    ## Trial: 7, mtry: 7 --- 2019-03-08 12:40:12
    ## Trial: 7, mtry: 8 --- 2019-03-08 12:40:20
    ## Trial: 7, mtry: 9 --- 2019-03-08 12:40:30
    ## Trial: 8, mtry: 3 --- 2019-03-08 12:40:40
    ## Trial: 8, mtry: 4 --- 2019-03-08 12:40:44
    ## Trial: 8, mtry: 5 --- 2019-03-08 12:40:50
    ## Trial: 8, mtry: 6 --- 2019-03-08 12:40:56
    ## Trial: 8, mtry: 7 --- 2019-03-08 12:41:03
    ## Trial: 8, mtry: 8 --- 2019-03-08 12:41:12
    ## Trial: 8, mtry: 9 --- 2019-03-08 12:41:21
    ## Trial: 9, mtry: 3 --- 2019-03-08 12:41:31
    ## Trial: 9, mtry: 4 --- 2019-03-08 12:41:35
    ## Trial: 9, mtry: 5 --- 2019-03-08 12:41:41
    ## Trial: 9, mtry: 6 --- 2019-03-08 12:41:47
    ## Trial: 9, mtry: 7 --- 2019-03-08 12:41:54
    ## Trial: 9, mtry: 8 --- 2019-03-08 12:42:03
    ## Trial: 9, mtry: 9 --- 2019-03-08 12:42:12
    ## Trial: 10, mtry: 3 --- 2019-03-08 12:42:22
    ## Trial: 10, mtry: 4 --- 2019-03-08 12:42:26
    ## Trial: 10, mtry: 5 --- 2019-03-08 12:42:32
    ## Trial: 10, mtry: 6 --- 2019-03-08 12:42:38
    ## Trial: 10, mtry: 7 --- 2019-03-08 12:42:46
    ## Trial: 10, mtry: 8 --- 2019-03-08 12:42:54
    ## Trial: 10, mtry: 9 --- 2019-03-08 12:43:04
    ## Trial: 11, mtry: 3 --- 2019-03-08 12:43:14
    ## Trial: 11, mtry: 4 --- 2019-03-08 12:43:19
    ## Trial: 11, mtry: 5 --- 2019-03-08 12:43:24
    ## Trial: 11, mtry: 6 --- 2019-03-08 12:43:31
    ## Trial: 11, mtry: 7 --- 2019-03-08 12:43:38
    ## Trial: 11, mtry: 8 --- 2019-03-08 12:43:47
    ## Trial: 11, mtry: 9 --- 2019-03-08 12:43:56
    ## Trial: 12, mtry: 3 --- 2019-03-08 12:44:06
    ## Trial: 12, mtry: 4 --- 2019-03-08 12:44:11
    ## Trial: 12, mtry: 5 --- 2019-03-08 12:44:17
    ## Trial: 12, mtry: 6 --- 2019-03-08 12:44:23
    ## Trial: 12, mtry: 7 --- 2019-03-08 12:44:31
    ## Trial: 12, mtry: 8 --- 2019-03-08 12:44:39
    ## Trial: 12, mtry: 9 --- 2019-03-08 12:44:48
    ## Trial: 13, mtry: 3 --- 2019-03-08 12:44:58
    ## Trial: 13, mtry: 4 --- 2019-03-08 12:45:03
    ## Trial: 13, mtry: 5 --- 2019-03-08 12:45:08
    ## Trial: 13, mtry: 6 --- 2019-03-08 12:45:15
    ## Trial: 13, mtry: 7 --- 2019-03-08 12:45:22
    ## Trial: 13, mtry: 8 --- 2019-03-08 12:45:31
    ## Trial: 13, mtry: 9 --- 2019-03-08 12:45:40
    ## Trial: 14, mtry: 3 --- 2019-03-08 12:45:50
    ## Trial: 14, mtry: 4 --- 2019-03-08 12:45:55
    ## Trial: 14, mtry: 5 --- 2019-03-08 12:46:00
    ## Trial: 14, mtry: 6 --- 2019-03-08 12:46:06
    ## Trial: 14, mtry: 7 --- 2019-03-08 12:46:14
    ## Trial: 14, mtry: 8 --- 2019-03-08 12:46:22
    ## Trial: 14, mtry: 9 --- 2019-03-08 12:46:31
    ## Trial: 15, mtry: 3 --- 2019-03-08 12:46:41
    ## Trial: 15, mtry: 4 --- 2019-03-08 12:46:46
    ## Trial: 15, mtry: 5 --- 2019-03-08 12:46:51
    ## Trial: 15, mtry: 6 --- 2019-03-08 12:46:58
    ## Trial: 15, mtry: 7 --- 2019-03-08 12:47:05
    ## Trial: 15, mtry: 8 --- 2019-03-08 12:47:13
    ## Trial: 15, mtry: 9 --- 2019-03-08 12:47:23
    ## Trial: 16, mtry: 3 --- 2019-03-08 12:47:33
    ## Trial: 16, mtry: 4 --- 2019-03-08 12:47:38
    ## Trial: 16, mtry: 5 --- 2019-03-08 12:47:43
    ## Trial: 16, mtry: 6 --- 2019-03-08 12:47:49
    ## Trial: 16, mtry: 7 --- 2019-03-08 12:47:57
    ## Trial: 16, mtry: 8 --- 2019-03-08 12:48:05
    ## Trial: 16, mtry: 9 --- 2019-03-08 12:48:14
    ## Trial: 17, mtry: 3 --- 2019-03-08 12:48:25
    ## Trial: 17, mtry: 4 --- 2019-03-08 12:48:29
    ## Trial: 17, mtry: 5 --- 2019-03-08 12:48:35
    ## Trial: 17, mtry: 6 --- 2019-03-08 12:48:41
    ## Trial: 17, mtry: 7 --- 2019-03-08 12:48:48
    ## Trial: 17, mtry: 8 --- 2019-03-08 12:48:57
    ## Trial: 17, mtry: 9 --- 2019-03-08 12:49:06
    ## Trial: 18, mtry: 3 --- 2019-03-08 12:49:16
    ## Trial: 18, mtry: 4 --- 2019-03-08 12:49:20
    ## Trial: 18, mtry: 5 --- 2019-03-08 12:49:26
    ## Trial: 18, mtry: 6 --- 2019-03-08 12:49:32
    ## Trial: 18, mtry: 7 --- 2019-03-08 12:49:40
    ## Trial: 18, mtry: 8 --- 2019-03-08 12:49:48
    ## Trial: 18, mtry: 9 --- 2019-03-08 12:49:58
    ## Trial: 19, mtry: 3 --- 2019-03-08 12:50:08
    ## Trial: 19, mtry: 4 --- 2019-03-08 12:50:13
    ## Trial: 19, mtry: 5 --- 2019-03-08 12:50:18
    ## Trial: 19, mtry: 6 --- 2019-03-08 12:50:25
    ## Trial: 19, mtry: 7 --- 2019-03-08 12:50:32
    ## Trial: 19, mtry: 8 --- 2019-03-08 12:50:40
    ## Trial: 19, mtry: 9 --- 2019-03-08 12:50:49
    ## Trial: 20, mtry: 3 --- 2019-03-08 12:50:59
    ## Trial: 20, mtry: 4 --- 2019-03-08 12:51:04
    ## Trial: 20, mtry: 5 --- 2019-03-08 12:51:09
    ## Trial: 20, mtry: 6 --- 2019-03-08 12:51:16
    ## Trial: 20, mtry: 7 --- 2019-03-08 12:51:23
    ## Trial: 20, mtry: 8 --- 2019-03-08 12:51:32
    ## Trial: 20, mtry: 9 --- 2019-03-08 12:51:41

``` r
p <- ggplot(data = results,
            aes(x = ntree, y= mse, col = as.factor(mtry)))
p + geom_line() +
  geom_point() +
  scale_color_brewer("mtry", palette = "Dark2")
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-2-1.png)

RMSE fluctuates inconsistently based on the number of trees - 50 looks like a good compromise between time to run and low error. In terms of mtry, RMSE improvement appears to stop at around 5 so that seems like a good option.

Problem 2
---------

Problem 8 from Chapter 8 in the text. Set your seed with 9823 and split into train/test using 50% of your data in each split. In addition to parts (a) - (e), do the following:

Answer 2
--------

#### (a) Split the data set into a training set and a test set.

``` r
df <- tbl_df(Carseats)
set.seed(9823)
inTraining <- createDataPartition(df$Sales, p = .5, list = F)
training <- df[inTraining, ]
testing  <- df[-inTraining, ]
```

#### (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain?

``` r
tree_carseats <- rpart(Sales ~ . , training)
prp(tree_carseats)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
test_pred <- predict(tree_carseats, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 4.484515

#### (c) Use cross-validation in order to determine the optimal level of

tree complexity. Does pruning the tree improve the test MSE?

``` r
set.seed(9823)
fit_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
cv_carseats_tree <- train(Sales ~ ., training, 
                          method = "rpart2", 
                          trControl = fit_control, 
                          tuneGrid = data.frame(maxdepth = 1:10))
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
plot(cv_carseats_tree)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-6-1.png)

Maxdepth of 3 gives the lowest RMSE, we will re-fit using that

``` r
set.seed(9823)
cv_carseats_tree3 <- rpart(Sales ~ ., training, maxdepth = 3)
prp(cv_carseats_tree3)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
test_pred <- predict(cv_carseats_tree3, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 4.933184

#### (d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important.

``` r
set.seed(9823)
carseats_bag <- randomForest(Sales ~ ., data = training, 
                             mtry = ncol(training)-1,
                             ntrees=500,
                             importance=TRUE)
test_pred <- predict(carseats_bag, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 3.01177

``` r
imp <- varImp(carseats_bag)
rn <- row.names(imp)
imp_df <- tibble(variable = rn, importance = imp$Overall) %>%
  arrange(desc(-importance)) %>%
  mutate(variable = factor(variable, variable))
p <- ggplot(data = imp_df, aes(variable, importance))
p + geom_col(fill = "#6e0000") +
  coord_flip()
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-10-1.png)

Shelf location and price are the two most important variables here

#### (e) Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables ar emost important. Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

``` r
set.seed(9823)
carseats_rf <- train(Sales ~ ., data = training,
                     method = "rf", ntree = 250,
                      importance = T, tuneGrid = data.frame(mtry = 2:10))
p <- ggplot(data = carseats_rf$results, aes(x = mtry, y = RMSE))
p + geom_point() +
  geom_line()
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-11-1.png)

mtry of 5-8 results in similiar RMSE, we will use 5 for simplicity

``` r
set.seed(9823)
carseats_rf5 <- randomForest(Sales ~ ., data = training, mtry = 5, importance = TRUE)
test_pred <- predict(carseats_rf5, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 3.12534

``` r
imp <- varImp(carseats_rf5)
rn <- row.names(imp)
imp_df <- tibble(variable = rn, importance = imp$Overall) %>%
  arrange(desc(-importance)) %>%
  mutate(variable = factor(variable, variable))
p <- ggplot(data = imp_df, aes(variable, importance))
p + geom_col(fill = "#6e0000") +
  coord_flip()
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-13-1.png)

Again, shelf location and price are most important

#### 1. Fit a gradient-boosted tree to the training data and report the estimated test MSE.

``` r
set.seed(9823)
grid <- expand.grid(interaction.depth = c(1, 3), 
                    n.trees = seq(0, 2000, by = 100),
                    shrinkage = c(.01, 0.001),
                    n.minobsinnode = 10)
trainControl <- trainControl(method = "cv", number = 5)
carseats_gb <- train(Sales ~ ., 
                     data = training,
                     distribution = "gaussian", 
                     method = "gbm",
                     trControl = trainControl, 
                     tuneGrid = grid,
                     verbose = FALSE)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
test_pred <- predict(carseats_gb, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 1.816239

#### 2. Fit a multiple regression model to the training data and report the estimated test MSE.

``` r
carseats_lm <- lm(Sales ~ .,data = training)
test_pred <- predict(carseats_lm, newdata = testing)
mean((test_pred - testing$Sales)^2)
```

    ## [1] 1.012709

#### 3. Summarize your results.

Tree error MSE steadily improved throughout the problem, with the original tree & cross-validation of depth around 4.5-5. Bagging and random forest dropped MSE to around 3, then boosting dropped to 1.8. However, a multiple regression with all variables was best at 1.01 and the easiest to explain.
