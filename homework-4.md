Homework 4: Bags, Forests, Boosts, oh my
================
Yi Liu
3/8/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  2.0.1     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ purrr::accumulate() masks foreach::accumulate()
    ## ✖ dplyr::filter()     masks stats::filter()
    ## ✖ dplyr::lag()        masks stats::lag()
    ## ✖ purrr::when()       masks foreach::when()

``` r
library(broom)
library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loaded glmnet 2.0-16

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(ISLR)
library(janitor)
library(stringr)
library(rpart)
library(rpart.plot)
library(partykit)
```

    ## Loading required package: grid

    ## Loading required package: libcoin

    ## Loading required package: mvtnorm

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(gbm)
```

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:rpart':
    ## 
    ##     solder

    ## The following object is masked from 'package:caret':
    ## 
    ##     cluster

    ## Loading required package: splines

    ## Loaded gbm 2.1.3

``` r
theme_set(theme_bw())
```

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

    ## Trial: 1, mtry: 3 --- 2019-03-09 19:49:22
    ## Trial: 1, mtry: 4 --- 2019-03-09 19:49:27
    ## Trial: 1, mtry: 5 --- 2019-03-09 19:49:33
    ## Trial: 1, mtry: 6 --- 2019-03-09 19:49:39
    ## Trial: 1, mtry: 7 --- 2019-03-09 19:49:47
    ## Trial: 1, mtry: 8 --- 2019-03-09 19:49:55
    ## Trial: 1, mtry: 9 --- 2019-03-09 19:50:05
    ## Trial: 2, mtry: 3 --- 2019-03-09 19:50:15
    ## Trial: 2, mtry: 4 --- 2019-03-09 19:50:20
    ## Trial: 2, mtry: 5 --- 2019-03-09 19:50:25
    ## Trial: 2, mtry: 6 --- 2019-03-09 19:50:32
    ## Trial: 2, mtry: 7 --- 2019-03-09 19:50:40
    ## Trial: 2, mtry: 8 --- 2019-03-09 19:50:48
    ## Trial: 2, mtry: 9 --- 2019-03-09 19:50:57
    ## Trial: 3, mtry: 3 --- 2019-03-09 19:51:07
    ## Trial: 3, mtry: 4 --- 2019-03-09 19:51:12
    ## Trial: 3, mtry: 5 --- 2019-03-09 19:51:17
    ## Trial: 3, mtry: 6 --- 2019-03-09 19:51:24
    ## Trial: 3, mtry: 7 --- 2019-03-09 19:51:31
    ## Trial: 3, mtry: 8 --- 2019-03-09 19:51:40
    ## Trial: 3, mtry: 9 --- 2019-03-09 19:51:49
    ## Trial: 4, mtry: 3 --- 2019-03-09 19:51:59
    ## Trial: 4, mtry: 4 --- 2019-03-09 19:52:04
    ## Trial: 4, mtry: 5 --- 2019-03-09 19:52:10
    ## Trial: 4, mtry: 6 --- 2019-03-09 19:52:16
    ## Trial: 4, mtry: 7 --- 2019-03-09 19:52:24
    ## Trial: 4, mtry: 8 --- 2019-03-09 19:52:32
    ## Trial: 4, mtry: 9 --- 2019-03-09 19:52:42
    ## Trial: 5, mtry: 3 --- 2019-03-09 19:52:52
    ## Trial: 5, mtry: 4 --- 2019-03-09 19:52:57
    ## Trial: 5, mtry: 5 --- 2019-03-09 19:53:02
    ## Trial: 5, mtry: 6 --- 2019-03-09 19:53:09
    ## Trial: 5, mtry: 7 --- 2019-03-09 19:53:16
    ## Trial: 5, mtry: 8 --- 2019-03-09 19:53:25
    ## Trial: 5, mtry: 9 --- 2019-03-09 19:53:34
    ## Trial: 6, mtry: 3 --- 2019-03-09 19:53:44
    ## Trial: 6, mtry: 4 --- 2019-03-09 19:53:49
    ## Trial: 6, mtry: 5 --- 2019-03-09 19:53:54
    ## Trial: 6, mtry: 6 --- 2019-03-09 19:54:01
    ## Trial: 6, mtry: 7 --- 2019-03-09 19:54:08
    ## Trial: 6, mtry: 8 --- 2019-03-09 19:54:17
    ## Trial: 6, mtry: 9 --- 2019-03-09 19:54:26
    ## Trial: 7, mtry: 3 --- 2019-03-09 19:54:37
    ## Trial: 7, mtry: 4 --- 2019-03-09 19:54:41
    ## Trial: 7, mtry: 5 --- 2019-03-09 19:54:47
    ## Trial: 7, mtry: 6 --- 2019-03-09 19:54:53
    ## Trial: 7, mtry: 7 --- 2019-03-09 19:55:01
    ## Trial: 7, mtry: 8 --- 2019-03-09 19:55:10
    ## Trial: 7, mtry: 9 --- 2019-03-09 19:55:19
    ## Trial: 8, mtry: 3 --- 2019-03-09 19:55:29
    ## Trial: 8, mtry: 4 --- 2019-03-09 19:55:34
    ## Trial: 8, mtry: 5 --- 2019-03-09 19:55:39
    ## Trial: 8, mtry: 6 --- 2019-03-09 19:55:46
    ## Trial: 8, mtry: 7 --- 2019-03-09 19:55:53
    ## Trial: 8, mtry: 8 --- 2019-03-09 19:56:02
    ## Trial: 8, mtry: 9 --- 2019-03-09 19:56:11
    ## Trial: 9, mtry: 3 --- 2019-03-09 19:56:21
    ## Trial: 9, mtry: 4 --- 2019-03-09 19:56:25
    ## Trial: 9, mtry: 5 --- 2019-03-09 19:56:31
    ## Trial: 9, mtry: 6 --- 2019-03-09 19:56:37
    ## Trial: 9, mtry: 7 --- 2019-03-09 19:56:44
    ## Trial: 9, mtry: 8 --- 2019-03-09 19:56:53
    ## Trial: 9, mtry: 9 --- 2019-03-09 19:57:02
    ## Trial: 10, mtry: 3 --- 2019-03-09 19:57:12
    ## Trial: 10, mtry: 4 --- 2019-03-09 19:57:16
    ## Trial: 10, mtry: 5 --- 2019-03-09 19:57:22
    ## Trial: 10, mtry: 6 --- 2019-03-09 19:57:29
    ## Trial: 10, mtry: 7 --- 2019-03-09 19:57:36
    ## Trial: 10, mtry: 8 --- 2019-03-09 19:57:44
    ## Trial: 10, mtry: 9 --- 2019-03-09 19:57:54
    ## Trial: 11, mtry: 3 --- 2019-03-09 19:58:04
    ## Trial: 11, mtry: 4 --- 2019-03-09 19:58:09
    ## Trial: 11, mtry: 5 --- 2019-03-09 19:58:15
    ## Trial: 11, mtry: 6 --- 2019-03-09 19:58:21
    ## Trial: 11, mtry: 7 --- 2019-03-09 19:58:29
    ## Trial: 11, mtry: 8 --- 2019-03-09 19:58:38
    ## Trial: 11, mtry: 9 --- 2019-03-09 19:58:47
    ## Trial: 12, mtry: 3 --- 2019-03-09 19:58:58
    ## Trial: 12, mtry: 4 --- 2019-03-09 19:59:02
    ## Trial: 12, mtry: 5 --- 2019-03-09 19:59:08
    ## Trial: 12, mtry: 6 --- 2019-03-09 19:59:14
    ## Trial: 12, mtry: 7 --- 2019-03-09 19:59:22
    ## Trial: 12, mtry: 8 --- 2019-03-09 19:59:30
    ## Trial: 12, mtry: 9 --- 2019-03-09 19:59:40
    ## Trial: 13, mtry: 3 --- 2019-03-09 19:59:50
    ## Trial: 13, mtry: 4 --- 2019-03-09 19:59:54
    ## Trial: 13, mtry: 5 --- 2019-03-09 20:00:00
    ## Trial: 13, mtry: 6 --- 2019-03-09 20:00:06
    ## Trial: 13, mtry: 7 --- 2019-03-09 20:00:14
    ## Trial: 13, mtry: 8 --- 2019-03-09 20:00:22
    ## Trial: 13, mtry: 9 --- 2019-03-09 20:00:32
    ## Trial: 14, mtry: 3 --- 2019-03-09 20:00:42
    ## Trial: 14, mtry: 4 --- 2019-03-09 20:00:46
    ## Trial: 14, mtry: 5 --- 2019-03-09 20:00:52
    ## Trial: 14, mtry: 6 --- 2019-03-09 20:00:58
    ## Trial: 14, mtry: 7 --- 2019-03-09 20:01:06
    ## Trial: 14, mtry: 8 --- 2019-03-09 20:01:14
    ## Trial: 14, mtry: 9 --- 2019-03-09 20:01:24
    ## Trial: 15, mtry: 3 --- 2019-03-09 20:01:34
    ## Trial: 15, mtry: 4 --- 2019-03-09 20:01:38
    ## Trial: 15, mtry: 5 --- 2019-03-09 20:01:44
    ## Trial: 15, mtry: 6 --- 2019-03-09 20:01:50
    ## Trial: 15, mtry: 7 --- 2019-03-09 20:01:58
    ## Trial: 15, mtry: 8 --- 2019-03-09 20:02:06
    ## Trial: 15, mtry: 9 --- 2019-03-09 20:02:15
    ## Trial: 16, mtry: 3 --- 2019-03-09 20:02:25
    ## Trial: 16, mtry: 4 --- 2019-03-09 20:02:29
    ## Trial: 16, mtry: 5 --- 2019-03-09 20:02:35
    ## Trial: 16, mtry: 6 --- 2019-03-09 20:02:41
    ## Trial: 16, mtry: 7 --- 2019-03-09 20:02:49
    ## Trial: 16, mtry: 8 --- 2019-03-09 20:02:57
    ## Trial: 16, mtry: 9 --- 2019-03-09 20:03:06
    ## Trial: 17, mtry: 3 --- 2019-03-09 20:03:16
    ## Trial: 17, mtry: 4 --- 2019-03-09 20:03:21
    ## Trial: 17, mtry: 5 --- 2019-03-09 20:03:26
    ## Trial: 17, mtry: 6 --- 2019-03-09 20:03:33
    ## Trial: 17, mtry: 7 --- 2019-03-09 20:03:40
    ## Trial: 17, mtry: 8 --- 2019-03-09 20:03:48
    ## Trial: 17, mtry: 9 --- 2019-03-09 20:03:57
    ## Trial: 18, mtry: 3 --- 2019-03-09 20:04:07
    ## Trial: 18, mtry: 4 --- 2019-03-09 20:04:12
    ## Trial: 18, mtry: 5 --- 2019-03-09 20:04:18
    ## Trial: 18, mtry: 6 --- 2019-03-09 20:04:24
    ## Trial: 18, mtry: 7 --- 2019-03-09 20:04:32
    ## Trial: 18, mtry: 8 --- 2019-03-09 20:04:40
    ## Trial: 18, mtry: 9 --- 2019-03-09 20:04:49
    ## Trial: 19, mtry: 3 --- 2019-03-09 20:05:00
    ## Trial: 19, mtry: 4 --- 2019-03-09 20:05:05
    ## Trial: 19, mtry: 5 --- 2019-03-09 20:05:10
    ## Trial: 19, mtry: 6 --- 2019-03-09 20:05:17
    ## Trial: 19, mtry: 7 --- 2019-03-09 20:05:24
    ## Trial: 19, mtry: 8 --- 2019-03-09 20:05:33
    ## Trial: 19, mtry: 9 --- 2019-03-09 20:05:42
    ## Trial: 20, mtry: 3 --- 2019-03-09 20:05:52
    ## Trial: 20, mtry: 4 --- 2019-03-09 20:05:56
    ## Trial: 20, mtry: 5 --- 2019-03-09 20:06:02
    ## Trial: 20, mtry: 6 --- 2019-03-09 20:06:09
    ## Trial: 20, mtry: 7 --- 2019-03-09 20:06:16
    ## Trial: 20, mtry: 8 --- 2019-03-09 20:06:24
    ## Trial: 20, mtry: 9 --- 2019-03-09 20:06:34

``` r
p <- ggplot(data = results,
            aes(x = ntree, y= mse, col = as.factor(mtry)))
p + geom_line() +
  geom_point() +
  scale_color_brewer("mtry", palette = "Dark2")
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-3-1.png)

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

![](homework-4_files/figure-markdown_github/unnamed-chunk-5-1.png)

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

![](homework-4_files/figure-markdown_github/unnamed-chunk-7-1.png)

Maxdepth of 3 gives the lowest RMSE, we will re-fit using that

``` r
set.seed(9823)
cv_carseats_tree3 <- rpart(Sales ~ ., training, maxdepth = 3)
prp(cv_carseats_tree3)
```

![](homework-4_files/figure-markdown_github/unnamed-chunk-8-1.png)

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

![](homework-4_files/figure-markdown_github/unnamed-chunk-11-1.png)

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

![](homework-4_files/figure-markdown_github/unnamed-chunk-12-1.png)

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

![](homework-4_files/figure-markdown_github/unnamed-chunk-14-1.png)

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

Tree error MSE steadily improved throughout the problem, with the original tree & cross-validation of depth around 4.5. Bagging and random forest dropped MSE to around 3, then boosting dropped to 1.8. However, a multiple regression with all variables was best at 1.01 and the easiest to explain.
