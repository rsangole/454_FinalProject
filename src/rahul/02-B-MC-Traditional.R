set.seed(42)

# Common definitions
parallelMap::parallelStart(mode = "multicore")
#------
source("src/rahul/02-A-MC-DataPrep.R")
train_data <- list(
        data = as.matrix(df_xgbpkg_prep_train[, -1]),
        label = as.numeric(df_xgbpkg_prep_train$cover_type) - 1
)
test_data <- list(
        data = as.matrix(df_xgbpkg_prep_test[, -1]),
        label = as.numeric(df_xgbpkg_prep_test$cover_type) - 1
)
(dtrain <- xgb.DMatrix(data = train_data$data, label = train_data$label))
(dtest <- xgb.DMatrix(data = test_data$data, label = test_data$label))

#-------------Basic Training using XGBoost-----------------
# you can also put in xgb.DMatrix object, which stores label, data and other meta datas needed for advanced features
print("Training xgboost with xgb.DMatrix")
watchlist <- list(train = dtrain, test = dtest)
bst <- xgb.train(
        params = list(
                eta = 1,
                max_depth = 10,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                alpha = 0.1
        ),
        data = dtrain,
        nrounds = 20L,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst, feature_names = bst$feature_names), top_n = 15)
bst_class <-predict(bst, newdata = dtrain)
caret::confusionMatrix(factor(bst_class), factor(train_data$label))
bst_class <- predict(bst, newdata = dtest)
caret::confusionMatrix(factor(bst_class), factor(test_data$label))

ptrain <- predict(bst, dtrain, outputmargin=TRUE)
ptest  <- predict(bst, dtest, outputmargin=TRUE)
# set the base_margin property of dtrain and dtest
# base margin is the base prediction we will boost from
setinfo(dtrain, "base_margin", ptrain)
setinfo(dtest, "base_margin", ptest)
bst <- xgb.train(
        params = list(
                eta = 1,
                max_depth = 10,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                alpha = 0.1
        ),
        data = dtrain,
        nrounds = 20L,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst


## CV to search for the best value of nrounds
search_using_cv <- xgb.cv(
        params = list(
                eta = .5,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 5,
                subsample = 0.8,
                colsample_by_tree = 0.8
        ),
        data = dtrain,
        nrounds = 120,
        nfold = 5,
        metrics = "merror",
        early_stopping_rounds = 5L)

# Best iteration:
#         iter train_merror_mean train_merror_std test_merror_mean test_merror_std
# 54         0.2154822       0.01160932        0.2202504      0.01164988
#
# Best iteration:
#         iter train_merror_mean train_merror_std test_merror_mean test_merror_std
# 120          1.34e-05     6.151423e-06         0.036463    0.0004009459

#model - xbg-trad-001
watchlist <- list(train = dtrain, test = dtest)
bst <- xgb.train(
        params = list(
                eta = .5,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 5,
                subsample = 0.8,
                colsample_by_tree = 0.8
                ),
        data = dtrain,
        nrounds = 120L,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst
save(bst,file = "src/rahul/xgb_trad_001.Rdata")
load("src/rahul/xgb_trad_001.Rdata")
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst, feature_names = bst$feature_names), top_n = 15)
bst_class <-predict(bst, newdata = dtrain)
caret::confusionMatrix(factor(bst_class), factor(train_data$label))
bst_class <- predict(bst, newdata = dtest)
caret::confusionMatrix(factor(bst_class), factor(test_data$label))
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1     2     3     4     5     6
# 0 61354  1572     0     0    22     5   184
# 1  2041 83094   122     0   316    86    37
# 2     1   101 10360    61    21   190     0
# 3     0     0    50   740     0    21     0
# 4    28   133     9     0  2474     7     1
# 5     4    69   185    23    12  4901     0
# 6   124    21     0     0     2     0  5931
#
# Overall Statistics
#
# Accuracy : 0.9687
# 95% CI : (0.9679, 0.9696)
# No Information Rate : 0.4876
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.9498
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9654   0.9777  0.96588 0.898058  0.86898  0.94069  0.96392
# Specificity            0.9839   0.9709  0.99771 0.999591  0.99896  0.99827  0.99913
# Pos Pred Value         0.9718   0.9696  0.96516 0.912454  0.93288  0.94359  0.97581
# Neg Pred Value         0.9802   0.9786  0.99776 0.999516  0.99783  0.99817  0.99868
# Prevalence             0.3646   0.4876  0.06154 0.004727  0.01633  0.02989  0.03530
# Detection Rate         0.3520   0.4767  0.05944 0.004246  0.01419  0.02812  0.03403
# Detection Prevalence   0.3622   0.4917  0.06158 0.004653  0.01521  0.02980  0.03487
# Balanced Accuracy      0.9747   0.9743  0.98180 0.948824  0.93397  0.96948  0.98152

#model - xbg-trad-001-02
bst00102 <- xgb.train(
        params = list(
                eta = .3,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 1,
                subsample = 1,
                colsample_by_tree = 1
        ),
        data = dtrain,
        nrounds = 120L,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst00102
save(bst00102,file = "src/rahul/bst00102.Rdata")
load("src/rahul/bst00102.Rdata")
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst00102, feature_names = bst00102$feature_names), top_n = 15)
bst00102_class <-predict(bst00102, newdata = dtrain)
caret::confusionMatrix(factor(bst00102_class), factor(train_data$label))
bst00102_class <- predict(bst00102, newdata = dtest)
caret::confusionMatrix(factor(bst00102_class), factor(test_data$label))
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1     2     3     4     5     6
# 0 61482  1397     0     0    26     3   165
# 1  1934 83291   121     0   308    88    41
# 2     2    76 10391    74    19   165     0
# 3     0     0    41   728     0    18     0
# 4    22   135    12     0  2480     4     1
# 5     2    68   161    22    12  4932     0
# 6   110    23     0     0     2     0  5946
#
# Overall Statistics
#
# Accuracy : 0.971
# 95% CI : (0.9702, 0.9718)
# No Information Rate : 0.4876
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.9534
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9674   0.9800  0.96877 0.883495  0.87109  0.94664  0.96636
# Specificity            0.9856   0.9721  0.99795 0.999660  0.99899  0.99843  0.99920
# Pos Pred Value         0.9748   0.9709  0.96868 0.925032  0.93444  0.94901  0.97780
# Neg Pred Value         0.9814   0.9808  0.99795 0.999447  0.99786  0.99836  0.99877
# Prevalence             0.3646   0.4876  0.06154 0.004727  0.01633  0.02989  0.03530
# Detection Rate         0.3527   0.4779  0.05961 0.004177  0.01423  0.02830  0.03411
# Detection Prevalence   0.3619   0.4922  0.06154 0.004515  0.01523  0.02982  0.03489
# Balanced Accuracy      0.9765   0.9761  0.98336 0.941578  0.93504  0.97254  0.98278
bst00102$evaluation_log %>% gather(key = error_type, value = error, -iter) %>%
        xyplot(error~iter,groups=error_type, .)

#---- Same xgb parameters, but different dataset
# xgb-trad-002
source("src/rahul/02-A-MC-DataPrep.R")
train_data <- list(
        data = as.matrix(df_xgbpkg_prep_train[, c(-1,-56:-63)]),
        label = as.numeric(df_xgbpkg_prep_train$cover_type) - 1
)
test_data <- list(
        data = as.matrix(df_xgbpkg_prep_test[, c(-1,-56:-63)]),
        label = as.numeric(df_xgbpkg_prep_test$cover_type) - 1
)
(dtrain <- xgb.DMatrix(data = train_data$data, label = train_data$label))
(dtest <- xgb.DMatrix(data = test_data$data, label = test_data$label))
watchlist <- list(train = dtrain, test = dtest)
bst2 <- xgb.train(
        params = list(
                eta = .3,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 1,
                subsample = 1,
                colsample_by_tree = 1
        ),
        data = dtrain,
        nrounds = 120L,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst2
save(bst2,file = "src/rahul/xgb_trad_002.Rdata")
load("src/rahul/xgb_trad_002.Rdata")
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst2, feature_names = bst2$feature_names), top_n = 15)
bst2_class <-predict(bst2, newdata = dtrain)
caret::confusionMatrix(factor(bst2_class), factor(train_data$label))
bst2_class <- predict(bst2, newdata = dtest)
caret::confusionMatrix(factor(bst2_class), factor(test_data$label))
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1     2     3     4     5     6
# 0 61401  1434     0     0    20     6   170
# 1  2018 83262   130     0   300    88    39
# 2     2    87 10381    63    24   180     0
# 3     0     0    40   737     0    23     0
# 4    19   132     9     0  2490     4     1
# 5     0    54   166    24    12  4909     0
# 6   112    21     0     0     1     0  5943
#
# Overall Statistics
#
# Accuracy : 0.9703
# 95% CI : (0.9695, 0.9711)
# No Information Rate : 0.4876
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.9522
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9662   0.9797  0.96784 0.894417  0.87460  0.94223  0.96587
# Specificity            0.9853   0.9712  0.99782 0.999637  0.99904  0.99849  0.99920
# Pos Pred Value         0.9741   0.9700  0.96684 0.921250  0.93785  0.95044  0.97795
# Neg Pred Value         0.9807   0.9805  0.99789 0.999499  0.99792  0.99822  0.99875
# Prevalence             0.3646   0.4876  0.06154 0.004727  0.01633  0.02989  0.03530
# Detection Rate         0.3523   0.4777  0.05956 0.004228  0.01429  0.02816  0.03410
# Detection Prevalence   0.3616   0.4925  0.06160 0.004590  0.01523  0.02963  0.03486
# Balanced Accuracy      0.9757   0.9754  0.98283 0.947027  0.93682  0.97036  0.98254



#----- From tsne EDA, adding one more feature
source("src/rahul/02-A-MC-DataPrep.R")
df_xgbpkg_prep_train <- df_xgbpkg_prep_train %>%
        # bind_cols(train_cols_to_append) %>%
        transform_wilderness_to_factor() %>%
        transform_soil_to_factor() %>%
        mutate(wilderness_area=as.numeric(wilderness_area),
               soil_type = as.numeric(soil_type),
               log_sw = log(soil_type*wilderness_area))
df_xgbpkg_prep_test <- df_xgbpkg_prep_test %>%
        # bind_cols(train_cols_to_append) %>%
        transform_wilderness_to_factor() %>%
        transform_soil_to_factor() %>%
        mutate(wilderness_area=as.numeric(wilderness_area),
               soil_type = as.numeric(soil_type),
               log_sw = log(soil_type*wilderness_area))
train_data <- list(
        data = as.matrix(df_xgbpkg_prep_train[, c(-1)]),
        label = as.numeric(df_xgbpkg_prep_train$cover_type) - 1
)
test_data <- list(
        data = as.matrix(df_xgbpkg_prep_test[, c(-1)]),
        label = as.numeric(df_xgbpkg_prep_test$cover_type) - 1
)
(dtrain <- xgb.DMatrix(data = train_data$data, label = train_data$label))
(dtest <- xgb.DMatrix(data = test_data$data, label = test_data$label))
watchlist <- list(train = dtrain, test = dtest)
bst3 <- xgb.train(
        params = list(
                eta = .3,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 1,
                subsample = 1,
                colsample_by_tree = 1
        ),
        data = dtrain,
        nrounds = 120,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst3
save(bst3,file = "src/rahul/xgb_trad_003.Rdata")
load("src/rahul/xgb_trad_003.Rdata")
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst3, feature_names = bst3$feature_names), top_n = 15)
bst3_class <-predict(bst3, newdata = dtrain)
caret::confusionMatrix(factor(bst3_class), factor(train_data$label))
bst3_class <- predict(bst3, newdata = dtest)
caret::confusionMatrix(factor(bst3_class), factor(test_data$label))
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0     1     2     3     4     5     6
# 0 61529  1388     0     0    21     4   171
# 1  1882 83338   135     0   309    89    41
# 2     3    68 10282    68    12   158     0
# 3     0     0    44   730     0    18     0
# 4    23   121    14     0  2490     2     1
# 5     2    56   251    26    13  4939     0
# 6   113    19     0     0     2     0  5940
#
# Overall Statistics
#
# Accuracy : 0.971
# 95% CI : (0.9702, 0.9718)
# No Information Rate : 0.4876
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.9534
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9682   0.9806  0.95861 0.885922  0.87460  0.94798  0.96538
# Specificity            0.9857   0.9725  0.99811 0.999643  0.99906  0.99794  0.99920
# Pos Pred Value         0.9749   0.9714  0.97082 0.921717  0.93927  0.93418  0.97794
# Neg Pred Value         0.9818   0.9813  0.99729 0.999458  0.99792  0.99840  0.99873
# Prevalence             0.3646   0.4876  0.06154 0.004727  0.01633  0.02989  0.03530
# Detection Rate         0.3530   0.4781  0.05899 0.004188  0.01429  0.02834  0.03408
# Detection Prevalence   0.3621   0.4922  0.06076 0.004544  0.01521  0.03033  0.03485
# Balanced Accuracy      0.9769   0.9765  0.97836 0.942782  0.93683  0.97296  0.98229

bst3$evaluation_log %>% gather(key = error_type, value = error, -iter) %>%
        xyplot(error~iter,groups=error_type, .)


#-------------------------------------------
#------------------004----kitchen sink model -----
#----- From tsne EDA, adding one more feature
source("src/rahul/02-A-MC-DataPrep.R")
train_soil_wild_cols <- df_xgbpkg_prep_train %>% select(contains("wild"),contains("soil"))
test_soil_wild_cols <- df_xgbpkg_prep_test %>% select(contains("wild"),contains("soil"))
df_xgbpkg_prep_train <- df_xgbpkg_prep_train %>%
        transform_wilderness_to_factor() %>%
        transform_soil_to_factor() %>%
        mutate(wilderness_area=as.numeric(wilderness_area),
               soil_type = as.numeric(soil_type),
               log_sw = log(soil_type*wilderness_area)) %>%
        bind_cols(train_soil_wild_cols)
df_xgbpkg_prep_test <- df_xgbpkg_prep_test %>%
        # bind_cols(train_cols_to_append) %>%
        transform_wilderness_to_factor() %>%
        transform_soil_to_factor() %>%
        mutate(wilderness_area=as.numeric(wilderness_area),
               soil_type = as.numeric(soil_type),
               log_sw = log(soil_type*wilderness_area)) %>%
        bind_cols(test_soil_wild_cols)
train_data <- list(
        data = as.matrix(df_xgbpkg_prep_train[, c(-1)]),
        label = as.numeric(df_xgbpkg_prep_train$cover_type) - 1
)
test_data <- list(
        data = as.matrix(df_xgbpkg_prep_test[, c(-1)]),
        label = as.numeric(df_xgbpkg_prep_test$cover_type) - 1
)
(dtrain <- xgb.DMatrix(data = train_data$data, label = train_data$label))
(dtest <- xgb.DMatrix(data = test_data$data, label = test_data$label))
watchlist <- list(train = dtrain, test = dtest)
bst4 <- xgb.train(
        params = list(
                eta = .3,
                max_depth = 14,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror",
                min_child_weight = 1,
                subsample = 1,
                colsample_by_tree = 1
        ),
        data = dtrain,
        nrounds = 120,
        watchlist = watchlist,
        early_stopping_rounds = 10L
)
bst4
save(bst4,file = "src/rahul/xgb_trad_004.Rdata")
load("src/rahul/xgb_trad_004.Rdata")
xgb.ggplot.importance(importance_matrix = xgb.importance(model = bst4, feature_names = bst4$feature_names), top_n = 15)
bst4_class <-predict(bst4, newdata = dtrain)
caret::confusionMatrix(factor(bst4_class), factor(train_data$label))
bst4_class <- predict(bst3, newdata = dtest)
caret::confusionMatrix(factor(bst4_class), factor(test_data$label))
