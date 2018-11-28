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
                eta = 1,
                max_depth = 6,
                nthread = 16,
                objective = "multi:softmax",
                num_class = 7,
                eval_metric = "merror"
        ),
        data = dtrain,
        nrounds = 100,
        nfold = 5,
        metrics = "merror",
        early_stopping_rounds = 5L)
