# Common definitions
parallelMap::parallelStart(mode = "multicore")
library(xgboost)
library(mlr)
library(tidyverse)
source("munge/01-C-FeatureEngg.R")

load("cache/raw_train.rdata")
load("cache/raw_test.rdata")
glimpse(raw_train)

df_xgbpkg_prep_train <- raw_train %>%
        add_boxbox_transform() %>%
        add_distances_features() %>%
        make_all_responses_numeric_datatype() %>%
        make_response_var_the_first_var() %>%
        as.data.frame()
df_xgbpkg_prep_test <- raw_test %>%
        add_boxbox_transform() %>%
        add_distances_features() %>%
        make_all_responses_numeric_datatype() %>%
        make_response_var_the_first_var() %>%
        as.data.frame()


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


(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L))
(meas_binary <- list(mmce, f1, kappa, fpr, fnr, auc))

make_watchlist <- function(d1,d2){
        train_data <- list(
                data = as.matrix(d1[, -1]),
                label = as.numeric(d1$cover_type) - 1)
        test_data <- list(
                data = as.matrix(d2[, -1]),
                label = as.numeric(d2$cover_type) - 1)
        dtrain <- xgb.DMatrix(data = train_data$data, label = train_data$label)
        dtest <- xgb.DMatrix(data = test_data$data, label = test_data$label)
        list(train = dtrain, test = dtest)
}

df_spruce <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("spruce_fir")
df_spruce_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("spruce_fir")
(tsk_spruce <- makeClassifTask(data = df_spruce, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_spruce, df_spruce_test)))
fit_spruce <- train(lrn, tsk_spruce)

df_lodgepole_pine <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("lodgepole_pine")
df_lodgepole_pine_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("lodgepole_pine")
(tsk_lodgepole_pine <- makeClassifTask(data = df_lodgepole_pine, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_lodgepole_pine, df_lodgepole_pine_test)))
fit_lodgepole_pine <- train(lrn, tsk_lodgepole_pine)

df_ponderosa_pine <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("ponderosa_pine")
df_ponderosa_pine_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("ponderosa_pine")
(tsk_ponderosa_pine <- makeClassifTask(data = df_ponderosa_pine, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_ponderosa_pine, df_ponderosa_pine_test)))
fit_ponderosa_pine <- train(lrn, tsk_ponderosa_pine)

df_cottonwood_Willow <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("cottonwood_Willow")
df_cottonwood_Willow_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("cottonwood_Willow")
(tsk_cottonwood_Willow <- makeClassifTask(data = df_cottonwood_Willow, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_cottonwood_Willow, df_cottonwood_Willow_test)))
fit_cottonwood_Willow <- train(lrn, tsk_cottonwood_Willow)

df_aspen <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("aspen")
df_aspen_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("aspen")
(tsk_aspen <- makeClassifTask(data = df_aspen, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_aspen, df_aspen_test)))
fit_aspen <- train(lrn, tsk_aspen)

df_douglasfir <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("douglasfir")
df_douglasfir_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("douglasfir")
(tsk_douglasfir <- makeClassifTask(data = df_douglasfir, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_douglasfir, df_douglasfir_test)))
fit_douglasfir <- train(lrn, tsk_douglasfir)

df_krummholz <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("krummholz")
df_krummholz_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("krummholz")
(tsk_krummholz <- makeClassifTask(data = df_krummholz, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 10, nrounds = 300L,
                    watchlist = make_watchlist(df_krummholz, df_krummholz_test)))
fit_krummholz <- train(lrn, tsk_krummholz)


pred__spruce_test<- predict(fit_spruce, newdata = df_spruce_test)
pred__lodgepole_pine_test<- predict(fit_lodgepole_pine, newdata = df_lodgepole_pine_test)
pred__ponderosa_pine_test<- predict(fit_ponderosa_pine, newdata = df_ponderosa_pine_test)
pred__cottonwood_Willow_test<- predict(fit_cottonwood_Willow, newdata = df_cottonwood_Willow_test)
pred__aspen_test<- predict(fit_aspen, newdata = df_aspen_test)
pred__douglasfir_test<- predict(fit_douglasfir, newdata = df_douglasfir_test)
pred__krummholz_test<- predict(fit_krummholz, newdata = df_krummholz_test)

results <- tibble(
        class = c(
                "spruce_fir",
                "lodgepole_pine",
                "ponderosa_pine",
                "cottonwood_Willow",
                "aspen",
                "douglasfir",
                "krummholz"
        ),
        predictions = list(pred__spruce_test,
                        pred__lodgepole_pine_test,
                        pred__ponderosa_pine_test,
                        pred__cottonwood_Willow_test,
                        pred__aspen_test,
                        pred__douglasfir_test,
                        pred__krummholz_test)
) %>%
        mutate(
                perf = map(predictions, ~performance(.x, meas_binary)),
                mmce = map_dbl(perf, ~.x[["mmce"]]),
                auc = map_dbl(perf, ~.x[["auc"]]),
                kappa = map_dbl(perf, ~.x[["kappa"]]),
                pred_class = map(predictions, ~getPredictionResponse(.x)),
                pred_prob = map(predictions, ~getPredictionProbabilities(.x))
        ) %>%
        arrange(auc,kappa,mmce)
results %>%
        ggplot(aes(y=kappa,x=auc))+
        geom_point(aes(color=class))

save(results, file = "src/rahul/cache/02_c_results.RData")

#Result --> multiclass does better than this 1vall


calculateROCMeasures(pred__lodgepole_pine_test)
calculateROCMeasures(pred__ponderosa_pine_test)
calculateROCMeasures(pred__cottonwood_Willow_test)
calculateROCMeasures(pred__aspen_test)
calculateROCMeasures(pred__douglasfir_test)
calculateROCMeasures(pred__krummholz_test)


# Early stopping = 3
# > results
# # A tibble: 7 x 6
# class             predictions             perf         mmce   auc kappa
# <chr>             <list>                  <list>      <dbl> <dbl> <dbl>
#         1 spruce            <S3: PredictionClassif> <dbl [6]> 0.0913  0.970 0.802
# 2 lodgepole_pine    <S3: PredictionClassif> <dbl [6]> 0.0880  0.973 0.824
# 3 aspen             <S3: PredictionClassif> <dbl [6]> 0.00519 0.998 0.822
# 4 douglasfir        <S3: PredictionClassif> <dbl [6]> 0.00568 0.999 0.900
# 5 ponderosa_pine    <S3: PredictionClassif> <dbl [6]> 0.00582 0.999 0.949
# 6 cottonwood_Willow <S3: PredictionClassif> <dbl [6]> 0.00111 0.999 0.878
# 7 krummholz         <S3: PredictionClassif> <dbl [6]> 0.00305 1.000 0.955
# >


combined_results <- as_tibble(matrix(ncol = 7, nrow = pred__spruce_test$data %>% nrow))
for (i in 1:7) {
        combined_results[,i] = results$pred_prob[[i]]
}
names(combined_results) <- results$class
combined_results

combined_results <- combined_results %>%
        mutate(max_pred = colnames(combined_results)[apply(combined_results,1,which.max)])

combined_results$max_pred = factor(combined_results$max_pred,
                                   levels = levels(df_xgbpkg_prep_test$cover_type))

caret::confusionMatrix(combined_results$max_pred, df_xgbpkg_prep_test$cover_type)
# Confusion Matrix and Statistics
#
# Reference
# Prediction          spruce_fir lodgepole_pine ponderosa_pine cottonwood_Willow aspen douglasfir krummholz
# spruce_fir             57501           3837              0                 0    25          4       310
# lodgepole_pine          5864          80732            225                 1   610        195        33
# ponderosa_pine             1            129          10195                62    23        270         0
# cottonwood_Willow          0              0             42               740     0         26         0
# aspen                     29            138              7                 0  2176          5         1
# douglasfir                10            136            257                21    12       4710         0
# krummholz                147             18              0                 0     1          0      5809
#
# Overall Statistics
#
# Accuracy : 0.9286
# 95% CI : (0.9274, 0.9298)
# No Information Rate : 0.4876
# P-Value [Acc > NIR] : < 2.2e-16
#
# Kappa : 0.8848
# Mcnemar's Test P-Value : NA
