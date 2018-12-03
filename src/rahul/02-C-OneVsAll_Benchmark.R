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
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
                    watchlist = make_watchlist(df_spruce, df_spruce_test)))
fit_spruce <- train(lrn, tsk_spruce)

df_lodgepole_pine <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("lodgepole_pine")
df_lodgepole_pine_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("lodgepole_pine")
(tsk_lodgepole_pine <- makeClassifTask(data = df_lodgepole_pine, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
                    watchlist = make_watchlist(df_lodgepole_pine, df_lodgepole_pine_test)))
fit_lodgepole_pine <- train(lrn, tsk_lodgepole_pine)

df_ponderosa_pine <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("ponderosa_pine")
df_ponderosa_pine_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("ponderosa_pine")
(tsk_ponderosa_pine <- makeClassifTask(data = df_ponderosa_pine, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
                    watchlist = make_watchlist(df_ponderosa_pine, df_ponderosa_pine_test)))
fit_ponderosa_pine <- train(lrn, tsk_ponderosa_pine)

df_cottonwood_Willow <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("cottonwood_Willow")
df_cottonwood_Willow_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("cottonwood_Willow")
(tsk_cottonwood_Willow <- makeClassifTask(data = df_cottonwood_Willow, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
                    watchlist = make_watchlist(df_cottonwood_Willow, df_cottonwood_Willow_test)))
fit_cottonwood_Willow <- train(lrn, tsk_cottonwood_Willow)

df_aspen <- df_xgbpkg_prep_train %>% make_response_var_one_vs_all("aspen")
df_aspen_test <- df_xgbpkg_prep_test %>% make_response_var_one_vs_all("aspen")
(tsk_aspen <- makeClassifTask(data = df_aspen, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
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
                    verbose = 1, early_stopping_rounds = 3, nrounds = 300L,
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
                "spruce",
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
                kappa = map_dbl(perf, ~.x[["kappa"]])
        ) %>%
        arrange(auc,kappa,mmce)
results %>%
        ggplot(aes(y=kappa,x=auc))+
        geom_point(aes(color=class))

save(results, file = "src/rahul/cache/02_c_results.RData")

#Result --> multiclass does better than this 1vall
