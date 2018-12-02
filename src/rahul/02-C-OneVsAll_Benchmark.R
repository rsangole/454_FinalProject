# Common definitions
parallelMap::parallelStart(mode = "multicore")
meas_binary <- list(mmce, f1, kappa, fpr, fnr, auc)
(rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE))
(ctrl = makeTuneControlGrid())

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




#
#
# (tsk <- makeClassifTask(data = df_xgbpkg_prep_train, target = "cover_type"))
# (lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
#                     verbose = 1, early_stopping_rounds = 20, nrounds = 50L))
# (ps = makeParamSet(
#         makeDiscreteParam("eta", values = c(0.1,0.3,0.5)),
#         makeDiscreteParam("max_depth", values = c(10,15,20))
# ))
# (res = tuneParams(learner = lrn,
#                   task = tsk,
#                   resampling = rdesc,
#                   par.set = ps,
#                   control = ctrl,
#                   show.info = T,
#                   measures = meas_binary))
#
#
# lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
#                  res$opt.path$env$path,
#                  auto.key = TRUE, type = "b")
#
# message("Best tune result is: ")
# res
#
# # Predict with the best tune
# (lrn <- setHyperPars(lrn, par.vals = res$x))
# (mod <- train(lrn, tsk))
# pred = predict(mod, task = tsk)
# # head(getPredictionProbabilities(pred))
#
# calculateConfusionMatrix(pred, relative = T)
# performance(pred)
#
# getLearnerModel(mod) %>%
#         xgb.importance(model = .) %>%
#         xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
#         labs(title = 'Feature Importance - spruce_fir')
#
