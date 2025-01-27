# Common definitions
parallelMap::parallelStart(mode = "multicore", cpus = 4)
meas_binary <- list(mmce, f1, kappa, fpr, fnr, auc)
(rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE))
(ctrl = makeTuneControlGrid())

################################################
#                  DATASET D3                  #
################################################

# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_D3_m1 <- df_D3 %>%
    make_response_var_one_vs_all("spruce_fir") %>%
    convert_factor_vars_to_WOE() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var()
glimpse(df_D3_m1)

df_D3_m1 <- as.data.frame(df_D3_m1)
head(df_D3_m1)


library(xgboost)
?xgb.cv
dtrain <- xgb.DMatrix(as.matrix(df_D3_m1[-1]), label = (as.integer(df_D3_m1[[1]])-1))
cv <- xgb.cv(data = dtrain,
             nrounds = 50,
             nfold = 10,
             metrics = list("rmse","auc"),
             max_depth = 15,
             eta = 0.4,
             objective = "binary:logistic")
print(cv)
cv20 <- xgb.cv(data = dtrain,
             nrounds = 50,
             nfold = 10,
             metrics = list("rmse","auc"),
             max_depth = 20,
             eta = 0.4,
             objective = "binary:logistic")
print(cv20)


(tsk <- makeClassifTask(data = df_D3_m1, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 20, nrounds = 50L))
(ps = makeParamSet(
    makeDiscreteParam("eta", values = c(0.1,0.3,0.5)),
    makeDiscreteParam("max_depth", values = c(10,15,20))
))
(res = tuneParams(learner = lrn,
                  task = tsk,
                  resampling = rdesc,
                  par.set = ps,
                  control = ctrl,
                  show.info = T,
                  measures = meas_binary))


lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
                 res$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res

# Predict with the best tune
(lrn <- setHyperPars(lrn, par.vals = res$x))
(mod <- train(lrn, tsk))
pred = predict(mod, task = tsk)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred, relative = T)
performance(pred)

getLearnerModel(mod) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
    labs(title = 'Feature Importance - spruce_fir')

