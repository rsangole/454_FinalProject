# Common definitions
parallelMap::parallelStart(mode = "multicore", cpus = 4)
meas_binary <- list(mmce, f1, kappa, fpr, fnr, auc)
(rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE))
(ctrl = makeTuneControlGrid())

################################################
#                  DATASET D2                  #
################################################

# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_D2_m1 <- df_D2 %>%
    make_response_var_one_vs_all("spruce_fir")
glimpse(df_D2_m1)

df_D2_m1 <- as.data.frame(df_D2_m1)
head(df_D2_m1)

(tsk <- makeClassifTask(data = df_D2_m1, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
                    verbose = 1, early_stopping_rounds = 20, nrounds = 50L))
(ps = makeParamSet(
    makeDiscreteParam("eta", values = c(0.1,0.3,0.5)),
    makeDiscreteParam("max_depth", values = c(5,10,15))
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
