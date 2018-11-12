# Common definitions
parallelMap::parallelStart(mode = "multicore", cpus = 4)
meas_binary <- list(mmce, f1, kappa, fpr, fnr, auc)
(rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE))
(ctrl = makeTuneControlGrid())

################################################
#                  DATASET D1                  #
################################################

# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_D1_m1 <- df_D1 %>%
  make_response_var_one_vs_all("spruce_fir")
glimpse(df_D1_m1)

df_D1_m1 <- as.data.frame(df_D1_m1)
head(df_D1_m1)

(tsk <- makeClassifTask(data = df_D1_m1, target = "cover_type"))
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


#------------------------------------------------------------------------

# Model #2
# First test for one-vs-all for `lodgepole_pine` vs all

df_m2 <- df_xg %>%
    make_response_var_one_vs_all("lodgepole_pine")
glimpse(df_m2)

df_m2 <- as.data.frame(df_m2)
head(df_m2)

(tsk2 <- makeClassifTask(data = df_m2, target = "cover_type"))
(lrn2 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", verbose = 1))
(rdesc2 <- makeResampleDesc(method = "CV", iters = 3))
ps2 = makeParamSet(
    makeDiscreteParam("nrounds", values = c(10L, 30L, 60L)),
    makeDiscreteParam("max_depth", values = c(2,4,6))
)
(ctrl2 = makeTuneControlGrid())

parallelMap::parallelStart(mode = "multicore", cpus = 4)

(res2 = tuneParams(learner = lrn2,
                  task = tsk2,
                  resampling = rdesc2,
                  par.set = ps2,
                  control = ctrl2))

lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
                 res2$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res2

# Predict with the best tune
(lrn2 <- setHyperPars(lrn2, par.vals = res2$x))
(mod2 <- train(lrn2, tsk2))
pred2 = predict(mod2, task = tsk2)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred2, relative = T)
performance(pred2)

getLearnerModel(mod2) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)

#------------------------------------------------------------------------

# Model #3
# First test for one-vs-all for `ponderosa_pine` vs all

df_m3 <- df_xg %>%
    make_response_var_one_vs_all("ponderosa_pine")
glimpse(df_m3)

df_m3 <- as.data.frame(df_m3)
head(df_m3)

(tsk3 <- makeClassifTask(data = df_m3, target = "cover_type"))
(lrn3 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", verbose = 0))
(rdesc3 <- makeResampleDesc(method = "CV", iters = 3))
ps3 = makeParamSet(
    makeDiscreteParam("nrounds", values = c(60L,100L)),
    makeDiscreteParam("max_depth", values = c(6,10))
)
(ctrl3 = makeTuneControlGrid())

parallelMap::parallelStart(mode = "multicore", cpus = 4)

(res3 = tuneParams(learner = lrn3,
                   task = tsk3,
                   resampling = rdesc3,
                   par.set = ps3,
                   control = ctrl3))

lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
                 res3$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res3

# Predict with the best tune
(lrn3 <- setHyperPars(lrn3, par.vals = res3$x))
(mod3 <- train(lrn3, tsk3))
pred3 = predict(mod3, task = tsk3)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred3, relative = T)
performance(pred3)

getLearnerModel(mod3) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
    labs(title = 'Feature Importance - ponderosa_pine')


#------------------------------------------------------------------------

# Model #4
# First test for one-vs-all for `krummholz` vs all

df_m4 <- df_xg %>%
    make_response_var_one_vs_all("krummholz")
glimpse(df_m4)

df_m4 <- as.data.frame(df_m4)
head(df_m4)

(tsk4 <- makeClassifTask(data = df_m4, target = "cover_type"))
(lrn4 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", verbose = 0))
(rdesc4 <- makeResampleDesc(method = "CV", iters = 3))
ps4 = makeParamSet(
    makeDiscreteParam("nrounds", values = c(100L)),
    makeDiscreteParam("max_depth", values = c(10, 15))
)
(ctrl4 = makeTuneControlGrid())

parallelMap::parallelStart(mode = "multicore", cpus = 4)

(res4 = tuneParams(learner = lrn4,
                   task = tsk4,
                   resampling = rdesc4,
                   par.set = ps4,
                   control = ctrl4))

lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
                 res4$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res4

# Predict with the best tune
(lrn4 <- setHyperPars(lrn4, par.vals = res4$x))
(mod4 <- train(lrn4, tsk4))
pred4 = predict(mod4, task = tsk4)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred4, relative = T)
performance(pred4)

getLearnerModel(mod4) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
    labs(title = 'Feature Importance - krummholz')


#------------------------------------------------------------------------

# Model #5
# First test for one-vs-all for `douglasfir` vs all

df_m5 <- df_xg %>%
    make_response_var_one_vs_all("douglasfir")
glimpse(df_m5)

df_m5 <- as.data.frame(df_m5)
head(df_m5)

(tsk5 <- makeClassifTask(data = df_m5, target = "cover_type"))
(lrn5 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", verbose = 0))
(rdesc5 <- makeResampleDesc(method = "CV", iters = 3))
ps5 = makeParamSet(
    makeDiscreteParam("nrounds", values = c(100L)),
    makeDiscreteParam("max_depth", values = c(10, 15))
)
(ctrl5 = makeTuneControlGrid())

parallelMap::parallelStart(mode = "multicore", cpus = 4)

(res5 = tuneParams(learner = lrn5,
                   task = tsk5,
                   resampling = rdesc5,
                   par.set = ps5,
                   control = ctrl5))

lattice::dotplot(mmce.test.mean~max_depth, groups=nrounds,
                 res5$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res5

# Predict with the best tune
(lrn5 <- setHyperPars(lrn5, par.vals = res5$x))
(mod5 <- train(lrn5, tsk5))
pred5 = predict(mod5, task = tsk5)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred5, relative = T)
performance(pred5)

getLearnerModel(mod5) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
    labs(title = 'Feature Importance - douglasfir')

save(res, file = "cache/rahul/res.Rdata")
save(res3, file = "cache/rahul/res3.Rdata")
save(res4, file = "cache/rahul/res4.Rdata")
save(res5, file = "cache/rahul/res5.Rdata")
