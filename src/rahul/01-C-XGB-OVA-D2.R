# Common definitions
parallelMap::parallelStart(mode = "multicore", cpus = 16)
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
df_D2_test_m1 <- df_D2_test %>%
    make_response_var_one_vs_all("spruce_fir")

df_D2_m1 <- as.data.frame(df_D2_m1)

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

lattice::xyplot(auc.test.mean~as.numeric(max_depth), groups=eta,
                 res$opt.path$env$path,
                 auto.key = TRUE, type = "b")

message("Best tune result is: ")
res

# Predict with the best tune
(lrn <- setHyperPars(lrn, par.vals = res$x))
(mod <- train(lrn, tsk))
pred = predict(mod, newdata = df_D2_test_m1)
# head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred, relative = T)
performance(pred, measures = meas_binary)

getLearnerModel(mod) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)+
    labs(title = 'Feature Importance - spruce_fir')

# > pred = predict(mod, newdata = df_D2_test_m1)
# Warning in predict.WrappedModel(mod, newdata = df_D2_test_m1) :
#     Provided data for prediction is not a pure data.frame but from class tbl_df, hence it will be converted.
# > calculateConfusionMatrix(pred, relative = T)
# Relative confusion matrix (normalized by row/column):
#     predicted
# true         spruce_fir other     -err.-
#     spruce_fir 0.93/0.95  0.07/0.04 0.07
# other      0.03/0.05  0.97/0.96 0.03
# -err.-          0.05       0.04 0.04
#
#
# Absolute confusion matrix:
#     predicted
# true         spruce_fir  other -err.-
#     spruce_fir      59367   4185   4185
# other            3209 107541   3209
# -err.-           3209   4185   7394
#
# > res
# Tune result:
#     Op. pars: eta=0.5; max_depth=15
# mmce.test.mean=0.0443068,f1.test.mean=0.9388184,kappa.test.mean=0.9040929,fpr.test.mean=0.0309184,fnr.test.mean=0.0676387,auc.test.mean=0.9915260
# > performance(pred, measures = meas_binary)
# mmce         f1      kappa        fpr        fnr        auc
# 0.04242063 0.94137701 0.90814509 0.02897517 0.06585159 0.99227397
