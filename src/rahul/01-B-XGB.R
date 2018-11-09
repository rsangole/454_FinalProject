# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_m1 <- df_xg %>%
  make_response_var_one_vs_all("spruce_fir")
glimpse(df_m1)

(tsk <- makeClassifTask(data = df_m1, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", nrounds=300, verbose = 1))
(rdesc <- makeResampleDesc(method = "CV", iters = 5))
# ps = makeParamSet(
#     makeDiscreteParam("nrounds", values = c(80L,120L,200L)),
#     makeDiscreteParam("max_depth", values = c(2,4,6))
# )
# ctrl = makeTuneControlRandom(maxit = 10L)
# ctrl = makeTuneControlGrid(resolution = 10)

# parallelMap::parallelStart(mode = "multicore", cpus = 8)

# (res <- resample(learner = lrn,
#                  task = tsk,
#                  # resampling = rdesc,
#                  measures = acc,
#                  show.info = T))

# (res = tuneParams(learner = lrn,
#                  task = tsk,
#                  resampling = rdesc,
#                  par.set = ps,
#                  control = ctrl))

(mod <- train(lrn, tsk))

pred = predict(mod, task = tsk)
head(getPredictionProbabilities(pred))

calculateConfusionMatrix(pred)
calculateConfusionMatrix(pred, relative = T)
performance(pred)

generateFilterValuesData(tsk)

getLearnerModel(mod) %>%
    xgb.importance(model = .) %>%
    xgb.ggplot.importance(top_n = 10, rel_to_first = T)
