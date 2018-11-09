# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_m1 <- df_xg %>%
  make_response_var_one_vs_all("spruce_fir")
glimpse(df_m1)

(tsk <- makeClassifTask(data = df_m1, target = "cover_type"))
(lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg"))
(rdesc <- makeResampleDesc(method = "CV", iters = 3))
ps = makeParamSet(
    makeNumericParam("nrounds", lower = 20L, upper = 80L),
    makeDiscreteParam("max_depth", values = c(2,4,6))
)
ctrl = makeTuneControlRandom(maxit = 10L)

# (res <- resample(learner = lrn,
#                  task = tsk,
#                  # resampling = rdesc,
#                  measures = acc,
#                  show.info = T))

res = tuneParams(learner = lrn,
                 task = tsk,
                 resampling = rdesc,
                 par.set = ps,
                 control = ctrl)
