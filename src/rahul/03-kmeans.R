df_soilclust <- df_xgbpkg_prep_train %>%
        select(contains("soil"),contains("wild"))

# Common definitions
#parallelMap::parallelStart(mode = "multicore")
lrn <- makeLearner("cluster.kmeans")
tsk <- makeClusterTask(data = df_xgbpkg_prep_train[,-1])
(rdesc <- makeResampleDesc(method = "CV", iters = 5))
ps <- makeParamSet(
        makeDiscreteParam("centers", values = 2:8)
)
ctrl = makeTuneControlGrid()
res = tuneParams(learner = lrn, task = tsk, resampling = rdesc,
                 par.set = ps, control = ctrl)
res
getTuneResultOptPath(res) %>% ggplot(aes(centers,db.test.mean))+geom_point()

res$x

lrn <- setHyperPars(lrn, par.vals = res$x)
lrn
m <- train(lrn, tsk)
m

predict(m, tsk)

generateHyperParsEffectData(res) %>%
        plotHyperParsEffect(y="db.test.mean",x="centers",plot.type = "line")
