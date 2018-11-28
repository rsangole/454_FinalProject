# Multiclass
#
# * H0: Scaling makes things better: D3 vs D4
# * H0: Adding PCA makes things better: D3/D4 vs (D3/D4)+preProcWrapper
# * H0:
set.seed(42)

# Common definitions
library("parallelMap")
parallelStartSocket(63)
(tsk <- makeClassifTask(id = "base", data = df_xgbpkg_prep_train, target = "cover_type"))
(ctrl = makeTuneControlGrid())
(rdesc <- makeResampleDesc(method = "CV", iters=5, stratify = TRUE))
(meas_multi <- list(mmce, kappa))
(ps = makeParamSet(
    makeDiscreteParam("eta", values = c(0.5,1.0)),
    makeDiscreteParam("max_depth", values = c(3,8,14)),
    makeDiscreteParam("nrounds", values = c(50, 100, 150))
    ))
(lrn <- makeLearner(cl = "classif.xgboost",
                     id = "gridsearch", verbose = 1))
# (lrn1 <- makeLearner(cl = "classif.xgboost",
#                      id = "gridsearch_1",
#                     eta = 1.0,
#                     max_depth = 3,
#                     nrounds = 50))
# (lrn2 <- makeLearner(cl = "classif.xgboost",
#                      id = "gridsearch_2",
#                     eta = 1.0,
#                     max_depth = 3,
#                     nrounds = 100))

start_time <- Sys.time()
#tune_result_0001
(res = tuneParams(learner = lrn,
                  task = tsk,
                  resampling = rdesc,
                  par.set = ps,
                  control = ctrl,
                  show.info = T,
                  measures = meas_multi))
end_time <- Sys.time()
end_time-start_time # 7 hours
save(res, file = "src/rahul/tune_results_0001.RData")

# > res$opt.path$env$path
# eta max_depth nrounds mmce.test.mean kappa.test.mean
# 1  0.5         3      50     0.22532273       0.6317543
# 2    1         3      50     0.21327232       0.6535066
# 3  0.5         8      50     0.08989453       0.8549865
# 4    1         8      50     0.06296379       0.8986320
# 5  0.5        14      50     0.03708048       0.9403448
# 6    1        14      50     0.03797054       0.9389371
# 7  0.5         3     100     0.19818791       0.6775936
# 8    1         3     100     0.18544413       0.6995117
# 9  0.5         8     100     0.06038456       0.9027517
# 10   1         8     100     0.04853089       0.9219223
# 11 0.5        14     100     0.03322515       0.9465675
# 12   1        14     100     0.03561997       0.9427271
# 13 0.5         3     150     0.18196751       0.7047507
# 14   1         3     150     0.17065226       0.7238248
# 15 0.5         8     150     0.04932507       0.9206132
# 16   1         8     150     0.04348061       0.9300640
# 17 0.5        14     150     0.03236950       0.9479515
# 18   1        14     150     0.03492661       0.9438462


tune_results <- res$opt.path$env$path
tune_results$eta <- as.factor(tune_results$eta)
tune_results$max_depth <- as.factor(tune_results$max_depth)
tune_results$nrounds <- as.numeric(tune_results$nrounds)
tune_results %>%
        ggplot()+
        geom_point(aes(x=nrounds,y=mmce.test.mean,color=eta))+
        facet_wrap(~max_depth)

# # tsk2 <- makeClassifTask(id = "h1",   data = df_hyp1_train, target = "cover_type")
# (lrn1 <- makeLearner(cl = "classif.xgboost",
#                      id = "gridsearch",
#                      predict.type = "prob",
#                      verbose = 1,
#                      early_stopping_rounds = 20,
#                      nrounds = 50L,
#                      eta = 0.3,
#                      max_depth = 10))

# (lrn2 <- makePreprocWrapperCaret(learner = "classif.xgboost",
#                                  id = "xgb_pca",
#                                  predict.type = "prob",
#                                  verbose = 1,
#                                  early_stopping_rounds = 20,
#                                  nrounds = 50L,
#                                  eta = 0.3,
#                                  max_depth = 10,
#                                  ppc.pca = TRUE,
#                                  ppc.thresh = 0.8))
#
bmr_h1 = benchmark(learners = list(lrn1, lrn2),
                   tasks = list(tsk1, tsk2),
                   resamplings = rdesc, measures = meas_multi, show.info = T)

# Mapping in parallel: mode = multicore; cpus = 32; elements = 4.
# > bmr_h1
#   task.id              learner.id mmce.test.mean kappa.test.mean
# 1    base              model1_xbg     0.08475078       0.8631463
# 2    base classif.xgboost.preproc     0.31232825       0.4829163
# 3      h1              model1_xbg     0.11203070       0.8187550
# 4      h1 classif.xgboost.preproc     0.31231836       0.4851333


#---- 3* H0: Scaling makes things better
# (lrn3 <- makePreprocWrapperCaret(learner = "classif.xgboost",
#                                  id = "xgb_scale",
#                                  predict.type = "prob",
#                                  verbose = 1,
#                                  early_stopping_rounds = 20,
#                                  nrounds = 50L,
#                                  eta = 0.3,
#                                  max_depth = 10,
#                                  ppc.center = TRUE,
#                                  ppc.scale = TRUE))
# bmr_h3 = benchmark(learners = lrn3, tasks = tsk1, resamplings = rdesc,
#                    measures = meas_multi, show.info = T)
#
# bstFit <- train(lrn1, tsk1)
# bstFit
#
# getLearnerModel(bstFit,more.unwrap = T)
# bstFit$learner.model$evaluation_log %>% plot
