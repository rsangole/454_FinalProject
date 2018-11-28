# Multiclass
#
# * H0: Scaling makes things better: D3 vs D4
# * H0: Adding PCA makes things better: D3/D4 vs (D3/D4)+preProcWrapper
# * H0:
set.seed(42)

# Common definitions
parallelMap::parallelStart(mode = "multicore")
meas_multi <- list(mmce, kappa)
# (rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE))
(rdesc <- makeResampleDesc(method = "CV", iters=5, stratify = TRUE))

tsk1 <- makeClassifTask(id = "base", data = df_xgbpkg_prep_train, target = "cover_type")
# tsk2 <- makeClassifTask(id = "h1",   data = df_hyp1_train, target = "cover_type")
(lrn1 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
                     verbose = 1,
                     early_stopping_rounds = 20,
                     nrounds = 50L,
                     eta = 0.3,
                     max_depth = 10))
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
# bmr_h1 = benchmark(learners = list(lrn1, lrn2),
#                    tasks = list(tsk1, tsk2),
#                    resamplings = rdesc, measures = meas_multi, show.info = T)

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

bstFit <- train(lrn1, tsk1)
bstFit

getLearnerModel(bstFit,more.unwrap = T)
bstFit$learner.model$evaluation_log %>% plot
