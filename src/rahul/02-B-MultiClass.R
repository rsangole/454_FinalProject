# Multiclass
#
# * H0: Scaling makes things better: D3 vs D4
# * H0: Adding PCA makes things better: D3/D4 vs (D3/D4)+preProcWrapper
# * H0:


# Common definitions
parallelMap::parallelStart(mode = "multicore", cpus = 16)
meas_multi <- list(mmce, kappa)
# (rdesc <- makeResampleDesc(method = "CV", iters = 10, stratify = TRUE))
(rdesc <- makeResampleDesc(method = "Holdout", stratify = TRUE))

tsk1 <- makeClassifTask(id = "base", data = df_hyp1_base_train, target = "cover_type")
tsk2 <- makeClassifTask(id = "h1",   data = df_hyp1_train, target = "cover_type")

#---- H0: Binning makes things better
tsks <- list(tsk1, tsk2)
(lrn1 <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
                    verbose = 1,
                    early_stopping_rounds = 20,
                    nrounds = 50L,
                    eta = 0.3,
                    max_depth = 10))
bmr_h1 = benchmark(learners = lrn, tasks = tsks, resamplings = rdesc, measures = meas_multi, show.info = T)



#---- 2* H0: PCA makes things better
(lrn2 <- makePreprocWrapperCaret(learner = "classif.xgboost",
                                id = "xgb_pca",
                                predict.type = "prob",
                                verbose = 1,
                                early_stopping_rounds = 20,
                                nrounds = 50L,
                                eta = 0.3,
                                max_depth = 10,
                                ppc.pca = TRUE,
                                ppc.thresh = 0.8))
bmr_h2 = benchmark(learners = list(lrn1, lrn2), tasks = tsk1, resamplings = rdesc,
                   measures = meas_multi, show.info = T)
