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

#---- H0: Binning makes things better
tsks <- list(
    makeClassifTask(id = "base", data = df_hyp1_base_train, target = "cover_type"),
    makeClassifTask(id = "h1",   data = df_hyp1_train, target = "cover_type")
    )
(lrn <- makeLearner(cl = "classif.xgboost", id = "model1_xbg", predict.type = "prob",
                    verbose = 1,
                    early_stopping_rounds = 20,
                    nrounds = 50L,
                    eta = 0.3,
                    max_depth = 10))
bmr_h1 = benchmark(learners = lrn, tasks = tsks, resamplings = rdesc, measures = meas_multi, show.info = T)
