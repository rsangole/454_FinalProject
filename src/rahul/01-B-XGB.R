library(mlbench)

# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_m1 <- df_xg %>%
  make_response_var_one_vs_all("spruce_fir")

glimpse(df_m1)

tsk <- makeClassifTask(data = df_m1, target = "cover_type")
tsk

ho <- makeResampleInstance("Holdout", tsk)
ho

tsk.train <- subsetTask(tsk, ho$train.inds[[1]])
tsk.test <- subsetTask(tsk, ho$test.inds[[1]])

lrn <- makeLearner("classif.xgboost", nrounds = 10)
cv <- makeResampleDesc("CV", iters = 5)
res <- resample(lrn, tsk.train, cv, acc)
