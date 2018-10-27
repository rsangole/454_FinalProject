library(car)
library(tidyverse)
library(lattice)
library(latticeExtra)
library(caret)
library(ggthemr)
library(janitor)
library(gridExtra)
library(tidyr)
library(xgboost)
library(xgboostExplainer)

ggthemr("fresh")
options(max.print = 1000)


load("cache/raw_train.rdata")
glimpse(raw_train)

df_xg <- raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins() %>%
    # transform_wilderness_to_factor() %>%
    # transform_soil_to_factor() %>%
    make_response_var_the_first_var()

glimpse(df_xg)

model_mat <- model.matrix(cover_type~., df_xg)[,-1]
model_mat <- xgboost::xgb.DMatrix(model_mat)

xgboost(data = model_mat,
        label = 'cover_type',
        verbose = 2)
