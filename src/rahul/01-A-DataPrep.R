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
    convert_factors_dummies() %>%
    make_response_var_the_first_var() %>%
    purrr::map_df(~as.numeric(.x))

glimpse(df_xg)

model_mat <- xgboost::xgb.DMatrix(as.matrix(df_xg), label=df_xg$cover_type)

xgboost(data = model_mat,
        # label = df_xg$cover_type,
        verbose = 2,
        nrounds = 500)
