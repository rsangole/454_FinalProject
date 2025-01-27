# library(car)
library(tidyverse)
library(lattice)
library(latticeExtra)
# library(ggthemr)
library(janitor)
library(gridExtra)
library(tidyr)
library(xgboost)
# library(xgboostExplainer)
library(mlr)
# library(caret)

# ggthemr("fresh")
options(max.print = 1000)

source("munge/01-C-FeatureEngg.R")

load("cache/raw_train.rdata")
load("cache/raw_test.rdata")
glimpse(raw_train)

df_xgbpkg_prep_train <- raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var() %>%
    as.data.frame()
df_xgbpkg_prep_test <- raw_test %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var() %>%
    as.data.frame()

df_xgbpkg_prep_train %>% glimpse()
# df_hyp1_train <- as.data.frame(df_D2)
# df_hyp1_test  <- as.data.frame(df_D2_test)
