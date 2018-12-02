library(car)
library(tidyverse)
library(lattice)
library(latticeExtra)
library(ggthemr)
library(janitor)
library(gridExtra)
library(tidyr)
library(xgboost)
library(xgboostExplainer)
library(mlr)
# library(caret)

ggthemr("fresh")
options(max.print = 1000)

source("munge/01-C-FeatureEngg.R")

load("cache/raw_train.rdata")
load("cache/raw_test.rdata")
glimpse(raw_train)

df_D1 <- raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins() %>%
    convert_factors_dummies() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var()
df_D1_test <- raw_test %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins() %>%
    convert_factors_dummies() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var()
# glimpse(df_D1)

df_D2 <- raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins(cut_labels = F) %>%
    # transform_wilderness_to_factor() %>%
    # transform_soil_to_factor() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var()
# glimpse(df_D2)
df_D2_test <- raw_test %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins(cut_labels = F) %>%
    # transform_wilderness_to_factor() %>%
    # transform_soil_to_factor() %>%
    make_all_responses_numeric_datatype() %>%
    make_response_var_the_first_var()

df_D3 <- raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins(cut_labels = F) %>%
    transform_wilderness_to_factor() %>%
    transform_soil_to_factor() %>%
    make_response_var_the_first_var()
# glimpse(df_D3)
df_D3_test <- raw_test %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins(cut_labels = F) %>%
    transform_wilderness_to_factor() %>%
    transform_soil_to_factor() %>%
    make_response_var_the_first_var()

# df_D3 %>%
#     make_response_var_one_vs_all("spruce_fir") %>%
#     convert_factor_vars_to_WOE() %>%
#     glimpse()
