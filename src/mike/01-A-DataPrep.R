library(car)
library(tidyverse)
library(lattice)
library(latticeExtra)
library(caret)
library(ggthemr)
library(janitor)
library(gridExtra)
library(tidyr)
ggthemr("fresh")
options(max.print = 1000)


load("cache/raw_train.rdata")
glimpse(raw_train)

raw_train %>%
    add_boxbox_transform() %>%
    add_distances_features() %>%
    transform_continuous_to_bins() %>%
    transform_wilderness_to_factor() %>%
    transform_soil_to_factor() %>%
    make_response_var_the_first_var() %>%
    glimpse()
