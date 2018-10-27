library(car)
library(tidyverse)
library(lattice)
library(latticeExtra)
library(caret)


load('cache/raw_train.rdata')
glimpse(raw_train)

# Near Zero Variance Analysis
nzv_analysis <- caret::nearZeroVar(raw_train[-55],saveMetrics = T, allowParallel = T)
nzv_analysis
cat("Potential variables to remove \n\n", names(raw_train)[nzv_analysis$nzv], "\n\n which have near zero variance")


# BoxCox Transformations to Normalize Variables
raw_train[c("elevation","aspect","slope","horizontal_distance_to_hydrology","vertical_distance_to_hydrology","horizontal_distance_to_roadways","hillshade_9am","hillshade_noon","hillshade_3pm","horizontal_distance_to_fire_points","wilderness_area1","wilderness_area2","wilderness_area3")] %>%
    purrr::map_dbl(~caret::BoxCoxTrans(.x)$lambda)

## Only elevation has an estimation of lambda = 2
add_boxbox_transform <- function(df){
    df %>%
        mutate(elevation_boxcox = elevation^2) %>%
        dplyr::select(-elevation)
}


# Binning continuous variables ----





# Factor variables to single column ----





# Distances




# Ratios & Differences



# Transforms




# WOE




# Information Value




# PCA
pca_fit <- prcomp(x = raw_train[1:14], center = T, scale. = T)
pca_fit
pca_fit %>% summary
screeplot(pca_fit,type = 'l', npcs = 14)
## Potentially keep ~8 PCs for all variables inserted in the PCA model (except Soil Type)
## ~90% of variation explained

add_pca_transform <- function(df){
    pca_df <- as_tibble(predict(pca_fit, raw_train[1:14])[,1:8])
    df[,1:14] <- NULL
    pca_df %>% bind_cols(df)
}

# Fully standardized training set
