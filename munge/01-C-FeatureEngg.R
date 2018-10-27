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

# Near Zero Variance Analysis
nzv_analysis <- caret::nearZeroVar(raw_train[-55], saveMetrics = T,
                                   allowParallel = T)
nzv_analysis
cat("Potential variables to remove \n\n", names(raw_train)[nzv_analysis$nzv], "\n\n which have near zero variance")
remove_nzv_variables <- function(df) {
  message("... Removing NZV variables: ",
          paste0(names(df)[nzv_analysis$nzv], sep = ", "))
  df %>%
    dplyr::select(names(df)[!nzv_analysis$nzv])
}

# BoxCox Transformations to Normalize Variables
raw_train[c("elevation", "aspect", "slope", "horizontal_distance_to_hydrology", "vertical_distance_to_hydrology", "horizontal_distance_to_roadways", "hillshade_9am", "hillshade_noon", "hillshade_3pm", "horizontal_distance_to_fire_points", "wilderness_area1", "wilderness_area2", "wilderness_area3")] %>%
  purrr::map_dbl(~caret::BoxCoxTrans(.x)$lambda)

## Only elevation has an estimation of lambda = 2
add_boxbox_transform <- function(df) {
  message("... Box-Cox transforming `elevation` variable")
  df %>%
    mutate(elevation_boxcox = elevation^2) %>%
    dplyr::select(-elevation)
}


# Binning continuous variables ----
make_and_print_bins <- function(x, breaks = 10) {
  x_cut <- cut(x, breaks = breaks)
  print(x_cut %>% tabyl() %>% adorn_pct_formatting())
  grid.arrange(histogram(~x),
    histogram(~x_cut,
      scales = list(x = list(rot = 45))
    ),
    ncol = 2
  )
}
make_and_print_bins(raw_train$elevation)
# make_and_print_bins(raw_train$aspect)
# make_and_print_bins(raw_train$slope)
# make_and_print_bins(raw_train$horizontal_distance_to_hydrology)
# make_and_print_bins(raw_train$vertical_distance_to_hydrology)
# make_and_print_bins(raw_train$horizontal_distance_to_roadways)
# make_and_print_bins(raw_train$hillshade_9am)
# make_and_print_bins(raw_train$hillshade_noon)
# make_and_print_bins(raw_train$hillshade_3pm)
# make_and_print_bins(raw_train$horizontal_distance_to_fire_points)

transform_continuous_to_bins <- function(df, vars = NULL, breaks = 10) {
  message("... Transforming conti vars to binned factor vars, using breaks = ", breaks)
  if (is.null(vars)) {
    vars <- c(
      "elevation",
      "aspect",
      "slope",
      "horizontal_distance_to_hydrology",
      "vertical_distance_to_hydrology",
      "horizontal_distance_to_roadways",
      "hillshade_9am",
      "hillshade_noon",
      "hillshade_3pm",
      "horizontal_distance_to_fire_points"
    )
  }
  split_mask <- names(df) %in% vars
  df[split_mask] %>%
    purrr::map_df(~cut(.x, breaks = breaks)) %>%
    bind_cols(df[!split_mask])
}

# Factor variables to single column ----
wilderness_areas <-
  tbl_df(data.frame(
    wa = stringr::str_c("wilderness_area", 1:4),
    wilderness_area = c(
      "Rawah Wilderness Area",
      "Neota Wilderness Area",
      "Comanche Peak Wilderness Area",
      "Cache la Poudre Wilderness Area"
    ),
    stringsAsFactors = FALSE
  ))
soil_types <-
  tbl_df(data.frame(
    st = stringr::str_c("soil_type", 1:40),
    soil_type = c(
      "Cathedral family -Rock outcrop complex, extremely stony",
      "Vanet -Ratake families complex, very stony",
      "Haploborolis -Rock outcrop complex, rubbly",
      "Ratake family -Rock outcrop complex, rubbly",
      "Vanet family -Rock outcrop complex complex, rubbly",
      "Vanet -Wetmore families -Rock outcrop complex, stony",
      "Gothic family",
      "Supervisor -Limber families complex",
      "Troutville family, very stony",
      "Bullwark -Catamount families -Rock outcrop complex, rubbly",
      "Bullwark -Catamount families -Rock land complex, rubbly.",
      "Legault family -Rock land complex, stony",
      "Catamount family -Rock land -Bullwark family complex, rubbly",
      "Pachic Argiborolis -Aquolis complex",
      "unspecified in the USFS Soil and ELU Survey",
      "Cryaquolis -Cryoborolis complex",
      "Gateview family -Cryaquolis complex",
      "Rogert family, very stony",
      "Typic Cryaquolis -Borohemists complex",
      "Typic Cryaquepts -Typic Cryaquolls complex",
      "Typic Cryaquolls -Leighcan family, till substratum complex",
      "Leighcan family, till substratum, extremely bouldery",
      "Leighcan family, till substratum -Typic Cryaquolls complex",
      "Leighcan family, extremely stony",
      "Leighcan family, warm, extremely stony",
      "Granile -Catamount families complex, very stony",
      "Leighcan family, warm -Rock outcrop complex, extremely stony",
      "Leighcan family -Rock outcrop complex, extremely stony",
      "Como -Legault families complex, extremely stony",
      "Como family -Rock land -Legault family complex, extremely stony",
      "Leighcan -Catamount families complex, extremely stony",
      "Catamount family -Rock outcrop -Leighcan family complex, extremely stony",
      "Leighcan -Catamount families -Rock outcrop complex, extremely stony",
      "Cryorthents -Rock land complex, extremely stony",
      "Cryumbrepts -Rock outcrop -Cryaquepts complex",
      "Bross family -Rock land -Cryumbrepts complex, extremely stony",
      "Rock outcrop -Cryumbrepts -Cryorthents complex, extremely stony",
      "Leighcan -Moran families -Cryaquolls complex, extremely stony",
      "Moran family -Cryorthents -Leighcan family complex, extremely stony",
      "Moran family -Cryorthents -Rock land complex, extremely stony"
    ),
    stringsAsFactors = FALSE
  ))

transform_wilderness_to_factor <- function(df) {
  message("... Transforming wilderness to factor")
  df %>%
    tidyr::gather(wa, wa_flag, wilderness_area1:wilderness_area4) %>%
    dplyr::filter(wa_flag != 0) %>%
    left_join(wilderness_areas, by = "wa") %>%
    dplyr::select(-wa, -wa_flag) %>%
    mutate(wilderness_area = as.factor(wilderness_area))
}

transform_soil_to_factor <- function(df) {
  message("... Transforming soil to factor")
  df %>%
    gather(key = st, value = st_flag, soil_type1:soil_type40) %>%
    dplyr::filter(st_flag != 0) %>%
    left_join(soil_types, by = "st") %>%
    dplyr::select(-st, -st_flag) %>%
    mutate(soil_type = as.factor(soil_type))
}

# Distances
add_distances_features <- function(df){
    df %>%
        mutate(st_line_dist_to_hydrology = sqrt(horizontal_distance_to_hydrology^2+vertical_distance_to_hydrology^2),
               neg_vert_dist = vertical_distance_to_hydrology<0,
               hdth_gt_1250 = horizontal_distance_to_hydrology>1250,
               hdth_lt_600 = horizontal_distance_to_hydrology<600,
               vdth_gt_500 = vertical_distance_to_hydrology>=500,
               vdth_btw_350_500 = vertical_distance_to_hydrology>350 & vertical_distance_to_hydrology<500,
               vdth_lt_350 = vertical_distance_to_hydrology <= 350,
               dist_ratio_sq = (horizontal_distance_to_hydrology/vertical_distance_to_hydrology)^2)
}

# PCA

## 1> PCA for all components except Soil Type & Wilderness Type
pca_fit <- prcomp(x = raw_train[1:10], center = T, scale. = T)
pca_fit
pca_fit %>% summary()
screeplot(pca_fit, type = "l", npcs = 10)
## Potentially keep ~6 PCs for all variables inserted in the PCA model (except Soil Type)
## ~89% of variation explained

add_pca_transform_continuous_vars <- function(df) {
  message("... All vars except Soil or Wild vars replaced by 6 PCA components")
  pca_df <- as_tibble(predict(pca_fit, df %>% select("elevation", "aspect", "slope", "horizontal_distance_to_hydrology", "vertical_distance_to_hydrology", "horizontal_distance_to_roadways", "hillshade_9am", "hillshade_noon", "hillshade_3pm", "horizontal_distance_to_fire_points"))[, 1:6])
  names(pca_df) <- paste0('conti_',names(pca_df))
  keep <- !(names(df) %in% c("elevation", "aspect", "slope", "horizontal_distance_to_hydrology", "vertical_distance_to_hydrology", "horizontal_distance_to_roadways", "hillshade_9am", "hillshade_noon", "hillshade_3pm", "horizontal_distance_to_fire_points"))
  pca_df %>% bind_cols(df[keep])
}

## 2> PCA for Soil Type & Wild Type
pca_fit2 <- prcomp(x = raw_train %>% dplyr::select(contains("wild"), contains("soil")), center = T, scale. = T)
pca_fit2
pca_fit2 %>% summary()
screeplot(pca_fit2, type = "l", npcs = 20)
## Doesn't seem to show any promise of variable reduction here
## Though there is a sharp knee in the scree plot at k=4

add_pca_transform_soil_wild <- function(df) {
  message("... Soil & Wild vars replaced by 4 PCA components")
  pca_df <- as_tibble(predict(pca_fit2, df %>% select(contains("wild"), contains("soil")))[, 1:4])
  names(pca_df) <- paste0('soil_',names(pca_df))
  keep <- !(names(df) %in% names(df %>% dplyr::select(contains("wild"), contains("soil"))))
  pca_df %>% bind_cols(df[keep])
}

## 3> PCA for HillShade Only
pca_fit3 <- prcomp(x = raw_train %>% dplyr::select(contains("hill")), center = T, scale. = T)
pca_fit3
pca_fit3 %>% summary()
screeplot(pca_fit3, type = "l", npcs = 3)

## First 2 components explain 99% of the variation

add_pca_transform_hillshade <- function(df) {
    message("... Hillshade vars replaced by 2 PCA components")
    pca_df <- as_tibble(predict(pca_fit3, df %>% dplyr::select(contains("hill")))[, 1:2])
    names(pca_df) <- paste0('hill_',names(pca_df))
    keep <- !(names(df) %in% names(df %>% dplyr::select(contains("hill"))))
    pca_df %>% bind_cols(df[keep])
}


# Fully standardized training set
center_scale_pp <- preProcess(raw_train,
                              method = c("center","scale"))



# Make response var the first variable (useful when modeling)

make_response_var_the_first_var <- function(df){
    message("... Making resp var the first var")
    df %>%
        select(cover_type,
               names(df))
}
