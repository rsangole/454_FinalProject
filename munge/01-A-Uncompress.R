library(tidyverse)
library(glue)
library(R.utils)
library(janitor)
library(caret)

gunzip("data/covtype.data.gz")

raw_data <- read_csv("data/covtype.data", col_names = F)

raw_data

col_names <- c(
  "elevation",
  "aspect",
  "slope",
  "horizontal_distance_to_hydrology",
  "vertical_distance_to_hydrology",
  "horizontal_distance_to_roadways",
  "hillshade_9am",
  "hillshade_noon",
  "hillshade_3pm",
  "horizontal_distance_to_fire_points",
  glue::glue("wilderness_area{x}", x = 1:4),
  glue::glue("soil_type{x}", x = 1:40),
  "cover_type"
)

names(raw_data) <- col_names

raw_data$cover_type <- factor(
  x = raw_data$cover_type,
  levels = 1:7,
  labels = c(
    "spruce_fir",
    "lodgepole_pine",
    "ponderosa_pine",
    "cottonwood_Willow",
    "aspen",
    "douglasfir",
    "krummholz"
  )
)

raw_data


# Code Designations
# Wilderness Areas:
# 1 -- Rawah Wilderness Area
# 2 -- Neota Wilderness Area
# 3 -- Comanche Peak Wilderness Area
# 4 -- Cache la Poudre Wilderness Area
#
# Soil Types:
# 1 to 40 : based on the USFS Ecological Landtype Units for this study area.
#
# Forest Cover Types:
# 1 -- Spruce/Fir
# 2 -- Lodgepole Pine
# 3 -- Ponderosa Pine
# 4 -- Cottonwood/Willow
# 5 -- Aspen
# 6 -- Douglas-fir
# 7 -- Krummholz
#
# Class Distribution
# Number of records of Spruce-Fir: 	211840
# Number of records of Lodgepole Pine: 	283301
# Number of records of Ponderosa Pine: 	 35754
# Number of records of Cottonwood/Willow:   2747
# Number of records of Aspen: 		  9493
# Number of records of Douglas-fir: 	 17367
# Number of records of Krummholz: 	 20510
# Number of records of other: 		     0
#
# Total records:				581012
