library(tidyverse)
library(glue)
library(janitor)
library(caret)
library(lattice)
library(skimr)

load('cache/raw_train.rdata')

raw_train
str(raw_train)

skimr::skim(raw_train)
