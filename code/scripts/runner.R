library(readxl)
library(tidyverse)
library(raster)
library(sf)

source("code/scripts/001_load_data_v2.R")          #Done
source("code/scripts/002_transform_data.R")          #Done
#source("code/scripts/002_PCA.R")            #Done
#source("code/scripts/003_Keras_encoder.R")          #Done
#source("code/scripts/004_Julia_circuitscape.R")          #Done
source("code/scripts/005_Kruger_lmm.R")          #Done
source("code/scripts/006_SA_lmm.R")          #Done
source("code/scripts/007_increasing_threshold.R")          #Done
source("code/scripts/008_decreasing_threshold.R")          #Done