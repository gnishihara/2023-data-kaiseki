# 一般化加法モデル
# Generalized Additive Models (GAM)
# 2023 09 28
# Greg Nishihara

library(tidyverse)
library(ggpubr)
library(mgcv)
library(lemon)
library(statmod)

dset = CO2 |> as_tibble() 
dset
