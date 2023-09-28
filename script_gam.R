# 一般化加法モデル
# Generalized Additive Models (GAM)
# 2023 09 28
# Greg Nishihara

library(tidyverse) # データ処理・作図用
library(ggpubr)    # 作図用
library(lemon)     # 作図用

library(gratia) # GAM用パッケージ
library(mgcv)   # GAM用パッケージ

library(statmod) # ランダム化残差用

dset = CO2 |> as_tibble() 
dset
