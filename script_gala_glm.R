# ガラパゴス諸島の種数解析
# 一般化線形モデル
# 2023 09 28
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(ggpubr)
library(lemon)
library(emmeans)

# alr4 パッケージの galapagos データ
data(galapagos, package = "alr4")

gala = galapagos |> 
  as_tibble(rownames = "island")



