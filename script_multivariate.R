# 主成分分析・多変量解析
# 2023 09 29
# Greg Nishihara
 
# パッケージの読み込み
library(tidyverse)

# つぎのパッケージをインストールしてください
library(vegan)
library(ade4)

data(doubs) # 今日解析するデータ

# 環境要因のデータ
doubs$env

# 魚類の行列（データ）
doubs$fish
doubs$species

# 空間の情報
doubs$xy

doubs$xy |> as_tibble() |> 
  ggplot() + 
  geom_point(
    aes(
      x = x,
      y = y
    )
  )

# 環境データの解析

env = doubs$env |> as_tibble()
xy = doubs$xy |> as_tibble()

env_xy = bind_cols(xy, env)
env_xy = env_xy |> 
  mutate(station = 1:n())














