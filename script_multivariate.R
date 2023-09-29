# 主成分分析・多変量解析
# 2023 09 29
# Greg Nishihara
 
# パッケージの読み込み
library(tidyverse)
library(patchwork)

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

env_xy

ggplot(env_xy) + 
  geom_col(
    aes(
      x = dfs,
      y = alt
    )
  )

ggplot(env_xy) + 
  geom_col(
    aes(
      x = dfs,
      y = har
    )
  )

ggplot(env_xy) + 
  geom_col(
    aes(
      x = dfs,
      y = pho
    )
  )

ggplot(env_xy) + 
  geom_col(
    aes(
      x = dfs,
      y = nit
    )
  )

#######

plot1 = ggplot(env_xy) +
  geom_point(
    aes(
      x = pho,
      y = nit
    )
  )

plot2 = ggplot(env_xy) +
  geom_point(
    aes(
      x = pho,
      y = amm
    )
  )

plot3 = ggplot(env_xy) +
  geom_point(
    aes(
      x = amm,
      y = nit
    )
  )

plot1 + plot2 + plot3

ggplot(env_xy) +
  geom_point(
    aes(
      x = pho,
      y = nit,
      color = amm
    )
  )

##############
# 主成分分析
# Principal Component Analysis (PCA)
#
X = env_xy |> select(pho, nit, amm) |> 
  as.matrix()
X

m1 = rda(X)
# PC1: 主成分1
# PC2: 主成分2
# PC3: 主成分3
# 主成分の数と変数の数は同じ
biplot(m1)

# PC2 と PC3 の図
biplot(m1, choices = c(2,3))



# 水質データの解析

X = env_xy |> 
  select(pH, har,
         pho, nit, amm,
         oxy, bdo) |> 
  as.matrix()

X
scale(X)

m1 = rda(X, scale = TRUE)

biplot(m1)

# PC2 と PC3 の図
biplot(m1, choices = c(2,3))

# 固有値
e1 = m1$CA$eig
e1 / sum(e1) * 100 # データのばらつきを説明する度合

# 主成分の値は次の方法でとる。
# PC1 = m1$CA$u[, 1] # 使わない

summary(m1)
PC1 = summary(m1)$site[,1]
env_xy |> 
  mutate(PC1 = PC1) |> 
  ggplot() + 
  geom_col(
    aes(
      x = dfs,
      y = PC1
    )
  )





























