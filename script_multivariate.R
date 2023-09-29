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

##############################

# 冗長性分析
# Redundancey analysis (RDA)
# 

fish = doubs$fish |> as_tibble()
env

fishl = fish |> 
  mutate(station = 1:n()) |> 
  pivot_longer(cols = -station)

fishs = fishl |> 
  mutate(y = value > 0) |> 
  group_by(station) |> 
  summarise(species = sum(y))

ggplot(fishs) + 
  geom_col(
    aes(
      x = station,
      y = species
    )
  )

# 環境データと種数のデータを結合

env_fish = full_join(fishs, env_xy,
                     by = "station")

ggplot(env_fish) + 
  geom_col(
    aes(
      x = dfs,
      y = species
    )
  )

env_fish = env_fish |> 
  mutate(PC1 = PC1)

ggplot(env_fish) + 
  geom_point(
    aes(
      x = PC1,
      y = species
    )
  )

ggplot(env_fish) + 
  geom_point(
    aes(
      x = nit,
      y = species
    )
  )


env_fish

env_fish |> 
  mutate(Cogo = fish$Cogo) |> 
  ggplot() + 
  geom_col(
    aes(
      x = dfs,
      y = Cogo
    )
  )

env_fish |> 
  mutate(Cogo = fish$Cogo) |> 
  ggplot() + 
  geom_point(
    aes(
      x = PC1,
      y = Cogo
    )
  )


Y = fish |> as.matrix()
# へリンガ変換
Yhat = decostand(Y, method = "hellinger")
X = env_fish |> select(pho, nit, amm) |>
  as.matrix()

m1 = rda(Yhat ~ pho + nit + amm,
         data = env_fish,
         scale = TRUE)

# plot(x, choices = c(1, 2), display = c("sp", "wa", "cn"),
# scaling = "species", type, xlim, ylim, const,
# correlation = FALSE, hill = FALSE, ...)

# scaling = "species"
# 相関関係を見たいとき
plot(m1, 
     choice = c(1,2), 
     scaling = "species")

# scaling = "sites"
# 調査地点・種の類似度を見たいとき
plot(m1, 
     choice = c(1,2), 
     scaling = "sites")


m2 = rda(Yhat ~ pho + nit + amm + 
           pH + oxy + bdo + har,
         data = env_fish,
         scale = TRUE)
# 相関関係を見たいとき
plot(m2, 
     choice = c(1,2), 
     scaling = "species")

# scaling = "sites"
# 調査地点・種の類似度を見たいとき
plot(m2, 
     choice = c(1,2), 
     scaling = "sites")



m2 = rda(Yhat ~ pho + nit + amm + 
           pH + oxy + bdo + har,
         data = env_fish ),
         scale = TRUE)
# 相関関係を見たいとき
plot(m2, 
     choice = c(1,2), 
     scaling = "species")

# scaling = "sites"
# 調査地点・種の類似度を見たいとき
plot(m2, 
     choice = c(1,2), 
     scaling = "sites")


###########################
# PCA vs RDA
Yhat = decostand(fish, method = "hellinger")
model0 = rda(Yhat, scale = TRUE)
plot(model0) 
# Teso, Cogo, Thth, Phph, Neba, Satr

model1 = rda(Yhat ~ pho + nit,
             data = env_fish, 
             scale = TRUE)
plot(model1, scaling = "sites")

####################

irist = iris |> as_tibble()

ggplot(irist) +
  geom_point(
    aes(
      x = Petal.Length,
      y = Petal.Width,
      color = Species
    )
  )


ggplot(irist) +
  geom_point(
    aes(
      x = Sepal.Length,
      y = Sepal.Width,
      color = Species
    )
  )


ggplot(irist) + 
  geom_point(
    aes(
      x = Species,
      y = Petal.Length,
      color = Species),
    position = position_jitter(0.1)
  )

m1 = lm(Petal.Length ~ Species,
   data = irist)
anova(m1)

n1 = lm(Petal.Width ~ Species,
        data = irist)
anova(n1)


###############
Y = irist |> 
  select(Petal.Length, Petal.Width) |> 
  as.matrix()

m1 = rda(Y)
biplot(m1, scaling = "species")

PC1 = summary(m1)$sites[,1]

tmp = irist |> select(Species)
tmp = tmp |> mutate(PC1 = PC1)
ggplot(tmp)  +
  geom_point(
    aes(
      x = Species,
      y = PC1,
      color = Species
    ),
    position = position_jitter(0.1)
  )

m1 = lm(PC1 ~ Species, data = tmp)
anova(m1)
############
Y = irist |> 
  select(Sepal.Length, .Width) |> 
  as.matrix()

m1 = rda(Y)
biplot(m1, scaling = "species")

PC1 = summary(m1)$sites[,1]

tmp = irist |> select(Species)
tmp = tmp |> mutate(PC1 = PC1)
ggplot(tmp)  +
  geom_point(
    aes(
      x = Species,
      y = PC1,
      color = Species
    ),
    position = position_jitter(0.1)
  )

m1 = lm(PC1 ~ Species, data = tmp)
anova(m1)
#########
Y = irist |> 
  select(Sepal.Length, Sepal.Width,
         Petal.Length, Petal.Width) |> 
  as.matrix()

m1 = rda(Y)
biplot(m1, scaling = "species")

PC1 = summary(m1)$sites[,1]
PC2 = summary(m1)$sites[,2]
summary(m1) 

tmp = irist |> select(Species)
tmp = tmp |> mutate(PC1 = PC1,
                    PC2 = PC2)
ggplot(tmp)  +
  geom_point(
    aes(
      x = Species,
      y = PC1,
      color = Species
    ),
    position = position_jitter(0.1)
  )

m1 = lm(PC1 ~ Species, data = tmp)
anova(m1)

####
# 多変量分散分析
# Multivariate Analysis of Variance
# MANOVA
# 
Y = irist |> 
  select(Sepal.Length,
         Sepal.Width,
         Petal.Length,
         Petal.Width) |> 
  as.matrix()

m1 = lm(Y ~ Species, data = irist)
anova(m1)











ggplot(irist) +
  geom_point(
    aes(
      x = Species,
      y = Sepal.Width,
      color = Species
    )
  )









