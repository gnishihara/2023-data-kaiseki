# ガラパゴス諸島の種数解析
# 一般化線形モデル
# 2023 09 28
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(ggpubr)
library(lemon)
library(emmeans)
library(statmod)

# alr4 パッケージの galapagos データ
data(galapagos, package = "alr4")

gala = galapagos |> 
  as_tibble(rownames = "island")

gala = gala |> 
  drop_na() |> 
  select(-EM, -island, -ES)

gala |> 
  pivot_longer(
    cols = c(
      Area, Anear, Dist, DistSC, 
      Elevation
    )
    ) |> 
  ggplot() + 
  geom_point(
    aes(
      x = value,
      y = NS
    )
  ) +
  facet_wrap(
    vars(name),
    scales = "free_x"
  )

###############################################
# mu = Anear + Area + Dist + DistSC + Elevation
# NS ~ N(mu, sigma)
###############################################

modelNF = glm(NS ~ Anear + Area + Dist + DistSC + Elevation,
              data = gala,
              family = gaussian("identity"))
plot(modelNF)


###############################################
# eta = Anear + Area + Dist + DistSC + Elevation
# mu ~ Poisson(eta)
# NS = exp(mu)
###############################################

modelPF = glm(NS ~ Anear + Area + Dist + DistSC + Elevation,
              data = gala,
              family = poisson("log"))

gala = gala |> 
  mutate(qres = qresid(modelPF),
         fit = fitted(modelPF))
gala |> 
  select(NS, qres, fit) |> 
  print(n = 25)

# ランダム化残差たい期待値
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = qres
    )
  ) +
  scale_y_continuous(limits = c(-10, 10))

# qqplot

ggplot(gala) + 
  geom_qq(
    aes(
      sample = qres
    )
  ) +
  geom_qq_line(
    aes(
      sample = qres
    ),
    linetype = "dashed",
    color = "grey50"
  )

# Scale-location
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  ) +
  geom_smooth(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  )



# モデルの改良
summary(modelPF)
# multi-collinearity
gala |>
  select(-qres, -fit) |> 
  cor()

# DistSC, Anear をモデルから外す
modelP01 = glm(NS ~ Area + Dist + Elevation,
               data = gala,
               family = poisson("log"))

gala = gala |> 
  mutate(qres = qresid(modelP01),
         fit = fitted(modelP01))
gala |> 
  select(NS, qres, fit) |> 
  print(n = 25)

# ランダム化残差たい期待値
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = qres
    )
  ) +
  scale_y_continuous(limits = c(-10, 10))

# qqplot

ggplot(gala) + 
  geom_qq(
    aes(
      sample = qres
    )
  ) +
  geom_qq_line(
    aes(
      sample = qres
    ),
    linetype = "dashed",
    color = "grey50"
  )

# Scale-location
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  ) +
  geom_smooth(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  )

summary(modelP01)

# モデル改良２
# 変数毎の平均値、標準偏差、最小値、最大値
gala |> 
  select(-qres, -fit) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarise(m = mean(value),
            s = sd(value),
            min = min(value),
            max = max(value))

gala |> 
  select(-qres, -fit) |> 
  mutate(logArea = log(Area),
         logDist = log(Dist),
         logElev = log(Elevation)) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarise(m = mean(value),
            s = sd(value),
            min = min(value),
            max = max(value))

########################################
# Area, Dist, Elev を log() 変換してから、
# 解析する

gala = gala |> mutate(logArea = log(Area),
               logDist = log(Dist),
               logElev = log(Elevation))

modelP02 = glm(NS ~ logArea + logDist + logElev,
               data = gala,
               family = poisson("log"))

gala = gala |> 
  mutate(qres = qresid(modelP02),
         fit = fitted(modelP02))
gala |> 
  select(NS, qres, fit) |> 
  print(n = 25)

# ランダム化残差たい期待値
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = qres
    )
  ) +
  scale_y_continuous(limits = c(-10, 10))

# qqplot

ggplot(gala) + 
  geom_qq(
    aes(
      sample = qres
    )
  ) +
  geom_qq_line(
    aes(
      sample = qres
    ),
    linetype = "dashed",
    color = "grey50"
  )

# Scale-location
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  ) +
  geom_smooth(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  )

summary(modelP02)

modelP03 = glm(NS ~ logArea + logDist,
               data = gala,
               family = poisson("log"))
summary(modelP03)

modelP04 = glm(NS ~ logArea + Dist,
               data = gala,
               family = poisson("log"))
summary(modelP04)

modelP05 = glm(NS ~ logArea + Dist + Anear,
               data = gala,
               family = poisson("log"))
summary(modelP05)
#############


gala = gala |> 
  mutate(logArea = log(Area),
         logDist = log(Dist),
         logElev = log(Elevation),
         logAnear = log(Anear))

modelPF = glm(NS ~ Area + Dist + Elevation + Anear,
               data = gala,
               family = poisson("log"))
car::vif(modelPF)

modelPF2 = glm(NS ~ Area + Dist + Anear,
              data = gala,
              family = poisson("log"))
car::vif(modelPF2)
summary(modelPF2)

modelPF3 = glm(NS ~ logArea + logDist + logAnear,
               data = gala,
               family = poisson("log"))
car::vif(modelPF3)
summary(modelPF3)

####################################
# 負の二項分布 一般化線形モデル
# Negative Binomial GLM
####################################
modelNB01 = MASS::glm.nb(NS ~ logArea + logDist + logAnear,
                         data = gala,
                         link = "log")

gala = gala |> 
  mutate(qres = qresiduals(modelNB01),
         fit = fitted(modelNB01))

# ランダム化残差と期待値の図
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = qres
    )
  ) +
  scale_y_continuous(limits = c(-2, 2))

# QQPLOT
ggplot(gala) +
  geom_qq(
    aes(
      sample = qres
    )
  ) +
  geom_qq_line(
    aes(
      sample = qres
    ),
    linetype = "dashed", color = "grey50"
  )

ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  ) +
  geom_smooth(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    ),
    se = F
  )


summary(modelNB01)
#############

modelNB02 = MASS::glm.nb(NS ~ logArea + logDist,
                         data = gala,
                         link = "log")

gala = gala |> 
  mutate(qres = qresiduals(modelNB02),
         fit = fitted(modelNB02))

# ランダム化残差と期待値の図
ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = qres
    )
  ) +
  scale_y_continuous(limits = c(-2, 2))

# QQPLOT
ggplot(gala) +
  geom_qq(
    aes(
      sample = qres
    )
  ) +
  geom_qq_line(
    aes(
      sample = qres
    ),
    linetype = "dashed", color = "grey50"
  )

ggplot(gala) + 
  geom_point(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    )
  ) +
  geom_smooth(
    aes(
      x = fit,
      y = sqrt(abs(qres))
    ),
    se = F
  )


summary(modelNB02)

modelNB03 = MASS::glm.nb(NS ~ logArea, data = gala, link = "log")


# モデル選択：AIC の低いモデルがいい
AIC(modelNB01, modelNB02, modelNB03)

pdata = gala |> 
  expand(Area = seq(min(Area), max(Area), length = 21),
         Dist = seq(min(Dist), max(Dist),length = 3)) |> 
  mutate(logArea = log(Area),
         logDist = log(Dist))

tmp = predict(modelNB02, newdata = pdata, se = T)
pdata = bind_cols(pdata, tmp)

ggplot(gala) + 
  geom_point(aes(x = logArea, y = NS)) +
  geom_line(
    aes(
      x = logArea,
      y = fit,
      color = Dist
    )
  )





