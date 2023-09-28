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
modelNBNULL = MASS::glm.nb(NS ~ 1, data = gala, link = "log") # 帰無モデル


# モデル選択：AIC の低いモデルがいい
# このとき、modelNB02 を選ぶ
AIC(modelNB01, modelNB02, modelNB03)

pdata = gala |> 
  expand(logArea = seq(min(logArea), max(logArea), length = 21),
         Dist = seq(min(Dist), max(Dist),length = 3)) |> 
  mutate(Area = exp(logArea),
         logDist = log(Dist))

tmp = predict(modelNB02, newdata = pdata, se = T)
pdata = bind_cols(pdata, tmp)

pdata = pdata |> 
  mutate(expect = exp(fit),
         l95 = exp(fit - 1.96 * se.fit),
         u95 = exp(fit + 1.96 * se.fit))

ggplot(gala) + 
  geom_point(aes(x = logArea, y = NS)) +
  geom_line(
    aes(
      x = logArea,
      y = expect,
      color = factor(Dist)
    ),
    data= pdata
  )





pdata = gala |> 
  expand(logArea = seq(min(logArea), max(logArea), length = 21),
         logDist = mean(logDist)) |> 
  mutate(Area = exp(logArea),
         Dist = exp(logDist))

tmp = predict(modelNB02, newdata = pdata, se = T)
pdata = bind_cols(pdata, tmp)

pdata = pdata |> 
  mutate(expect = exp(fit),
         l95 = exp(fit - 1.96 * se.fit),
         u95 = exp(fit + 1.96 * se.fit))

ggplot(gala) + 
  geom_ribbon(
    aes(
      x = logArea,
      ymin = l95, 
      ymax = u95
    ),
    data = pdata,
    alpha = 0.2
  ) +
  geom_point(aes(x = logArea, y = NS)) +
  geom_line(
    aes(
      x = logArea,
      y = expect,
      color = factor(Dist)
    ),
    data= pdata
  ) +
  guides(color = "none")

modelNB02

# Materials and Methods
# ... 
# A negative binomial GLM was used to analyze 
# the data. The link function (g()) was the natural 
# log function (Eq. 1). 
# 
# eta = b0 + b1 x1 + b2 x2
# mu = g(eta)
# eta ~ Neg. Bin. (mu, theta)
#
# x1 is the log Area, x2 is the log distance,
# b0 is the model intercept, b1 is the coefficient
# for x1, b2 is the coefficient for x2. 
# Diagnostic plots were used to examine the model
# for goodness of fit. The GLM was fitted using 
# R version 4.3.1 (R Core Team, 2023).

# R の引用：
citation()

AIC(modelNBNULL, modelNB02)
anova(modelNBNULL, modelNB02, test = "LRT")

# Results (AIC version)
# The model (Eq. 1) was compared to a null model 
# using AIC. The AIC of the null model was 261.4,
# whereas the AIC of Eq. 1 was 230.9. Therefore, 
# Eq. 1 sufficiently explained the relationship 
# between the number of species to the two explanatory
# variables (log Area and log distance).
# 
# The coefficient for log Area was 0.367 ± 0.0417 
# (z = 8.83; P < 0.0001) and the coefficient for
# log Distance was -0.149 ± 0.086 (z = -1.74; 
# P = 0.0828). The model intercept was 3.358 ± 0.222
# (z = 15.12; P < 0.0001). 










