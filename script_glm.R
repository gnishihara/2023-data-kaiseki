# 一般化線形モデル
# 2023 09 27
# Greg Nishihara
# 

#パッケージの読み込み
library(tidyverse) # データ処理・作図
library(ggpubr) # 作図用
library(lemon)  # 作図用

library(emmeans) # 多重比較用のパッケージ

irist = iris |> as_tibble()

# 変数名を修正
irist = irist |> 
  select(SL = Sepal.Length,
         SW = Sepal.Width,
         PL = Petal.Length,
         PW = Petal.Width,
         Species)

ggplot(irist) +
  geom_point(
    aes(
      x = PW,
      y = PL
    )
    )
#################################
# PW と PL の回帰曲線
# mu = b0 + b1 x PW (モデル定義)
# PL ~ N(mu, sigma) (分布の定義)
#################################
model1 = glm(PL ~ PW, 
             data = irist,
             family = gaussian("identity"))
plot(model1)
summary(model1) # 係数表 (coefficients)
anova(model1)   # 逸脱度表 (deviance)
anova(model1, test = "F") # F検定
anova(model1, test = "LRT") # 尤度比検定


# データの図に、モデルを当てはめる。
pdata = irist |> 
  expand(PW = seq(min(PW), max(PW), length = 11))
tmp = predict(model1, newdata = pdata, se = TRUE) |> 
  as_tibble()
pdata = bind_cols(pdata, tmp)

ggplot(irist) +
  geom_point(
    aes(
      x = PW,
      y = PL,
      color = Species
    )
  ) +
  geom_line(
    aes(
      x = PW,
      y = fit
    ),
    data = pdata,
    color = "orangered",
    linewidth = 1
  )
