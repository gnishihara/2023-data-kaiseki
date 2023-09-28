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

################################################
# 種ごとの回帰曲線
#################################
# PW と PL の回帰曲線
# mu = b0 + b1 x PW (モデル定義)
# PL ~ N(mu, sigma) (分布の定義)
#################################
# SHIFT+ALT+↓ 行を複製する

set = irist |> filter(str_detect(Species, "set"))
vir = irist |> filter(str_detect(Species, "vir"))
ver = irist |> filter(str_detect(Species, "ver"))

modelset = glm(PL ~ PW, 
             data = set,
             family = gaussian("identity"))
modelvir = glm(PL ~ PW, 
             data = vir,
             family = gaussian("identity"))
modelver = glm(PL ~ PW, 
             data = ver,
             family = gaussian("identity"))

plot(modelset)
plot(modelvir)
plot(modelver)


summary(modelset) # 係数表 (coefficients)
summary(modelvir) # 係数表 (coefficients)
summary(modelver) # 係数表 (coefficients)

anova(modelset, test = "F") # F検定
anova(modelvir, test = "F") # F検定
anova(modelver, test = "F") # F検定



# データの図に、モデルを当てはめる。
pdataset = set |> expand(PW = seq(min(PW), max(PW), length = 11))
pdatavir = vir |> expand(PW = seq(min(PW), max(PW), length = 11))
pdataver = ver |> expand(PW = seq(min(PW), max(PW), length = 11))

tmpset = predict(modelset, newdata = pdataset, se = TRUE) |> as_tibble()
tmpver = predict(modelvir, newdata = pdatavir, se = TRUE) |> as_tibble()
tmpvir = predict(modelver, newdata = pdataver, se = TRUE) |> as_tibble()

pdataset = bind_cols(pdataset, tmpset)
pdataver = bind_cols(pdataver, tmpver)
pdatavir = bind_cols(pdatavir, tmpvir)




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






