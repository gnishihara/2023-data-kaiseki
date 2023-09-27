# 多重比較の基礎
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

# irist をピボットして、その結果を
# 直接 ggplot() に渡す。
irist |> 
  pivot_longer(
    cols = c("SL", "SW", "PL", "PW")
  ) |> 
  ggplot() +
  geom_boxplot(
    aes(
      x = Species,
      y = value
    )
  ) +
  facet_rep_wrap(vars(name),
                 nrow = 2)
  
irist |> 
  pivot_longer(
    cols = c("SL", "SW", "PL", "PW")
  ) |> 
  ggplot() +
  geom_point(
    aes(
      x = Species,
      y = value
    ),
    position = position_jitter(width = 0.2),
    alpha = 0.2
  ) +
  facet_rep_wrap(vars(name),
                 nrow = 2)


# 多重比較
# 作業仮説：種ごとのPLの長さが違う
# 帰無仮説：種ごとのPLの平均値に違いがない
# Null Hypothesis: There are no 
# differences in the mean PL among species

# t 検定を３回しています。
# virginica と versicolor のt検定
# %>% の placeholder は　.
# |>  の placeholder は　_
irist |> 
  filter(!str_detect(Species, "setosa")) |> 
  t.test(PL ~ Species, data = _)

# versicolor と setosa のt検定
irist |> 
  filter(!str_detect(Species, "virginica")) |> 
  t.test(PL ~ Species, data = _)

# virginica と setosa のt検定
irist |> 
  filter(!str_detect(Species, "versicolor")) |> 
  t.test(PL ~ Species, data = _)

# パイプなしの t 検定 #####
# virginica と versicolor のt検定
data_ver_vir = irist |> 
  filter(!str_detect(Species, "setosa")) 
t.test(PL ~ Species, data = data_ver_vir)

# versicolor と setosa のt検定
data_ver_set = irist |> 
  filter(!str_detect(Species, "virginica")) 
t.test(PL ~ Species, data = data_ver_set)

# virginica と setosa のt検定
data_vir_set = irist |> 
  filter(!str_detect(Species, "versicolor")) 
t.test(PL ~ Species, data = data_vir_set)

# 多重比較

# setosa との比較のみ
model1 = lm(PL ~ Species, data = irist)
anova(model1)
# F(2, 147) = 1180.2, P < 0.0001
summary(model1)

# Tukey HSD 
emmeans(model1, 
        specs = pairwise ~ Species)

# データの標準偏差と標準誤差

se = function(x) {
  n = length(x)
  sd(x) / sqrt(n - 1)
}

irist |> 
  group_by(Species) |> 
  summarise(PL_mean = mean(PL),
            PL_se = se(PL))

# 残差 (residuals) とは

tmp = irist |> 
  filter(str_detect(Species, "virg")) |> 
  slice_sample(n = 10)

ggplot(tmp) +
  geom_point(
    aes(
      x = PL,
      y = PW
    )
  ) +
  geom_smooth(
    aes(
      x = PL,
      y = PW
    ),
    method = "lm",
    formula = y ~ x,
    se = FALSE
  )

residuals(model1)

irist = irist |> 
  mutate(resid = residuals(model1),
         fit   = fitted(model1))

ggplot(irist) + 
  geom_point(
    aes(
      x = Species,
      y = resid
    ),
    position = position_jitter(width = 0.1),
    alpha = 0.2
  )

# 残差プロットによる診断図
plot(model1)














