# 多重比較の基礎
# 2023 09 23
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

# virginica と versicolor のt検定
irist |> 
  filter(!str_detect(Species, "setosa")) |> 
  t.test(PL ~ Species, data = _)

# versicolor と setosa のt検定
irist |> 
  filter(!str_detect(Species, "virginca")) |> 
  t.test(PL ~ Species, data = _)

# virginica と setosa のt検定
irist |> 
  filter(!str_detect(Species, "versicolor")) |> 
  t.test(PL ~ Species, data = _)












  
  
