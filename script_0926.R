# Rの基礎： tidyverse 編
# 2023/ 09/ 26 
# Greg Nishihara

library(tidyverse)
library(ggpubr)

#データフレームを tibble に変換
irist = iris |> as_tibble() 

# irist 新しい変数を追加する

irist = irist |> 
  mutate(Petal.Area = Petal.Length * Petal.Width,
         Sepal.Area = Sepal.Length * Sepal.Width)

# group化して、記述統計量を求める
irist |> 
  group_by(Species) |> 
  summarise(SL_mean = mean(Sepal.Length),
            PL_mean = mean(Petal.Length))

# group化と across() の使い方
irist |> 
  group_by(Species) |> 
  summarise(across(c(Sepal.Length, 
                     Petal.Length),
                   mean))

# group化と across() の使い方,変数名の指定
irist |> 
  group_by(Species) |> 
  summarise(
    across(
      c(Sepal.Length, Petal.Length),
      list(m = mean)
      )
    )

# group化と across() の使い方,変数名の指定,
# 平均値、標準偏差を同時に求める
irist |> 
  group_by(Species) |> 
  summarise(
    across(
      c(Sepal.Length, Petal.Length),
      list(m = mean, s = sd)
    )
  )






