# Rの基礎： tidyverse 編
# 2023/ 09/ 26 
# Greg Nishihara

library(tidyverse)
library(ggpubr)
library(lemon)

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

# group化と across() の使い方,変数名の指定,
# 平均値を同時に求める。
# すべての変数を処理する
irist |> 
  group_by(Species) |> 
  summarise(
    across(
      c(Sepal.Length, Petal.Length,
        Sepal.Width, Petal.Width,
        Sepal.Area, Petal.Area),
      list(m = mean)
    )
  )

# group化と across() の使い方,変数名の指定,
# 平均値を同時に求める。
# すべての変数を処理する (everything())
irist |> 
  group_by(Species) |> 
  summarise(
    across(
      everything(),
      list(m = mean)
    )
  )

# group化と across() の使い方,変数名の指定,
# 平均値, 標準偏差を同時に求める。
# すべての変数を処理する (everything())
irisw = irist |> 
  group_by(Species) |> 
  summarise(
    across(
      everything(),
      list(m = mean, s = sd)
    )
  )

# pivoting data （ピボット)

irisw |> colnames()

irisw |> 
  pivot_longer(
    cols = c(Sepal.Length_m,
             Sepal.Length_s, Sepal.Width_m, 
             Sepal.Width_s,  Petal.Length_m,
             Petal.Length_s, Petal.Width_m, 
             Petal.Width_s,  Petal.Area_m,  
             Petal.Area_s,   Sepal.Area_m,
             Sepal.Area_s)
  )


# pivoting data （ピボット)
# matches() 変数を選ぶ
irisl = irisw |> 
  pivot_longer(
    cols = c(matches("Sepal"),
             matches("Petal"))
  )

# 文字変数の文字を分解する
# "[._]": 正規表現 (regular expression) の一種
irisl = irisl |> 
  separate(name, 
           sep = "[._]",
           into = c("Part", 
                    "Measurement",
                    "Statistic"))

# pivot_wider()
# 平均値と標準偏差を分ける

irisl2 = irisl |> 
  pivot_wider(names_from = Statistic,
              values_from = value)

# ggplot の図
ylabel = "Value"
ggplot(irisl2) + 
  geom_point(
    aes(
      x = Species,
      y = m,
      color = Part,
      shape = Measurement
    )) +
  scale_y_continuous(ylabel) +
  facet_grid(
    rows = vars(Part),
    cols = vars(Measurement)
  )

# ggplot の図
# エラーバーをつける
ylabel = "Value"
ggplot(irisl2) + 
  geom_point(
    aes(
      x = Species,
      y = m,
      color = Part,
      shape = Measurement
    )) +
  geom_errorbar(
    aes(
      x = Species,
      ymin = m - s,
      ymax = m + s,
      color = Part
    ),
    width = 0
  ) + 
  scale_y_continuous(ylabel) +
  facet_grid(
    rows = vars(Part),
    cols = vars(Measurement)
  )

# ggplot の図
# エラーバーをつける
# 凡例を外す
ylabel = "Value"
ggplot(irisl2) + 
  geom_point(
    aes(
      x = Species,
      y = m,
      color = Part,
      shape = Measurement
    )) +
  geom_errorbar(
    aes(
      x = Species,
      ymin = m - s,
      ymax = m + s,
      color = Part
    ),
    width = 0
  ) + 
  scale_y_continuous(ylabel) +
  guides(color = "none",
         shape = "none") +
  facet_grid(
    rows = vars(Part),
    cols = vars(Measurement)
  )

# Area と Length/Width のデータを分ける
irisl2_area = 
  irisl2 |> 
  filter(Measurement == "Area")

irisl2_lw = 
  irisl2 |> 
  filter(Measurement != "Area")

# Area と Length/Width のデータを分ける
# str_detect() をつかう
irisl2 |> 
  filter(
    str_detect(
      Measurement,
      "Wi"
    ))

# irisl2_lw の図
# Measurement を使って、二つの新しい
# 変数をつくる
irisl |> 
  pivot_wider(
    names_from = Measurement,
    values_from = value
  )

irisl_lwa = irisl |> 
  pivot_wider(
    names_from = c(Measurement, Statistic),
    values_from = value
  )

xlabel = "Width (cm)"
ylabel = "Length (cm)"

ggplot(irisl_lwa) + 
  geom_point(
    aes(
      x = Width_m,
      y = Length_m,
      color = Species,
      shape = Part
    )
  ) +
  geom_errorbar(
    aes(
      x = Width_m,
      y = Length_m,
      xmin = Width_m - Width_s,
      xmax = Width_m + Width_s,
      color = Species
    ),
    width = 0
  ) +
  geom_errorbar(
    aes(
      x = Width_m,
      y = Length_m,
      ymin = Length_m - Length_s,
      ymax = Length_m + Length_s,
      color = Species
    ),
    width = 0
  ) +
  guides(shape = "none") +
  scale_x_continuous(xlabel,
                     limits = c(0, 6)) +
  scale_y_continuous(ylabel,
                     limits = c(0, 8)) +
  facet_grid(
    cols = vars(Part)
  ) +
  theme(
    legend.position = c(0.5, 0.0),
    legend.justification = c(0.0, 0.0),
    legend.title = element_blank(),
    legend.background = element_blank()
  )


# 論文用図
# 

xlabel = "Width (cm)"
ylabel = "Length (cm)"

ggplot(irisl_lwa) + 
  geom_point(
    aes(
      x = Width_m,
      y = Length_m,
      color = Species,
      shape = Part
    )
  ) +
  geom_errorbar(
    aes(
      x = Width_m,
      y = Length_m,
      xmin = Width_m - Width_s,
      xmax = Width_m + Width_s,
      color = Species
    ),
    width = 0
  ) +
  geom_errorbar(
    aes(
      x = Width_m,
      y = Length_m,
      ymin = Length_m - Length_s,
      ymax = Length_m + Length_s,
      color = Species
    ),
    width = 0
  ) +
  guides(shape = "none") +
  scale_x_continuous(xlabel,
                     limits = c(0, 6)) +
  scale_y_continuous(ylabel,
                     limits = c(0, 8)) +
  facet_rep_grid(
    cols = vars(Part)
  ) +
  theme_pubr() + # ggpubr パッケージの関数
  theme(
    legend.position = c(1.0, 0.0),
    legend.justification = c(1.0, 0.0),
    legend.title = element_blank(),
    legend.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_rect(color = "black",
                                linewidth = 1),
    axis.line = element_line(linewidth = 0)
  )

