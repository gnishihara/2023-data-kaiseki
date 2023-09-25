# tidyverse システムの基礎
# 現代R
# 2023/09/25
# Greg Nishihara

# 使いたいパッケージから読み込む
library(tidyverse)
library(ggpubr)
library(lemon)
library(showtext)

# 作図の環境を準備する 
# 注意：最新バージョンのRではないと、下記のコードは動かない
# かもしれないです。
font_add_google("Noto Sans JP", family = "notosansjp")
showtext_auto()

# theme_gray(base_family = "notosansjp")

# 関数の定義
se = function(x) {
  n = length(x)
  s = sd(x)
  s / sqrt(n - 1)
}

se(x = iris$Sepal.Length)

# データの読み込み

# データの処理
# tidyverse データフレームよりも、tibble を使う
# パイプ演算子 %>% または |> (CTRL + SHIFT + M)
# 左辺の結果を右辺の関数の第1引数に渡す
iris |> as_tibble() # データフレームをtibble に変換する

iris |> 
  as_tibble() |> 
  print(n = 20)

irist = iris |> as_tibble()
irist

irist = irist |> 
  mutate(Petal.Area = Petal.Length * Petal.Width,
         Sepal.Area = Sepal.Length * Sepal.Width)


# 記述統計量

# Tidyverse の処理方法
irist |> 
  filter(str_detect(Species, "setosa")) |> 
  summarise(Petal.Area_mean = mean(Petal.Area),
            Petal.Area_sd = sd(Petal.Area),
            Petal.Area_se = se(Petal.Area))

# 既存Rの方法
test = iris$Species == "setosa" # Species が setosa と等しい
iris[test, ] # test が正の値をとるものを返す
iris[test, c("Petal.Length", "Petal.Width")]
pl = iris[test, "Petal.Length"]
pw = iris[iris$Species == "setosa", "Petal.Width"]
area = pl * pw
mean(area)
sd(area)
se(area)

# setosa の記述統計量
irist |> 
  filter(str_detect(Species, "setosa")) |> 
  summarise(Petal.Area_mean = mean(Petal.Area),
            Petal.Area_sd = sd(Petal.Area),
            Petal.Area_se = se(Petal.Area))

# virginica の記述統計量
irist |> 
  filter(str_detect(Species, "virginica")) |> 
  summarise(Petal.Area_mean = mean(Petal.Area),
            Petal.Area_sd = sd(Petal.Area),
            Petal.Area_se = se(Petal.Area))

# versicolor の記述統計量
irist |> 
  filter(str_detect(Species, "versicolor")) |> 
  summarise(Petal.Area_mean = mean(Petal.Area),
            Petal.Area_sd = sd(Petal.Area),
            Petal.Area_se = se(Petal.Area))

# group_by() グループごと記述統計量

irist_summary = irist |> 
  group_by(Species) |> 
  summarise(Petal.Area_mean = mean(Petal.Area),
            Petal.Area_sd = sd(Petal.Area),
            Petal.Area_se = se(Petal.Area))

fname = "irist_summary.csv"
write_csv(x = irist_summary, file = fname)

# CSVファイルの読み込み
Z = read_csv(file = "irist_summary.csv", skip = 0)

# write.csv() # 既存Rの関数
# write_csv() # tidyverse の関数
 
# 作図

ggplot(Z) + 
  geom_point(aes(x = Species,
                 y = Petal.Area_mean))

# ２種類の定義
ylabel = "'Petal Area'~('cm'^2)" # オリジナル（カッコが斜め）
# ylabel = "'Petal Area'~'(cm'^2*')'"　# 縦カッコ
# ylabel2 = expression('Petal Area'~('cm'^2))

parse(text = ylabel)
ggplot(Z) +
  geom_col(aes(x = Species,
               y = Petal.Area_mean)) +
  scale_x_discrete("Species") +
  scale_y_continuous(parse(text = ylabel), limits = c(0, 15)) 
  # scale_y_continuous(ylabel2, limits = c(0, 15))

ggsave(filename = "plot1.png",
       width = 600, height = 600, units = "px")

ggplot(Z) + 
  geom_point(aes(x = Species,
                 y = Petal.Area_mean)) +
  geom_errorbar(aes(x = Species,
                    ymin = Petal.Area_mean - Petal.Area_sd,
                    ymax = Petal.Area_mean + Petal.Area_sd),
                width = 0.1) +
  scale_x_discrete("Species") +
  scale_y_continuous(parse(text = ylabel), 
                     limits = c(0, 15)) +
  # labs(caption = "Error bars indicate 1 standard deviation.") +
  labs(caption = "エラーバーは１標準偏差")

ggsave(filename = "plot1jp.png",
       width = 600, height = 600, units = "px")


ggplot(Z) +
  geom_col(aes(x = Species,
               y = Petal.Area_mean),
           fill = "grey20") +
  geom_errorbar(aes(x = Species,
                    ymin = Petal.Area_mean,
                    ymax = Petal.Area_mean + Petal.Area_sd),
                width = 0.0,
                linewidth = 2,
                color = "grey20") +
  geom_errorbar(aes(x = Species,
                    ymin = Petal.Area_mean - Petal.Area_sd,
                    ymax = Petal.Area_mean),
                width = 0.0,
                linewidth = 2,
                color = "grey80") +
  scale_x_discrete("Species") +
  scale_y_continuous(parse(text = ylabel), limits = c(0, 15)) 

# 生データの散布図

ylabel = "'Sepal area'~'(cm'^2*')'"
xlabel = "'Petal area'~'(cm'^2*')'"

ggplot(irist)  +
  geom_point(aes(x = Petal.Area,
                 y = Sepal.Area,
                 color = Species,
                 shape = Species)) +
  scale_x_continuous(parse(text = xlabel)) +
  scale_y_continuous(parse(text = ylabel)) +
  scale_color_viridis_d(end = 0.8) +
  theme_grey(base_size = 12) +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank())

ggsave("plot2.pdf",
       width = 10, height = 10, units = "cm")

ggsave("plot2.svg",
       width = 10, height = 10, units = "cm")

# データ解析


 
 