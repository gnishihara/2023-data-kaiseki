# 日本語入りの図
# 2023 09 27
# Greg Nishihara

library(tidyverse)
library(ggpubr)
library(lemon)
library(showtext)

irist = iris |> as_tibble()

xlabel = "花びらの長さ (cm)"
ylabel = "花びらの幅 (cm)"

ggplot(irist) + 
  geom_point(
    aes(x = Petal.Length,
        y = Petal.Width)
  ) +
  scale_x_continuous(xlabel) +
  scale_y_continuous(ylabel)

