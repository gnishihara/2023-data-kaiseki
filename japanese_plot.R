# 日本語入りの図
# 2023 09 27
# Greg Nishihara

library(tidyverse)
library(ggpubr)
library(lemon)
library(showtext)
library(magick)

google_fonts("Noto Sans JP")
font_add_google("Noto Sans JP",
                family = "notosansjp")

# theme_gray() をデフォルトの
# ggplot テーマにする。
theme_gray(base_size = 12,
           base_family = "notosansjp") |> 
  theme_set()

showtext_auto() # 必ず一度だけ、実行すること

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

filename = "iris.pdf"
pngname = "iris.png"

ggsave(filename = filename,
       width = 80,
       height = 80, 
       units = "mm")

img = image_read_pdf(filename, density = 600)
image_write(image = img,
            path = pngname)

