# ガラパゴス諸島の種数解析
# 一般化線形モデル
# 2023 09 28
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(ggpubr)
library(lemon)
library(emmeans)

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






