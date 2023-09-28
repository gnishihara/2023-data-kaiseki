# 一般化加法モデル 2
# Generalized Additive Models (GAM)
# 2023 09 28
# Greg Nishihara

library(tidyverse) # データ処理・作図用
library(ggpubr)    # 作図用
library(lemon)     # 作図用

library(gratia) # GAM用パッケージ
library(mgcv)   # GAM用パッケージ

library(statmod) # ランダム化残差用

# C と O は大文字
dset = CO2 |> as_tibble()
dset = dset |> mutate(logconc = log(conc))
dset = dset |> 
  mutate(Plant = factor(as.character(Plant)))

ggplot(dset) + 
  geom_point(
    aes(
      x = logconc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = logconc,
      y = uptake,
      color = Plant
    )
  )

# GAM 
m1 = gam(uptake ~ logconc, data = dset)
pdata = dset  |> 
  expand(logconc = seq(min(logconc),
                       max(logconc),
                       length = 21))
tmp = predict(m1, 
              newdata = pdata,
              se = T) |> as_tibble()
pdata = bind_cols(pdata, tmp)
pdata

ggplot(dset) + 
  geom_point(
    aes(
      x = logconc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = logconc,
      y = fit
    ),
    data = pdata
  )

# くねくねさせる
# ノット (knot) の数を指定する

m2 = gam(uptake ~ s(logconc, k = 6), data = dset)
pdata = dset  |> 
  expand(logconc = seq(min(logconc),
                       max(logconc),
                       length = 21))
tmp = predict(m2, 
              newdata = pdata,
              se = T) |> as_tibble()
pdata = bind_cols(pdata, tmp)
pdata

ggplot(dset) + 
  geom_point(
    aes(
      x = logconc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = logconc,
      y = fit
    ),
    data = pdata
  )

summary(m1)
summary(m2)
AIC(m1, m2)

appraise(m2)

# それぞれの Plant に s() をあてはめる

m3  = gam(uptake ~ s(logconc, k = 6, by = Plant) + Plant, data = dset)
m3b = gam(uptake ~ s(logconc, k = 3, by = Plant) + Plant, data = dset)
m3c = gam(uptake ~ s(logconc, k = 4, by = Plant) + Plant, data = dset)
m3d = gam(uptake ~ s(logconc, k = 5, by = Plant) + Plant, data = dset)

AIC(m3, m3b, m3c, m3d)

summary(m3)

pdata = dset |> 
  expand(Plant = Plant,
         logconc = seq(min(logconc),
                       max(logconc),
                       length = 11))
tmp = predict(m3, newdata = pdata, se = T) |> as_tibble()
pdata = bind_cols(pdata, tmp)
pdata

ggplot(dset) + 
  geom_point(
    aes(
      x = logconc,
      y = uptake, 
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = logconc,
      y = fit,
      color = Plant
    ),
    data = pdata
  ) +
  scale_color_viridis_d() +
  facet_wrap(vars(Plant))

appraise(m3)

AIC(m2, m3)


##

m4 = gam(uptake ~ 
      s(logconc, k = 6) + 
      s(logconc, by = Plant, m = 1, k = 6) +
      s(Plant, bs = "re", k = 12),
    data = dset)

appraise(m4)
appraise(m3)
draw(m4)
ggsave("gam-appraise.png",
       width = 4000,
       height = 4000,
       units = "px")

