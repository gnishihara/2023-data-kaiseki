# 一般化加法モデル
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

ggplot(dset) + 
  geom_point(
    aes(
      x = conc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = conc,
      y = uptake,
      color = Plant
    )
  )

##
# GAM 

m1 = gam(uptake ~ conc, data = dset)
pdata = dset  |> 
  expand(conc = seq(min(conc),
                    max(conc),
                    length = 21))
tmp = predict(m1, 
              newdata = pdata,
              se = T) |> as_tibble()
pdata = bind_cols(pdata, tmp)
pdata

ggplot(dset) + 
  geom_point(
    aes(
      x = conc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = conc,
      y = fit
    ),
    data = pdata
  )



# くねくねさせる
# ノット (knot) の数を指定する

m2 = gam(uptake ~ s(conc, k = 6), 
         data = dset)
pdata = dset  |> 
  expand(conc = seq(min(conc),
                    max(conc),
                    length = 21))
tmp = predict(m2, 
              newdata = pdata,
              se = T) |> as_tibble()
pdata = bind_cols(pdata, tmp)
pdata

ggplot(dset) + 
  geom_point(
    aes(
      x = conc,
      y = uptake,
      color = Plant
    )
  ) +
  geom_line(
    aes(
      x = conc,
      y = fit
    ),
    data = pdata
  )

