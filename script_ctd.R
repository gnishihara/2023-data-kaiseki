# CTD データ処理
# 2023 09 27
# Greg Nishihara

# パッケージの読み込み
library(tidyverse)
library(ggpubr)
library(lemon)
###############################################

filename = "./data/CTD_Rseminer_st1_A_230926.csv"
ctd01 = read_csv(file = filename, skip = 28)
# read.csv() # 既存のR関数よりも、read_csv()をつかいましょう
ggplot(ctd01) + 
  geom_point(
    aes(x = `水深 (m)`,
        y = `水温 (℃)`)
  )

# 変数名を英語にかえて、必要なものだけ残す
# 残す変数:　水深、水温、塩分
ctd01 = ctd01 |> 
  select(depth = `水深 (m)`,
         temperature = `水温 (℃)` ,
         salinity = `塩分濃度 (PSS)`)
# ステーションの情報と観測者のIDも追加する
ctd01 = ctd01 |> 
  mutate(station = "st1",
         id = "A",
         date = "230926")

 

# rooturl = "https://github.com/gnishihara/2023-data-kaiseki/blob/main/data/"
# f1 = "CTD_Rseminer_st1_A_230926.csv"
# 
# str_c(rooturl, f1) |> read_csv()
