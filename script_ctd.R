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

# 作業の効率化

## ファイル名からステーション、観測者ID、日付を抽出する
tmp = basename(filename)
station = str_extract(tmp, "st[0-9]")
id = str_extract(tmp, "_[A-Z]_") |> str_extract("[A-Z]")
date = str_extract(tmp, "[0-9]{6}")


# データ読み込み関数
read_ctd_data = function(filename) {
  tmp = basename(filename)
  station = str_extract(tmp, "st[0-9]")
  id = str_extract(tmp, "_[A-Z]_") |> str_extract("[A-Z]")
  date = str_extract(tmp, "[0-9]{6}")
  
  ctd01 = read_csv(file = filename, skip = 28)
  ctd01 = ctd01 |> 
    select(depth = `水深 (m)`,
           temperature = `水温 (℃)` ,
           salinity = `塩分濃度 (PSS)`)
  ctd01 |> 
    mutate(station = station,
           id = id,
           date = date)
}




# rooturl = "https://github.com/gnishihara/2023-data-kaiseki/blob/main/data/"
# f1 = "CTD_Rseminer_st1_A_230926.csv"
# 
# str_c(rooturl, f1) |> read_csv()
