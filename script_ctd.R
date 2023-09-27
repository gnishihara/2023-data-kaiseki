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

# 作業の効率化 ########################################

filename = "./data/CTD_Rseminer_st1_A_230926.csv"

## ファイル名からステーション、観測者ID、日付を抽出する
tmp = basename(filename)
## 正規表現を用いてする
station = str_extract(tmp, "st[0-9]")
id      = str_extract(tmp, "_[A-Z]_") |> str_extract("[A-Z]")
date    = str_extract(tmp, "[0-9]{6}")

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

filename = "./data/CTD_Rseminer_st1_A_230926.csv"
ctd01a = read_ctd_data(filename = filename)

filename = "./data/CTD_Rseminer_st1_B_230926.csv"
ctd01b = read_ctd_data(filename = filename) # あと１６回繰り返して実行する

######################################################
# さらに効率のいい方法
# データ読み込み関数
read_ctd_data = function(filename) {
  ctd01 = read_csv(file = filename, skip = 28)
  ctd01 |> 
    select(depth = `水深 (m)`,
           temperature = `水温 (℃)` ,
           salinity = `塩分濃度 (PSS)`)
}
filename = "./data/CTD_Rseminer_st1_B_230926.csv"
read_ctd_data(filename = filename) # あと１６回繰り返して実行する

filenames = dir("./data", full.names = TRUE)
dataset = tibble(filenames)

dataset = dataset |> 
  mutate(data = map(filenames, read_ctd_data))

dataset = dataset |> 
  mutate(filenames = basename(filenames)) |> 
  separate(filenames, 
           into = c("a", "b",
                    "station", 
                    "id", 
                    "date",
                    "z")) |> 
  select(station, id, date, data)

dataset
# map() の説明

irist = iris |> as_tibble()
# Species ごとに、分解する
set = irist |> filter(str_detect(Species, "setosa"))
vir = irist |> filter(str_detect(Species, "virg"))
ver = irist |> filter(str_detect(Species, "vers"))

# Species ごとに回帰曲線を当てはめる
model_set = lm(Petal.Length ~ Petal.Width, data = set)
model_vir = lm(Petal.Length ~ Petal.Width, data = vir)
model_ver = lm(Petal.Length ~ Petal.Width, data = ver)

get_lm = function(data) {
  lm(Petal.Length ~ Petal.Width, data = data)
}

irist |> 
  group_nest(Species) |> 
  mutate(model = map(data, get_lm))










# rooturl = "https://github.com/gnishihara/2023-data-kaiseki/blob/main/data/"
# f1 = "CTD_Rseminer_st1_A_230926.csv"
# 
# str_c(rooturl, f1) |> read_csv()
