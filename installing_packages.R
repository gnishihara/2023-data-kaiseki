# パッケージのインストール
# tidyverse: 様々なパッケージのメタパッケージ

# 作図用のパッケージとデータ処理用のパッケージをインストール
# パッケージは一回インストールするだけ。

# install.packages(c("tidyverse", "ggpubr", "lemon", "showtext"))

# tidyverse パッケージを読み込むと、
# tidyverse パッケージの関数が優先的に
# 使われる
library(tidyverse)
lag() # tidyverse (dplyr) の lag() 
stats::lag() # stats パッケージの lag()
library(showtext)
