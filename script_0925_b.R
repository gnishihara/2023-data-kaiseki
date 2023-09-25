# 既存Rデータの操作 (iris)
# 2023/09/25
# Greg Nishihara

iris # データフレーム
pl = iris$Petal.Length
pw = iris$Petal.Width

# Area
pa = pl * pw
iris$Petal.Area = pa

iris$Sepal.Area =
  iris$Sepal.Length * iris$Sepal.Width

# 平均値と標準偏差・標準誤差

mean(x = iris$Petal.Area) # 平均値の関数
sd(x = iris$Petal.Area) # 標準偏差

# 標準誤差: s / sqrt(n - 1)
s = sd(x = iris$Petal.Area)  # 標準偏差
n = length(x = iris$Petal.Area) # 要素の数
s / sqrt(n - 1) # 標準誤差

# 分散
var(x = iris$Petal.Area)

# 中央値
median(x = iris$Petal.Area)

# 範囲
range(iris$Petal.Area)

# 最小値と最大値
min(iris$Petal.Area)
max(iris$Petal.Area)

# 四分位数
quantile(iris$Petal.Area)

# 作図
pl = iris$Petal.Length
pw = iris$Petal.Width
plot(x = pl, y = pw)

pa = iris$Petal.Area
sa = iris$Sepal.Area
plot(x = pa, y = sa)


