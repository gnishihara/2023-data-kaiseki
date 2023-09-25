# 演習０９２５のスクリプト
# 2023年 09月 25日
# Greg Nishihara

# ベクトル (Vectors)

c(1, 3, 5)
c(10.2, "A", 40) # c(数字 , 文字 , 数字)
c(1, FALSE, TRUE, 20) # c(数字, 論理値, 論理値, 数字)

c(TRUE, F, FALSE, T) # 論理値は大文字
c("TRUE", "FALSE")   # 文字

# ベクトルとオブジェクト
v1 = c(1, 2, 3, 10, 20, 30) # v1 というオブジェクト
# 代入: <- "ALT + -" または = 

# v1[2]: ２番目の要素を抽出する
v1[2]^2
v1[5]*2

w1 = c(1, 2, 3)
v1[w1]

w2 = c(4, 5, 6)
v1[w2]

v1^2

v1[w1] + v1[w2]
# c(1, 2, 3) + c(10, 20, 30)  = 
# c(11, 22, 33)

# 行列・配列 (matrix, array)
a1 = c(2, 5, 7)
a2 = c(-6, 8, 9)

cbind(a1, a2) # column-bind 列ごとに結合
rbind(a1, a2) # row-bind 行ごとに結合

X = cbind(a1, a2)
t(X)

X * t(X) # エラーでます
t(X) * X # エラーでます

X %*% t(X)
Z = X %*% t(X)
Z^{-1}

# リスト (list)

a1 = c(0, 2, -5)
a2 = c(9, 5, 1, 2)

cbind(a1, a2) # 警告がでます

a1 = c(0, 2, -5)
a2 = c(9, 5, 1, 2, 3)

cbind(a1, a2) # 警告がでます

a1 = c(0, 2, -5)
a2 = c(9, 5, 1, 2, 3, 4)

cbind(a1, a2) 

list(a1, a2)

t1 = c("A", "Z")

list(a1, t1, a2)

# データフレーム (data frame)

a1 = c(1, 2, 3)
a2 = c(20, 25, 30)
t1 = c("W", "X", "Y")
l1 = c(T, F, T)

data.frame(a1, a2, t1, l1)

df1 = data.frame(a1, a2, t1, l1)

df1$a2
df1$t1[2]
