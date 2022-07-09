# ----------------------------------------------------------------------------------------------
# ku15bizDS1mac.R:  Ｒ言語の基本操作とプログラミング入門
#                                                    by T. Kuwahara,  Keio Univ.
# ----------------------------------------------------------------------------------------------
# 作業ディレクトリの設定（自分の環境にあわせて修正して下さい）
# setwd("~/Document/Lecture"")

# 青木先生（群馬大）による関数の読み込み（自分の環境にあわせて修正して下さい）
# source("all.R", encoding="EUC-JP")

       # インターネットが利用可能ならば、こうすればいつでも最新版
       # source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")


# エンジンの重量問題データ
engwei <- c(
53.41,   53.16,   53.21,   53.27,   53.31,
53.23,   53.36,   53.14,   53.47,   53.40,
53.08,   53.33,   53.72,   53.43,   53.54,
53.34,   53.29,   53.18,   53.34,   53.38,
53.63,   53.52,   53.25,   53.57,   53.35,
53.65 )

# ヒストグラム（単純）
hist(engwei)
#  ヒストグラム（タイトル、色つき)
hist(engwei, main="エンジンの重量", xlab="weight", col="red")


# バイクのシート高問題：身長データ　　（大きめのデータ例)
height <-c(
  175.7,  174.2,  169.7,  170.4,  171.7,  171.5,  167.5,  146.1,  175.4,  165.4,  170.8,  162.9,  178.0,
  171.5,  156.4,  177.0,  171.2,  176.6,  164.5,  158.5,  172.9,  182.7,  166.3,  159.3,  158.9,  162.0,
  160.8,  171.9,  162.5,  155.6,  159.2,  145.0,  186.3,  180.1,  159.8,  173.0,  155.7,  168.0,  167.1,
  162.8,  162.8,  152.1,  161.0,  172.3,  159.6,  158.0,  166.2,  167.4,  169.4,  153.4,  168.1,  162.1,
  162.4,  157.2,  158.7,  148.3,  160.1,  170.6,  165.0,  159.0,  155.4,  169.5,  167.5,  163.6,  166.6,
  163.1,  167.5,  159.7,  165.4,  171.6,  157.3,  143.1,  168.8,  176.1,  153.1,  153.7,  176.0,  158.2,
  145.3,  144.5,  155.9,  159.1,  164.8,  158.2,  167.3,  157.5,  170.8,  154.4,  163.7,  177.8,  161.0,
  159.7,  178.0,  161.9,  164.0,  166.3,  175.0,  162.8,  172.9,  159.2,  155.6,  176.0,  154.5,  160.6,
  151.1,  162.6,  156.3,  162.7,  169.6,  161.7,  167.9,  153.8,  151.0,  174.3,  161.4,  151.5,  170.8,
  173.8,  162.0,  164.4,  141.1,  158.8,  161.6,  163.1,  168.5,  175.8,  172.8,  179.0,  162.1,  146.1,
  164.2,  173.5,  164.3,  150.0,  160.7,  161.7,  161.2,  179.4,  163.8,  164.9,  173.8,  163.0,  165.6,
  159.6,  166.6,  152.2,  160.6,  172.5,  160.1,  169.0,  169.9,  167.4,  167.8,  170.2,  160.6,  156.1,
  154.3,  168.9,  171.1,  161.8,  175.5,  166.5,  166.5,  156.1,  168.4,  153.9,  153.4,  181.1,  158.2,
  178.6,  163.7,  142.2,  161.8,  166.0,  162.0,  180.1,  167.5,  169.9,  156.7,  164.5,  159.9,  191.8,
  165.2,  146.3,  167.2,  168.3,  151.5,  139.7,  159.5,  185.4,  153.1,  163.7,  154.9,  164.0,  155.9,
  173.5,  157.0,  167.9,  162.7,  172.7,  158.7,  162.8,  164.0,  152.1,  166.3,  163.6,  176.2,  161.3,
  149.0,  164.0,  153.7,  160.3,  161.3,  164.2,  161.7,  165.9,  174.0,  153.3,  170.0,  158.3,  170.0,
  156.9,  175.6,  176.5,  184.6,  161.7,  164.5,  169.3,  167.3,  169.3,  173.8,  173.5,  180.1,  160.0,
  160.2,  180.7,  171.5,  170.3,  164.9,  165.3,  155.2,  154.2,  161.9,  169.5,  160.5,  158.7,  157.9,
  167.9,  158.5,  157.5,  166.1,  164.1,  183.7,  153.7,  155.6,  165.3,  151.7,  160.9,  158.4,  155.9,
  166.8,  154.0,  158.5,  164.9,  163.5,  174.2,  163.9,  149.0,  163.8,  153.9,  168.7,  158.6,  156.3,
  157.2,  161.6,  176.0,  169.4,  161.3,  160.2,  166.2,  158.8,  167.6,  156.6,  169.4,  166.1,  165.1,
  167.1,  183.8,  148.0,  152.3,  159.0,  156.2,  155.5,  162.2,  167.4,  166.5,  173.1,  163.1,  156.6,
  169.2,  166.4,  158.7,  160.0,  156.5,  171.5,  164.7,  166.5,  181.5,  163.7,  164.4,  166.3,  165.3,
  146.2,  173.8,  164.2,  152.2,  160.4,  170.2,  159.4,  157.3,  155.2,  162.9,  161.5,  162.5,  158.9,
  159.4,  171.0,  152.8,  161.1,  163.7,  157.9,  167.3,  164.6,  167.4,  149.6,  174.6,  162.2,  158.8,
  165.5,  162.5,  170.6,  171.1,  158.9,  157.2,  178.7
)

# ヒストグラムの作成
hist(height)
hist(height , main="ターゲット女性の身長", xlab="身長", col="red")

# 箱ひげ図の作成
boxplot(height)
boxplot(height, range=1.5, horizontal=T,col="bisque", frame=T)
hist(height, main="", col="bisque")

#　基本統計量（平均と分散、標準偏差など）
mean(height)
sd(height)
var(height)
summary(height)




#　Ｒ言語によるプログラミング入門

# 　変数
kuwa <- 3.14
kuwa

kuwa <- 777
kuwa
cat(kuwa)
print(kuwa)

kuwa <- "take"
kuwa

cat(kuwa)
print(kuwa)
 

A <-7
A
A + A

A <- "7"
A
A + A
cat(A, A)

as.numeric(A) + as.numeric(A)
A



# 変数は、演算に使える
r  <- 170
pi <- 3.14
enshu <- 2 * r * pi
enshu
( menseki <-  r * r * pi )


# 変数を使うと、何度も使えるプログラムを作れる
# 半径17の場合でも、最初の行だけ変えればよい
r  <- 17       
pi <- 3.14
enshu <- 2 * r * pi
enshu
( menseki <- r  * r * pi)


# 少し整えて....
r <- 5000
pi <- 3.14
enshu <- 2 * r  * pi
cat("円周は、", enshu,"　です。")
menseki <-  r  * r * pi
cat("円の面積は、", menseki,"　です。")



# 元の値を使った代入　
i <- 5
i

i <- i + 3
i

i  <- 1
( i <- i + 1 )
( i <- i + 1 )
( i <- i + 1 )
( i <- i + 1 )


# ベクトル
hara <-  c( -4, 0, 2, 6, 13 )
hara

hara[4]
hara[1:3]



# マトリクス
take <- matrix( c(
11,12,13,14,
21,22,23,24,
31,32,33,34,
41,42,43,44,
51,52,53,54
), ncol=4, byrow=TRUE)

# データフレーム
o1 <- matrix( c(
1, 78.3, 185.3,
2, 45.1, 159.5,
3, 66.5, 168.5,
4, 82.4, 177.5,
5, 50.1,  163.4
), ncol=3, byrow=TRUE)

o2 <- c("男","女","男","男","女")
oketa <- data.frame(o1,o2)
oketa

( oketa[2,2] + oketa[5,2] ) / 2
oketa[3,4]
mean(oketa[,2])


# データフレームいろいろ
colnames(oketa)
colnames(oketa) <- c("id","weight","height","sex")
colnames(oketa)
rownames(oketa) <- c("Toyota","Honda","Nissan","Mazda","Mitsubishi")
oketa

oketa$height
oketa[,3]
mean(oketa$weight)
mean(oketa[,2])
table(oketa$sex)
table(oketa[,4])



# 因子（factor）型変数とラベル付集計
sex <- c(
1,2,1,2,2,1,1,2,1,2,2,
1,1,2,2,1,2,1,1,2,2,2,
1,2,1,1,1,1,1,2,1,2,2,
1,1,1)
sex
table(sex)

sex2 <- factor(sex, levels=c(1,2), labels=c("男","女"))
sex2
table(sex2)

plot( table(sex2) )
barplot(table(sex2))

	#内訳図
	barplot( as.matrix( table(sex2)*100/sum(table(sex2)) ), horiz=T, legend=levels(sex2) )


# ちょっと練習（その１）　-　business.csv 読込み
biz <- read.table("business.csv", header=T, sep=",", nrows=-1)
biz
colnames(biz)
rownames(biz)


biz <- read.table("business.csv", header=T, sep=",", nrows=-1, row.name=1)
biz
colnames(biz)
rownames(biz)



# ループ　---------------------------------------------------------------
# ベクトルの作成
hara <- c(-4,0,2,6,13)
hara

# ベクトルの要素を指定して書かせる
hara[1]

# ベクトルの要素を指定には、変数も使える
i <- 1
cat( hara[i] )

# 次々と指定すればいいけど、面倒くさい
i <- 2 ;  cat( hara[i] )
i <- 3 ;  cat( hara[i] )
i <- 4 ;  cat( hara[i] )
i <- 5 ;  cat( hara[i] )

# これでも、面倒くさいことは変わらない
i <- 1 ;  cat( hara[i] )
i <- i + 1 ;  cat( hara[i] )
i <- i + 1 ;  cat( hara[i] )
i <- i + 1 ;  cat( hara[i] )
i <- i + 1 ;  cat( hara[i] )
i <- i + 1 ;  cat( hara[i] )


# で、ループで書く
for (i in 1:5) {
	cat( hara[i], "\n" )
	}

# ちょっと変形
for (i in 1:length(hara)) {
	cat( "hara[", i, "]=", hara[i], "\n" )
	}


# ２重ループ（ループの入れ子）
hara <- matrix( nr=5, nc=4 )
hara

for ( i in 1:5) {
	for ( j in 1:4 ) {
		hara[ i, j ] <- i * j
		}
	}

hara


# ちょっと練習（その２）
# 分散や標準偏差を自分で計算する
	ss <- 0
	xbar <- mean(height)
	n <- length(height)
	
	for ( i in 1:n ) {
   			ss <- ss + ( height[i] - xbar )^2
	}

   v <- ss/(n-1) ; cat("分散：　", v,"\n")
   s <- sqrt(v)    ; cat("標準偏差：　", s,"\n")



# 条件分岐  if　---------------------------------------------------------------
c.income <- NA
income <- 800

if ( income > 750 ) {
	c.income <- "高収入"
	}
c.income
	
c.income <- NA
income <- 500

if ( income > 750 ) {
	c.income <- "高収入"
	}
c.income	


	
# 条件分岐  if 〜 else 〜
income <- 800
if ( income > 750 ) {
    c.income <- "高収入"
	} else {
    c.income <- "低収入"	
}
c.income


income <- 500
if ( income > 750 ) {
    c.income <- "高収入"
	} else {
    c.income <- "低収入"	
}
c.income


# 条件分岐  if 〜 else if 〜 else 〜
income <- 270
if ( income > 750 ) {
    c.income <- "高収入"
	} else if ( income > 450 )  {
    c.income <- "中収入"	
     } else {
    c.income <- "低収入"	     	
}
c.income



# 関数の作成　---------------------------------------------------------------
catinc  <- function( income ) {
	
	             if ( income > 750 ) {
                      c.income <- "高収入"
	             } else if ( income > 450 )  {
                      c.income <- "中収入"	
                  } else {
                      c.income <- "低収入"
                  }                  
         cat(c.income)
}

# 関数の利用
catinc(270)
catinc(620)
catinc(1100)


# ちょっと練習（その３）
# 円の面積と円周も関数にしてしまいましょう。
en <- function(r) {
	pi <- 3.14
	enshu <- 2 * r  * pi
	cat("円周は、", enshu,"　です。\n")
	menseki <-  r  * r * pi
	cat("円の面積は、", menseki,"　です。")
}

en(15)
en(23)



# 入出力　---------------------------------------------------------------
# CSVファイルの読み込み
biz <- read.table("business.csv", header=T, sep=",", nrows=-1, row.name=1)
biz
colnames(biz)
rownames(biz)

nrow(biz); ncol(biz)

biz


# CSVファイルの書き出し
write.csv(biz, file="biz.csv")


# Rdata への書き出し
save(biz, file="biz.Rdata")

rm(biz)
biz

# Rdata の読み込み
load("biz.Rdata")

biz

# 作業ディレクトリ
getwd()
dir()
setwd("【ディレクトリパス】")


# 練習（ご自分でがんばれますか？）


# しっかり演習
# データの加工　---------------------------------------------------------------
# business.csvの読み込み
biz <- read.csv("business.csv", header=T, sep=",", nrows=-1)
colnames(biz)
biz

# 変数の合成
biz$COSTS <- biz$SALES - biz$PROFITS
biz$EFFIC <- biz$PROFITS / biz$EMPLOYS
biz

# 変数の合成（別のやり方)
colnames(biz)
biz[, 7] <- biz[, 5] - biz[, 6]
biz[, 8] <- biz[, 6] / biz[, 4]
colnames(biz)[7] <- "COSTS"
colnames(biz)[8] <- "EFFIC"
biz


# 条件式を用いた変数合成
biz <- read.csv("business.csv", header=T, sep=",", nrows=-1)

for ( i in 1:nrow(biz)) {

	if ( is.na(biz[i,6]) ) {
			biz[i,7] <- NA
		} else if ( biz[i,6] > 0 ) {
				biz[i,7] <- "black"
			} else if ( biz[i,6] == 0 ) {
					biz[i,7] <- "white"
				} else {
						biz[i,7] <- "red"
			}
			
}

colnames(biz)[7] <- "BALANCES"
biz


# 条件式を用いた変数合成（別の方法)
biz <- read.csv("business.csv", header=T, sep=",", nrows=-1)
biz$BALANCES <- ifelse(biz$PROFITS > 0, "black", ifelse(biz$PROFITS == 0, "white", "red") )
biz


# 欠測値のないデータを抽出する
biz2 <- biz[ complete.cases(biz), ]
nrow(biz)
nrow(biz2)

biz2


# はずれ値の処理（NAとする)
boxplot(biz$PROFITS, range=5.7, horizontal=T,col="bisque", frame=T)

for ( i in 1:nrow(biz)) {

	if ( !(is.na(biz[i,6])) ) {		
			if ( biz[i,6] < -2000 || 4000 < biz[i,6] )  biz[i,6] <- NA
	}
			
}

biz


# はずれ値の処理（NAとする)　別法
biz$PROFITS <- ifelse(biz$PROFITS < -2000 | 4000 < biz$PROFITS, NA , biz$PROFITS )
biz



# はずれ値の処理（消去する)
boxplot(biz$PROFITS, range=5.7, horizontal=T,col="bisque", frame=T)

biz <- subset( biz, -2000 <= biz$PROFITS & biz$PROFITS <= 4000 )

nrow(biz)
biz



# Ｒ言語の機能を拡張する（その１）
# 公開されている関数の利用：青木先生（群馬大）による all.R
source("all.R", encoding="EUC-JP")

   # インターネットが利用可能ならば、こうすればいつでも最新版
   # source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")


# all.R により、エクセルの関数名が使える
average(height)

stdev(height)

stdevp(height)
varp(height)


# 度数分布
dosuu.bunpu(height,5)
dosuu.bunpu(height,15)
dosuu.bunpu(height,25)


# レーダーチャート
test <- swiss[1:5,]   # swiss データセットの最初の5件
test
radar(test, col=c("black", "red", "green4", "blue", "brown"))


# 星座グラフ
data(iris)	 # Fisher の iris data
col <- c("blueviolet", "chocolate3", "darkcyan")[as.integer(iris$Species)]
Constellation.graph(iris[1:4], col=col, main="iris data")


# 練習用データ（data_lsce2）の読み込み） ------------------------------------------------
lsce <- read.table("data_lsce2.csv", header=T, sep=",", nrows=-1)

head(lsce)

ncol(lsce)
nrow(lsce)
colnames(lsce)


# 課題の処理例
# 課題１
hist(lsce$q17, col="green", main="自由裁量所得額")
# 課題２
boxplot(lsce$q17, main="自由裁量所得額", horizontal=T,col="bisque")
# 課題３
round( mean(lsce$q17, na.rm=T), 2 )
round( sd(lsce$q17, na.rm=T), 2 )
round( var(lsce$q17, na.rm=T), 2 )
