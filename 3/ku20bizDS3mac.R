# ビジネスのためのデータサイエンス / データマイニング（第３週）
#                     by  T. Kuwahara,  Keio University at SFC
#                                kuwahara@sfc.keio.ac.jp


# 作業ディレクトリの設定（自分の環境にあわせて修正して下さい）
# setwd("/Users/kuwahara/Google ドライブ/2. course/授業")

# 青木先生（群馬大）による関数の読み込み（ディレクトリパスは、自分の環境にあわせて修正して下さい）
# インターネットが利用可能ならば、こうすればいつでも最新版
source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")
            # あらかじめ all.R をダウンロードしておけば、ネット環境がないところでもOK
            # source("./all.R", encoding="EUC-JP")

# 本講座用関数集の読み込み（インターネット環境が必要です）
source("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/kuwahara18_Rlib.R",  encoding="EUC-JP")


#-------------------------------------------------------------------------
# 潜在情報量を計算する関数（kuwahara18_Rlib.Rにも入っている）
# nit : function - the amount of information (if n=0 then nit=0 )
nit <- function(n) {
	v <- 2*n*log(n); if (is.nan(v))  v <- 0
	return(as.numeric(v))
	}

# kusi：特化係数（kuwahara18_Rlib.Rにも入っている）
kusi <- function(D) invisible(as.matrix( D / (rowSums(D) %*% t(colSums(D))/sum(D)) ))

# kuI：顕在化された情報量（引数：ベクトル）（kuwahara18_Rlib.Rにも入っている）
kuI <- function(v) invisible(as.numeric(nit(sum(v)) - sum(sapply(v,nit))))

# 関連情報量 kuIAxB（kuwahara18_Rlib.Rにも入っている）
kuIAxB <- function(d) invisible(as.numeric(nit(sum(d)) - sum(sapply(rowSums(d),nit)) - sum(sapply(colSums(d),nit)) + sum(sapply(d,nit)) ))

#-------------------------------------------------------------------------

# 情報量（復習）
# log2(x)
# H = n*log2(k)

# 潜在情報量 bit 　　H = n*log2(n)
n <- 8 ;  H <- n * log2(n) ; cat(H)

# 潜在情報量 nit 　　H = 2* n * loge(n)
n <- 8 ;  H <- 2 * n * log(n) ; cat(H)


# 分割すると情報は顕在化する（復習）
# 尺度A
nit(25)  # 全体
nit(4)   # 絶望
nit(19)  # ふつう
nit(2)   # 至高
nit(25) - ( nit(4) + nit(19) + nit(2) )   # 顕在化した情報量

# 尺度B
nit(25)  # 全体
nit(9)   # 不幸
nit(12)  # どちらとも言えない
nit(4)   # 幸せ
nit(25) - ( nit(9) + nit(12) + nit(4) )   # 顕在化した情報量


# 関連情報量への道（ちょっと長い）
##########################################################################
# (0) 　A:県   B:食品      IAlB: Bで区分、Aで細分した場合の顕在情報量
##########################################################################
( T <- 1940 )
( TB <- c(678, 423, 839) )
( T <- sum(TB) ) 

# 食品で区分→県で細分後の表
( TBA <- matrix(c(229,  99, 302,
                  221, 101, 325,
                  228, 223, 212), byrow=T, ncol=3) )

( T <- sum( TBA ) )
( TB <- colSums( TBA ) )

( H0 <- nit(1940) )
( H0 <- nit( T ) )   #h0
( H0 <- nit( sum(Tb)) )
( H0 <- nit( sum( Tba )) )

( HB <- sum( sapply(TB,nit)) )    # この書き方は当面わからなくてよい
( HB <- nit(678) + nit(423) + nit(839) )　# こっちで十分

( IB <- H0 - HB )
( IB <- nit( 1940 ) - ( nit(678) + nit(423) + nit(839) ) )


# 顕在情報量
# 肉部門　県で細分化
( 肉IA <- nit(229+221+228) - ( nit(229)+nit(221)+nit(228) ) )
( 魚IA <- nit( 99+101+223) - ( nit( 99)+nit(101)+nit(223) ) )
( 菜IA <- nit(302+325+212) - ( nit(302)+nit(325)+nit(212) ) )

( IAlB <- 肉IA + 魚IA + 菜IA )
# ( IAlB <- HB - HBA )


##########################################################################
#（１）　顕在情報量の性質　 「構成比が同じであれば」に注意を払っておこう
##########################################################################
# N=630
( IB <- nit(630) - ( nit(229) + nit( 99) + nit(302) ) )

# N=3150 （５倍）
(IB <- nit(630*5) - ( nit(229*5) + nit( 99*5) + nit(302*5) ) )
IB / 5

# N=3150 （10倍）
(IB <- nit(630*10) - ( nit(229*10) + nit( 99*10) + nit(302*10) ) )
IB / 10

# N=1890 と N=4410 （３倍と７倍）
(IB_1 <- nit(630*3) - ( nit(229*3) + nit( 99*3) + nit(302*3) ) ) 
(IB_2 <- nit(630*7) - ( nit(229*7) + nit( 99*7) + nit(302*7) ) )
( IB <- IB_1 + IB_2 )
IB / ( 3 + 7 )



##########################################################################
#（２）　食品・県で一挙に区分したときの顕在情報量  I食品・県（Iab）
##########################################################################
# 一挙区分後の表
( TBA <- matrix(c(229,  99, 302,
                  221, 101, 325,
                  228, 223, 212), byrow=T, ncol=3) )

# 一挙区分後の潜在情報量 Hba （Habと書いても同じ）
( HBA <- sum( sapply(TBA,nit)) )  # この書き方は分からなくて良い

( HBA <- nit(229) + nit( 99) + nit(302)  # こっちで十分
       + nit(221) + nit(101) + nit(325)
       + nit(228) + nit(223) + nit(212) )


# 顕在情報量（一挙区分による） I食品・県（ = I県・食品 ） Iab=Iba
( IBA <- IAB <- H0 - HBA )

( IBA <- IAB <- nit(1940)  - (   nit(229) + nit( 99) + nit(302)
                               + nit(221) + nit(101) + nit(325)
                               + nit(228) + nit(223) + nit(212) )   )



##########################################################################
#（３）　いよいよ関連情報量　I県X食品（IAxB）= I食品X県（IBxA）
##########################################################################
# I県X食品 ＝ I県  -  I県 |食品
# I県（IA）
( IA <- nit(1940) - ( nit(630) + nit(647) + nit(663) ) )
# I県 |食品
      肉IA <- nit(678) - ( nit(229)+nit(221)+nit(228) )  # 肉部門
      魚IA <- nit(423) - ( nit( 99)+nit(101)+nit(223) )  # 魚部門
      菜IA <- nit(839) - ( nit(302)+nit(325)+nit(212) )  # 菜部門
( IAlB <- 肉IA + 魚IA + 菜IA )
# 関連情報量　I県X食品（IAxB)
( IAxB <- IA - IAlB )


# I食品X県 ＝ 食品 - I食品|県
# I食品（IB）
( IB <- nit(1940) - ( nit(678) + nit(423) + nit(839) ) )
# I食品|県
      長野IB <- nit(630) - ( nit(229)+nit( 99)+nit(302) )  # 長野部門
      岐阜IB <- nit(647) - ( nit(221)+nit(101)+nit(325) )  # 岐阜部門
      佐賀IB <- nit(663) - ( nit(228)+nit(223)+nit(212) )  # 佐賀部門
( IBlA <- 長野IB + 岐阜IB + 佐賀IB )
# 関連情報量　I県X食品（IAxB)
( IBxA <- IB - IBlA )


# 内訳図
TBA <- matrix(c(229,  99, 302,
                221, 101, 325,
                228, 223, 212), byrow=T, ncol=3) 
tbl <-d <- TBA
tbl[1,] <- d[3,]; tbl[3,] <- d[1,]
colnames(tbl) <- c("肉", "魚", "野菜")
rownames(tbl) <- c("佐賀","岐阜", "長野")
barplot(t(tbl*100/rowSums(tbl)), horiz=T, legend=TRUE)

# χ2値　　いろんなやり方があるが、下の chisq() 関数は、青木先生の関数集が必要
source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")
chisq(TBA)

##########################################################################
#（４）構成比が同じ場合の関連情報量　I県X食品（IAxB）= I食品X県（IBxA）
##########################################################################
# I県X食品 ＝ I県  -  I県 |食品
# I県（IA）
( IA <- nit(1940) - ( nit(630) + nit(647) + nit(663) ) )
# I県 |食品
      肉IA <- nit(678) - ( nit(220.175)+nit(226.117)+nit(231.708) )  # 肉部門
      魚IA <- nit(423) - ( nit(137.366)+nit(141.073)+nit(144.561) )  # 魚部門
      菜IA <- nit(839) - ( nit(272.459)+nit(279.811)+nit(286.730) )  # 菜部門
( IAlB <- 肉IA + 魚IA + 菜IA )
# 関連情報量　I県X食品（IAxB)
( IAxB <- IA - IAlB )
round( IAxB, 4)


# I食品X県 ＝ 食品 - I食品|県
# I食品（IB）
( IB <- nit(1940) - ( nit(678) + nit(423) + nit(839) ) )
# I食品|県
長野IB <- nit(630) - ( nit(220.175)+nit(137.366)+nit(272.459) )  # 長野部門
岐阜IB <- nit(647) - ( nit(226.117)+nit(141.073)+nit(279.811) )  # 岐阜部門
佐賀IB <- nit(663) - ( nit(231.708)+nit(144.561)+nit(286.730) )  # 佐賀部門
( IBlA <- 長野IB + 岐阜IB + 佐賀IB )
# 関連情報量　I県X食品（IAxB)
( IBxA <- IB - IBlA )
round( IBxA, 4)


# 内訳図
TBA <- matrix(c(220.175, 137.366, 272.459,
                226.117, 141.073, 279.811,
                231.708, 144.561, 286.730), byrow=T, ncol=3) 
tbl <-d <- TBA
tbl[1,] <- d[3,]; tbl[3,] <- d[1,]
colnames(tbl) <- c("肉", "魚", "野菜")
rownames(tbl) <- c("佐賀","岐阜", "長野")
barplot(t(tbl*100/rowSums(tbl)), horiz=T, legend=TRUE)




# ******************
#  本講座用関数集
# *******************
# 本講座用関数集の読み込み（インターネット環境が必要です）
source("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/kuwahara18_Rlib.R",  encoding="EUC-JP")

d <- matrix(c(229,  99, 302,
              221, 101, 325,
              228, 223, 212), byrow=T, ncol=3) 

# nit
n <- 1940; nit( n )

# kuI
v <- c( 678, 423, 839); cat( kuI( v ) )

# kuIA, kuIB
cat( kuIA(d) )
cat( kuIB(d) )

# kuIAxB
cat( kuIAxB(d) )





# 関連情報量の計算（関数:nitが必要）
# 関連情報量：IAxB = IA - IA|B
# 分割すると、情報は顕在化する
nit(700)
nit(517)
nit(183)
nit(700) - ( nit(517) + nit(183) )


# 教育による再分割（債務不履行の有無 * 学歴）
d <- matrix(c(293,79,224,104), ncol=2)
d
 H0 <- nit(sum(d))
 HA <- sum(sapply(rowSums(d),nit))
 IA <- H0 - HA
 IAlB <- sum(sapply(colSums(d),nit)) - sum(sapply(d,nit))
 ( IAxB <- IA - IAlB )

# 内訳図
tbl <-d
tbl[1,] <- d[2,]; tbl[2,] <- d[1,]
barplot(t(tbl*100/rowSums(tbl)), horiz=T, names=c("高校以上","中学"))


# 負債比による分割
d <- matrix(c(403,114,74,109), ncol=2)
d 
 H0 <- nit(sum(d))
 HA <- sum(sapply(rowSums(d),nit))
 IA <- H0 - HA
 IAlB <- sum(sapply(colSums(d),nit)) - sum(sapply(d,nit))
 ( IAxB <- IA - IAlB )

# 内訳図
tbl <-d
tbl[1,] <- d[2,]; tbl[2,] <- d[1,]
barplot(t(tbl*100/rowSums(tbl)), horiz=T, names=c("12.45以上","12.45未満"))

 
# 性別による分割
d <- matrix(c(295,222,105,78), ncol=2)
d 
 H0 <- nit(sum(d))
 HA <- sum(sapply(rowSums(d),nit))
 IA <- H0 - HA
 IAlB <- sum(sapply(colSums(d),nit)) - sum(sapply(d,nit))
 ( IAxB <- IA - IAlB )
 
# 内訳図
tbl <-d
tbl[1,] <- d[2,]; tbl[2,] <- d[1,]
barplot(t(tbl*100/rowSums(tbl)), horiz=T, names=c("女性","男性"))



# 階層的方法によるクラスタリング(2)
# 情報量分析による分割表・度数表のクラスタリング
# ３県（長野、岐阜、佐賀）の食品摂取データ
food <- matrix( c( 229,  99, 302,
                   221, 101, 325,
                   228, 223, 212 ), byrow=T, ncol=3)

colnames(food) = c( "肉", "魚", "野菜")
rownames(food) = c( "長野", "岐阜", "佐賀")
food

colSums( food )

# どの県とどの県をまとめるべきか......?
( H0 <- nit(1940) )                         # H0:潜在情報量
( HB <- nit(678) + nit(423) + nit(839) )    # HB:食品(B)で区分後の潜在情報量
    sum( sapply( colSums(food), nit ) )                         # こんな書き方もアリ
( IB <- H0 - HB )                           # IB:食品(B)による区分で、顕在化した情報量

( IAlB <-   nit(229+99+302)   - ( nit(229)+nit(99)+nit(302)  )    # IAlB:
          + nit(221+101+325)  - ( nit(221)+nit(101)+nit(325) )    # 食品(B)で区分し、
          + nit(228+223+212)  - ( nit(228)+nit(223)+nit(212) ) )  # 県(A)による細分で顕在化した情報量

( IAxB <- IB - IAlB )                        # IAxB:関連情報量（県と食品の関連情報量）

# 上の数行の計算で、いくつか関数を壊してしまっているので、もう一度本講座用関数をロードしなおす。
source("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/kuwahara18_Rlib.R",  encoding="EUC-JP")

# 関数 kuIAxB を使えば、関連情報量がもとめられます。
cat( kuIAxB(food) )



# 長野と岐阜をまとめるということは、 ---
# 合併前
food0 <- matrix( c( 229,  99, 302,
                    221, 101, 325,
                    228, 223, 212 ), byrow=T, ncol=3 )
cat( kuIAxB(food0))

#合併後
food1 <- matrix( c( 450, 200, 627,
                    228, 223, 212 ), byrow=T, ncol=3 )
cat( kuIAxB(food1))

# 合併による情報ロス
( ΔI = kuIAxB(food0) - kuIAxB(food1) )


# 長野と佐賀をまとめるということは、 ---
# 合併前
food0 <- matrix( c( 229,  99, 302,
                    228, 223, 212,
                    221, 101, 325 ), byrow=T, ncol=3 )
cat( kuIAxB(food0))

#合併後
food1 <- matrix( c( 457, 322, 514,
                    221, 101, 325 ), byrow=T, ncol=3 )
cat( kuIAxB(food1))

# 合併による情報ロス
( ΔI = kuIAxB(food0) - kuIAxB(food1) )


# 岐阜と佐賀をまとめるということは、 ---
# 合併前
food0 <- matrix( c( 221, 101, 325,
                    228, 223, 212,
                    229,  99, 302 ), byrow=T, ncol=3 )
cat( kuIAxB(food0))

#合併後
food1 <- matrix( c( 449, 324, 537,
                    229,  99, 302 ), byrow=T, ncol=3 )
cat( kuIAxB(food1))

# 合併による情報ロス
( ΔI = kuIAxB(food0) - kuIAxB(food1) )




# とても小さな実行例１　上田尚一,1982, データ解析の方法, 朝倉書店 より
d <- matrix(c(70,30,
              60,40,
              50,50,
              90,10), byrow=T, ncol=2)
              
d.aoaoi.ihc <- kuiClust(d)
plot(d.aoaoi.ihc, labels=c("A1","A2","A3","A4"), hang=-1, cex=2)     # デンドログラム



# 実行例２：食品摂取傾向データ　　上田尚一,1982, データ解析の方法, 朝倉書店 より
# food0 <- read.csv("./P142ex3-utf8.csv", header=T, nrows=-1)
# 利用OSにあわせて、以下のどちらかを実行してください。
food0 <- read.csv("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/P142ex3-utf8.csv",  header=T, nrows=-1)  # Macintosh用
food0 <- read.csv("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/P142ex3.csv",  header=T, nrows=-1)　     # Windows用

# こっから全員
food <- as.matrix( food0[,c(3:5)] )
food0

# クラスター分析の実行
food.ihc <- kuiClust(food)

# 結果の表示
food.ihc$tbl     # クラスター形成の経過（単純）
kuiCpr(food.ihc)   # クラスター形成の経過（清書）

plot(food.ihc, labels=food0[,1], hang=-1, 
     main="食品摂取傾向",
     cex=0.6, ylab="潜在情報量")     # デンドログラム1

plot(food.ihc, labels=food0[,1], hang=-1, 
     main="食品摂取傾向",
     cex=0.6, ylab="潜在情報量", xlab="都道府県")     # デンドログラム2


# プロット
# scree
n.clst <- c(length(food.ihc$height):1)
plot(n.clst, food.ihc[[2]], type="b",
     pch=20,ps=5, lwd=0.5,
     xlab="クラスター数", ylab="潜在情報量",main="食品摂取傾向")


# クラスターの特徴チェック
iclst <- cutree(food.ihc, k=7)
iclst

food1 <-cbind(food0, iclst)
food1.sorted <- food1[sort.list(food1$iclst),]
food1.sorted

# クラスターごとの平均値
result <- by(food1[,c(2:5)], food1$iclst, colMeans, na.rm=T)
atable <- result[[1]]
for (j in 2:nrow(result)) {
	atable <- rbind(atable,result[[j]])
	rownames(atable)[j] <- paste("cluster:",j)
};  rownames(atable)[1] <- paste("cluster:",1)

round(atable, 1)

# クラスターごとの特化係数
result <- by(food1[,c(2:5)], food1$iclst, colSums, na.rm=T)
btable <- result[[1]]
for (j in 2:nrow(result)) {
	btable <- rbind(btable,result[[j]])
	rownames(btable)[j] <- paste("cluster:",j)
};  rownames(btable)[1] <- paste("cluster:",1)

btable <- kusi(btable[,2:4])
round(btable, 2)



#########  付録　#############

# クラスター分析（階層的方法）---------------------------
# データの読み込み
ci88  <- read.table("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/ci88cls1d.csv", header=T, sep=",", nrows=-1) # データのみ
ci88m <- read.table("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/ci88cls1m.csv", header=T, sep=",", nrows=-1) # ラベル

ci88d <- dist(ci88, method="euclidean")      # 距離の計算
round(ci88d,2)


# 階層的方法によるクラスタリング(1)
(ci88.hc <- hclust(ci88d, method="single"))     # 最近隣法
(ci88.hc <- hclust(ci88d, method="complete"))   # 最遠隣法
(ci88.hc <- hclust(ci88d, method="average"))    # 群平均法
(ci88.hc <- hclust(ci88d, method="centroid"))   # 重心（セントロイド）法
(ci88.hc <- hclust(ci88d, method="median"))     # メディアン法
(ci88.hc <- hclust(ci88d, method="ward.D2"))       # ウォード法

plot(ci88.hc, labels=ci88m$cn1, hang=-1)     # デンドログラム
plot(ci88.hc, labels=ci88m$cn2, hang=-1)     # デンドログラム

memb <- cutree(ci88.hc, k=4)
memb

ci88a<-cbind(cbind(ci88m$cn1,ci88),memb)
ci88a<-cbind(cbind(ci88m$cn2,ci88),memb)

ci88s <- ci88a[sort.list(ci88a$memb),]
ci88s

result <- by(ci88a[,c(2:22)], ci88a$memb, colMeans, na.rm=T)
# ---  整形 ---------------
for (i in 1:nrow(result)){
	result[i] <- list(round(eval(parse(text=paste(c("result$'", as.character(i),"'"), collapse=""))),2))
}
result


# クラスター分析（階層的方法）---------------------------
help(kmeans)

## k-means  ci data
(cl <- kmeans(ci88, 4))
plot(ci88[,c(1:2)], col = cl$cluster)
points(cl$centers, col = 1:4, pch = 8, cex=4)

