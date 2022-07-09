# ビジネスのためのデータサイエンス / データマイニング（第４週）
#                     by  T. Kuwahara,  Keio University at SFC
#                                kuwahara@sfc.keio.ac.jp


# 作業ディレクトリの設定（自分の環境にあわせて修正して下さい。　レポート：大学イメージの分析の時に必要になるかな。
# もちろんメニューから変更・設定しても良い
setwd("~/Documents/Lecture/datapgm")

# 青木先生（群馬大）による関数の読み込み（インターネット接続が必要）
source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")
       # 青木先生（群馬大）による関数の読み込み（all.Rをダウンロードして用いる場合）
            # ディレクトリパスは、自分の環境にあわせて修正して下さい）
　　　　#  source("./all.R", encoding="EUC-JP")


# 相関比 --------------------------------------------------------------------------

# 青木繁伸先生による相関比を計算する関数（以下を実行するか、all.Rを読み込む、この上）
# 相関比と決定係数を求める
#             引数：　x         データベクトル
#                         group  観察値がどの群に属するかを表す変数

correlation.ratio <- function(       x,                                              # 変数ベクトル
                                group)                                          # 群を表す変数ベクトル
{
        ok <- complete.cases(x, group)                                               # 欠損値を持つケースを除く
        x <- x[ok]
        group <- factor(group[ok])
        n.i <- tapply(x, group, length)                                              # 各群のデータの個数
        n <- sum(n.i)                                                                # 全データ数
        v.i <- tapply(x, group, var)                                         # 各群の不偏分散
        R.sq <- 1-sum((n.i-1)*v.i)/var(x)/(n-1)                                      # 決定係数
        c.r <- sqrt(R.sq)                                                    # 相関比
        return(c("correlation ratio"=c.r, "coefficient of determination(R^2)"=R.sq))
}



# 相関比の計算：購入金額を来店時間帯で説明する  --------

# 量販店：来店時間帯 * 購入金額データ
raiten <- matrix( c( 
 1,  6800,
 1,  7600,
 1,  7200,
 1,  7400,
 1,  7000,
 2,  8800,
 2,  9200,
 2,  8600,
 2,  9600,
 3,  7600,
 3,  9800,
 3,  8400
 ),  ncol=2, byrow=TRUE)  
 
correlation.ratio(raiten[,2], raiten[,1])


# 来店時間帯別　箱ひげ図
colnames(raiten) <- c( "来店時間帯", "購入金額")
raiten <- as.data.frame(raiten)
raiten[,1]  <- factor( raiten[,1], levels=c(1:3), labels=c("午前","午後","深夜"))
boxplot( 購入金額 ~ 来店時間帯, data=raiten, main ="来店時間帯 * 購入金額", 
         xlab ="来店時間帯", ylab="購入金額", col="bisque")




# 実習：チョコレート・データ（度数表）の解析　（双対尺度法、コレスポンデンス分析） -----

# 課題：チョコレート・データ（度数表）のヨミコミ
 # SFC-SFSでダウンロードしたファイルを使う場合 
cfreq <- as.matrix( read.table("chocofreq.csv", header=FALSE, sep=",", nrows=-1) )
# ネットから読み込む場合（こっちの方が簡単）
cfreq <- as.matrix( read.table( "http////web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/chocofreq.csv"
                   , header=FALSE, sep=",", nrows=-1) )                     

# ラベルづけ
colnames(cfreq) <-c("値ごろ", "味", "カロリー", "カカオ", "新製品", "ブランド", "パッケージ", "メーカー","広告", "評判")
rownames(cfreq) <- c("明治ミルク",  "ブラック",  "効果",  "ライフ",  "ノワール",  "森永ミルク",  "カレ・ド",  "ダース",  "刻みカカオ",  "アッセ",  "ガーナ",  "紗々",  "カカオの恵み",  "シャルロッテ",  "エアーズ",  "ＧＡＢＡ",  "アロマーモ",  "バンホーテン",  "エアロ")

cfreq

# コレスポンデンス分析（双対尺度法）の実行
ans <- dual(cfreq)
summary(ans)
summary(ans, weighted=TRUE)
plot(ans,1,2)


# コレスポンデンス分析には、関数 ca もスグレモノ。　ただし、パッケージ ca と rgl をインストールする必要あり。
# install.packages("ca", dependencies=TRUE)
library(ca)
ca(cfreq)
plot(ca(cfreq))


