source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")

setwd("~/Desktop/授業/１年　秋学期/ビジネスのためのデータサイエンス/4/課題/レポート課題6")
univ <- as.matrix(read.table("BJdata_univ.csv", header=FALSE, sep=",", nrows=-1))



# ラベルづけ
colnames(univ) <-c("知っている", "興味がある", "好きである・気に入っている", "なくなると寂しい", "共感する・フィーリングが合う", "親しみを感じる", 
                    "品質が優れている", "最近使っている", "役に立つ", "他にはない魅力がある", "際立った個性がある",
                    "ステータスが高い", "かっこいい・スタイリッシュ", "時代を切り開いている", "勢いがある", "今注目されている")
rownames(univ) <- c("京都大学", "慶應義塾大学", "東京大学", "一橋大学", "早稲田大学")

univ

# コレスポンデンス分析（双対尺度法）の実行
ans <- dual(univ)
summary(ans)
summary(ans, weighted=TRUE)
plot(ans,1,2)


# コレスポンデンス分析には、関数 ca もスグレモノ。　ただし、パッケージ ca と rgl をインストールする必要あり。
install.packages("ca", dependencies=TRUE)
library(ca)
ca(univ)
plot(ca(univ))