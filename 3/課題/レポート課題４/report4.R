source("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/kuwahara18_Rlib.R", encoding="EUC-JP")

#データ
cfreq <- as.matrix(read.table("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/chocofreq.csv"
                    , header=FALSE, sep=",", nrows=-1))

colnames(cfreq) <- c("値ごろ",     "味",          "カロリー"    , "カカオ",       "新製品"
                   , "ブランド",   "パッケージ",  "メーカー",     "広告",         "評判")
rownames(cfreq) <- c("明治ミルク", "ブラック",    "効果",         "ライフ",       "ノワール"
                   , "森永ミルク", "カレ・ド",    "ダース",       "カカオの恵み", "アッセ"
                   , "ガーナ",     "紗々",        "刻みカカオ",   "シャルロッテ", "エアーズ"
                   , "GABA",       "アロマーモ",  "バンホーテン", "エアロ")

#クラスター形成の経過
cfreq.ihc <- kuiClust(cfreq)
kuiCpr(cfreq.ihc)

#デンドログラム
plot(cfreq.ihc, 
     labels= c("明治ミルク", "ブラック",    "効果",         "ライフ",       "ノワール"
             , "森永ミルク", "カレ・ド",    "ダース",       "カカオの恵み", "アッセ"
             , "ガーナ",     "紗々",        "刻みカカオ",   "シャルロッテ", "エアーズ"
             , "GABA",       "アロマーモ",  "バンホーテン", "エアロ"),
     hang=-1, main="チョコレートブランドの銘柄選択理由",
     cex=0.6, ylab="銘柄選択理由", xlab="ブランド")

#クラスター数の決定
n.clst <- c(length(cfreq.ihc$height):1)
plot(n.clst, cfreq.ihc[[2]], type="b",
     pch=20, ps=5, lwd=0.5,
     xlab="クラスター数", ylab="潜在情報量", main="チョコレートブランドの銘柄選択理由")

#クラスター番号のマージ
iclst <- cutree(cfreq.ihc, k=7)
iclst
cfreq1 <- as.data.frame(cbind(cfreq, iclst))
cfreq1.sorted <- cfreq1[sort.list(cfreq1$iclst),]
cfreq1.sorted

#クラスターごとの平均値
result <- by(cfreq1[,c(2:5)], cfreq1$iclst, colMeans, na.rm=T)
atable <- result[[1]]
for (j in 2:nrow(result)) {
  atable <- rbind(atable, result[[j]])
  rownames(atable)[j] <- paste("cluster:", j)
}; rownames(atable)[1] <- paste("cluster:", 1)
round(atable, 1)

#クラスターごとの特化係数
result <- by(cfreq1[c(0:10)], cfreq1$iclst, colSums, na.rm=T)
btable <- result[[1]]
for (j in 2:nrow(result)) {
  btable <- rbind(btable, result[[j]])
  rownames(btable)[j] <- paste("cluster:", j)
}; rownames(btable)[1] <- paste("cluster:", 1)
btable <- kusi(btable[,0:10])
round(btable, 2)