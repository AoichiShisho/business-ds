source("http://aoki2.si.gunma-u.ac.jp/R/src/all.R", encoding="euc-jp")

faculty <- matrix(c(
  1,  300, 1,  178, 1,  965, 1,  660, 1,  772, 1,  500, 1, 1100, 1,  535, 1, 1141, 1,  260, 1, 1000, 1,  772, 1, 1739,
  2, 2000, 2,  350, 2, 1100, 2,  600, 2,  178, 2,  899, 2,  800, 2,  597, 2,  597, 2,  943, 2,  723, 2,  649, 2,  180, 2,  807,
  3,  492, 3,  800
  ), ncol=2, byrow=TRUE)
correlation.ratio(faculty[,2], faculty[,1])

colnames(faculty) <- c("所属", "交通費")
faculty <- as.data.frame(faculty)
faculty[,1] <- factor(faculty[,1], levels=c(1:3), labels=c("総合政策", "環境情報", "政策メディア"))
boxplot(交通費~所属, data=faculty, main="所属*交通費",
        xlab="所属", ylab="交通費", col="bisque")


transfer <- matrix(c(
  1,  300, 1,  178, 1,  178, 1,  500, 1,  260, 1,  723, 1,  180,
  2, 2000, 2,  350, 2,  597, 2,  492,
  3,  660, 3,  600, 3,  800, 3, 1100, 3,  535, 3,  597, 3, 1141, 3,  943, 3, 1000, 3,  800, 3,  649, 3,  772, 3,  807, 3, 1739,
  4,  965, 4, 1100, 4,  899,
  5,  772
  ), ncol=2, byrow=TRUE)
correlation.ratio(transfer[,2], transfer[,1])

colnames(transfer) <- c("乗り換え", "交通費")
transfer <- as.data.frame(tranfer)
transfer[,1] <- factor(transfer[,1], levels=c(1:5), labels=c("0回", "1回", "2回", "3回", "4回以上"))
boxplot(交通費~乗り換え, data=transfer, main="乗り換え*交通費",
        xlab="乗り換え", ylab="交通費", col="bisque")

lines <- matrix(c(
  1,  300, 1,  178, 1,  178, 1,  500, 1,  260, 1,  723, 1,  180,
  2, 2000, 2,  350, 2,  600, 2,  597, 2,  492, 2,  800,
  3,  660, 3,  772, 3,  800, 3, 1100, 3,  535, 3,  597, 3, 1141, 3,  943, 3, 1000, 3,  649, 3,  772, 3,  807,
  4,  965, 4, 1100, 4,  899, 4, 1739
  ), ncol=2, byrow=TRUE)
correlation.ratio(lines[,2], lines[,1])

colnames(lines) <- c("路線の数", "交通費")
lines <- as.data.frame(lines)
lines[,1] <- factor(lines[,1], levels=c(1:4), labels=c("1本", "2本", "3本", "4本以上"))
boxplot(交通費~路線の数, data=lines, main="路線の数*交通費",
        xlab="路線の数", ylab="交通費", col="bisque")


rent <- matrix(c(
  1,  965, 1,  660, 1, 1100, 1,  899, 1,  800, 1, 1100, 1,  535, 1,  597, 1,  597, 1, 1141, 1,  943, 1, 1000, 1,  649, 1,  807, 1, 1739,
  2,  492,
  3,  178, 3,  350, 3,  500, 3,  260, 3,  800, 3,  180, 3,  772,
  4,  300, 4,  600, 4,  178,
  5, 2000, 5,  772, 5,  723
), ncol=2, byrow=TRUE)
correlation.ratio(rent[,2], rent[,1])

colnames(rent) <- c("下宿の家賃", "交通費")
rent <- as.data.frame(rent)
rent[,1] <- factor(rent[,1], levels=c(1:5), labels=c("実家暮らし", "3~5万円", "5~8万円", "8~10万円", "10万円以上"))
boxplot(交通費~下宿の家賃, data=rent, main="下宿の家賃*交通費",
        xlab="下宿の家賃", ylab="交通費", col="bisque")


walk <- matrix(c(
  1,  300, 1,  350, 1,  772, 1, 1100, 1,  600, 1,  500, 1,  899, 1, 1100, 1, 1141, 1,  492, 1,  943, 1,  260, 1, 1000, 1,  772,
  2, 2000, 2,  965, 2,  660, 2,  535, 2,  597, 2,  597, 2,  723, 2,  800, 2,  649, 2,  180, 2, 1739,
  3,  178, 3,  800,
  4,  178, 4,  807
  ), ncol=2, byrow=TRUE)
correlation.ratio(walk[,2], walk[,1])

colnames(walk) <- c("徒歩での時間", "交通費")
walk <- as.data.frame(walk)
walk[,1] <- factor(walk[,1], levels=c(1:4), labels=c("10分未満", "10~20分", "20~30分", "30~40分"))
boxplot(交通費~徒歩での時間, data=walk, main="徒歩での時間*交通費",
        xlab="徒歩での時間", "交通費", col="bisque")


type <- matrix(c(
  1,  300, 1, 2000, 1,  178, 1,  350, 1,  660, 1,  772, 1, 1100, 1,  600, 1,  178, 1,  500, 1,  800, 1, 1100, 1,  535, 1,  597, 1,  597, 1, 1141, 1,  943, 1,  260, 1,  723, 1, 1000, 1,  772, 1,  807, 1, 1739,
  2,  965, 2,  899, 2,  492, 2,  800, 2,  649, 2,  180
  ), ncol=2, byrow=TRUE)
correlation.ratio(type[,2], type[,1])

colnames(type) <- c("入力方法", "交通費")
type <- as.data.frame(type)
type[,1] <- factor(type[,1], levels=c(1:2), labels=c("フリック入力", "フルキー入力"))
boxplot(交通費~入力方法, data=type, main="入力方法*交通費",
        xlab="入力方法", ylab="交通費", col="bisque")


show <- matrix(c(
  1,  300, 1,  350, 1,  660, 1,  772, 1, 1100, 1,  500, 1,  899, 1,  597, 1, 1141, 1,  492, 1,  943, 1,  260, 1, 1000, 1,  800, 1,  649,
  2, 2000, 2,  178, 2,  965, 2,  600, 2,  597, 2,  180, 2,  772, 2,  807,
  3,  178, 3,  800, 3,  535,
  4, 1100, 4,  723, 4, 1739
  ), ncol=2, byrow=TRUE)
correlation.ratio(show[,2], show[,1])

colnames(show) <- c("大晦日に見る番組", "交通費")
show <- as.data.frame(show)
show[,1] <- factor(show[,1], levels=c(1:4), labels=c("紅白", "ガキ使", "RIJIN", "視聴しない"))
boxplot(交通費~大晦日に見る番組, data=show, main="大晦日に見る番組*交通費",
        xlab="大晦日に見る番組", ylab="交通費", col="bisque")


food <- matrix(c(
  1, 2000, 1,  899, 1, 1000,
  2,  178, 2,  965, 2,  350, 2,  660, 2, 1100, 2,  600, 2,  500, 2,  800, 2, 1100, 2,  535, 2,  597, 2,  597, 2, 1141, 2,  492, 2,  943, 2,  260, 2,  723, 2,  649, 2,  180, 2,  772, 2,  807, 2, 1739,
  3,  300, 3,  772, 3,  178, 3,  800
  ), ncol=2, byrow=TRUE)
correlation.ratio(food[,2], food[,1])

colnames(food) <- c("１ヶ月の食費", "交通費")
food <- as.data.frame(food)
food[,1] <- factor(food[,1], levels=c(1:3), labels=c("1万円以下", "1~5万円", "5万円以上"))
boxplot(交通費~１ヶ月の食費, data=food, main="１ヶ月の食費*交通費",
        xlab="１ヶ月の食費", ylab="交通費", col="bisque")


transfer2 <- matrix(c(
  1,  300, 1,  178, 1,  178, 1,  500, 1,  260, 1,  723, 1,  180,
  2, 2000, 2,  350, 2,  597, 2,  492,
  3,  660, 3,  600, 3,  800, 3, 1100, 3,  535, 3,  597, 3, 1141, 3,  943, 3, 1000, 3,  800, 3,  649, 3,  772, 3,  807, 3, 1739,
  4,  965, 4, 1100, 4,  899, 4,  772
), ncol=2, byrow=TRUE)
correlation.ratio(transfer2[,2], transfer2[,1])

colnames(transfer2) <- c("乗り換え", "交通費")
transfer2 <- as.data.frame(tranfer2)
transfer2[,1] <- factor(transfer2[,1], levels=c(1:4), labels=c("0回", "1回", "2回", "3回以上"))
boxplot(交通費~乗り換え, data=transfer2, main="乗り換え*交通費",
           xlab="乗り換え", ylab="交通費", col="bisque")