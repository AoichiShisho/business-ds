approxcurve <- function() {
  #三次関数の近似曲線

  #ここにデータを入力、読み込みでも可
  x<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y<-c(10, 20, 24, 6, 21, 14, 5, 19, 25, 3)
  model = (y~x)
  
  #startに初期値を入力する
  result <- nls(y~(a*x^3+b*x^2+c*x+d), start=c(a=0, b=0, c=0, d=10))

  summary(result)

  #predict関数を使用して予想値を算出する
  predict.c <- predict(result)

  plot(model)
  #次の作画を今の作画の上に描画
  par(new=T)
  plot(x, predict.c, type="l")
}