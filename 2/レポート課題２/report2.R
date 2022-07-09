#学生数：29
gender_nit <- c(
  Male=22, Female=7
)
#pie(gender_nit, main="性別", col=c("Blue", "Red"))


twitter <- c(
  250, 250, 50, 250, 250, 650, 650, 250, 250, 1000, 50, 250, 50, 650, 50, 50, 50,
  650, 50, 250, 250, 900, 250, 1000, 250, 50, 250, 650, 50
)
twitter_nit <- c(
  9, 12, 5, 1, 2
)
#mean(twitter) #= 331
#boxplot(twitter, horizontal=T, col="bisque")


grade <- c(
  2, 1, 2, 2, 4, 1, 1, 1, 2, 3, 5.5, 4, 1, 2, 2, 5, 3, 3, 2, 2, 2, 2, 1, 2, 2, 
  5.5, 3, 2, 2
)
grade_nit <- c(
  20, 4, 2, 1, 2
)
#mean(grade) #= 2.414


morning <- c(
  1, 1, 1, 2, 2, 2, 1, 3, 1, 1, 2, 2, 1, 2, 2, 2, 3, 1, 1, 1, 2, 1, 1, 3, 1, 1,
  2, 2, 2
)
morning_nit <- c(
  14, 12, 3
)
#mean(morning) #= 1.621


gpa <- c(
  2.5, 2.5, 2.5, 3.5, 2.5, 2.5, 2.5, 2.5, 3.5, 2.5, 3.5, 2.5, 3.5, 3.5, 3.5, 3.5,
  3.5, 2.5, 1.5, 3.5, 2.5, 1.5, 3.5, 2.5, 2.5, 3.5, 3.5, 3.5, 1.5
)
gpa_nit <- c(
  3, 13, 13
)
#mean(gpa) # = 2.845
#hist(gpa, breaks=seq(1, 4), ylim=c(0, 14), main="GPA", xlab="GPAの範囲の中央値", ylab="生徒数")


seminar_nit <- c(
  2, 6, 2, 1, 4, 1, 1, 12
)


hobby_nit <- c(
  12, 3, 4, 5, 4, 1, 1
)


programming <- c(
  2, 3, 3, 2, 2, 2, 2, 2, 2, 4, 2, 3, 2, 2, 3, 1, 2, 2, 2, 2, 1, 2, 3, 3, 2, 2, 
  3, 2, 3
)
programming_nit <- c(
  2, 18, 8, 1
)


career_nit <- c(
  8, 13, 8
)


examtype_nit <- c(
  7, 8, 5, 2, 3, 2, 2
)


circle_nit <- c(
  7, 12, 1, 9
)


vehicle_nit <- c(
  17, 5, 6, 1
)


comschool <- c(
  90, 20, 180, 45, 90, 20, 5, 90, 5, 150, 45, 150, 90, 90, 5, 90, 45, 90, 90, 90,
  90, 45, 90, 45, 90, 180, 150, 90, 20
)
comschool_nit <- c(
  11, 13, 3, 2
)
#hist(comschool, main="通学時間", ylim=c(0, 14), xlab="通学にかかる時間", ylab="生徒数")


nit <- function(n) {
  v <- 2*n*log(n); if (is.nan(v)) v <- 0
  return(as.numeric(v))
}