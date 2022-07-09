source("http://web.sfc.keio.ac.jp/~kuwahara/courses/bizDS/kuwahara18_Rlib.R", encoding="EUC-JP")

faculty <- matrix(c(1, 3, 0,
                    6, 7, 0,
                    8, 7, 1 ), byrow=T, ncol=3, nrow=3)

food <- matrix(c(2, 1, 1,
                 7, 4, 2,
                 9, 4, 3 ), byrow=T, ncol=3, nrow=3)

os <- matrix(c( 3, 1,
                11, 2,
                14, 2), byrow=T, ncol=2, nrow=3)

prefecture <- matrix(c( 2, 1, 0, 0, 1,
                       10, 2, 0, 0, 1,
                        6, 1, 1, 1, 7), byrow=T, ncol=5, nrow=3)

newyear <- matrix(c( 2, 0, 2, 0,
                    11, 2, 0, 0,
                    11, 2, 1, 2), byrow=T, ncol=4, nrow=3)

language <- matrix(c( 2, 0, 0, 1, 0, 0, 1,
                      6, 0, 3, 0, 3, 1, 0,
                     10, 2, 3, 0, 0, 1, 0), byrow=T, ncol=7, nrow=3)

type <- matrix(c(1,  3,
                 1, 12,
                 5, 11), byrow=T, ncol=2, nrow=3)

show <- matrix(c(2, 2, 0, 0,
                 5, 7, 1, 0,
                 7, 4, 3, 2), byrow=T, ncol=4, nrow=3)

cat(kuIAxB(faculty))

cat(kuIAxB(food))

cat(kuIAxB(os))

cat(kuIAxB(prefecture))

cat(kuIAxB(newyear))

cat(kuIAxB(language))

cat(kuIAxB(type))

cat(kuIAxB(show))

cat('「語学は何を選んだか」における顕在情報量')
cat('国語') 
nit(4) - nit(2)
cat('数学')
nit(13) - nit(6) - nit(3) - nit(3)
cat('英語') 
nit(16) - nit(10) - nit(2) - nit(3)

cat('「年末の過ごし方」における顕在情報量')
cat('国語') 
nit(4) - nit(2) - nit(2)
cat('数学')
nit(13) - nit(11) - nit(2)
cat('英語') 
nit(16) - nit(11) - nit(2) - nit(2)

cat('「好きな食べ物」における顕在情報量')
cat('国語') 
nit(4) - nit(2)
cat('数学')
nit(13) - nit(7) - nit(4) - nit(2)
cat('英語') 
nit(16) - nit(9) - nit(4) - nit(3)