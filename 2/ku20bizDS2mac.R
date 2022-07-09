# ビジネスのためのデータサイエンス / データマイニング（第２週）
#                     by  T. Kuwahara,  Keio University at SFC
#                                kuwahara@sfc.keio.ac.jp


# 作業ディレクトリの設定（自分の環境にあわせて修正して下さい）
setwd("/Users/kudotmac/Documents/Documents/2. Course/courses2010/dm/data")

# 潜在情報量を計算する関数
# nit : function - the amount of information (if n=0 then nit=0 )
  nit <- function(n) {
	v <- 2*n*log(n); if (is.nan(v))  v <- 0
	return(as.numeric(v))
	}

#---------------------------------------------------------------------------------------------------------------

# 情報量
# log2(x)
# H = n*log2(k)

# 潜在情報量 bit 　　H = n*log2(n)
n <- 8 ;  H <- n * log2(n) ; cat(H)

# 潜在情報量 nit 　　H = 2* n * loge(n)
  n <- 8 ;  H <- 2 * n * log(n) ; cat(H)
# n <- 8 ;  H <- n * log2(n) ; cat( H * 1.386 )
