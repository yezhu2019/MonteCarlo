# 法一：MonteCarlo-Buffon计算π的值
rm(list = ls())
# Init data
# constant
a <- 1    # 1/2 width of parallel lines
l <- 0.5  # 1/2 length of buffon needle
n <- 1000000 # number of random numbers
# random variable
# theta: angle between needle and parallel line, [0,pi]
# y: distance from the midpoint of the needle to the parallel line,[0,a]
MonteCarol_Buffon <- function(a,l,n){
  k <- 0
  for (i in 1:n){
    theta <- runif(1)*pi #此处的π只是确定针的位置,与π的预测值无关
    y <- runif(1)*a
    if (y<l*sin(theta)){
      k <- k + 1
    }
  }
  # p: probability of intersection between needle and parallel line
  p <- k/n
  pi1 <- 2*l/(a*p)
}
pi1 <- MonteCarol_Buffon(a,l,n)
sprintf('π is %.8f',pi1)


# 法二：概率分析法计算π的值
MonteCarol_statictical <- function(n){
  k <- 0
  x <- runif(n,-1, 1)
  y <- runif(n,-1, 1)
  for (i in 1:n){
    if(x[i]^2 + y[i]^2 < 1)
      k <- k +1
  }
  pi1 <- 4*k/n
}
pi1 <- MonteCarol_statictical(5000)
sprintf('π is %.8f',pi1)