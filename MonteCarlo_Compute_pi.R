# 法一：MonteCarlo-Buffon计算π的值
# 这种算法的存在的问题是θ的取值范围是0到π
# 这里π相当于已知，然后我们再来估计它
rm(list = ls())
# Init data
# constant
a <- 1    # 1/2 width of parallel lines
l <- 0.6  # 1/2 length of buffon needle
n <- 100000 # number of random numbers
# random variable
# theta: angle between needle and parallel line, [0,pi]
# y: distance from the midpoint of the needle to the parallel line,[0,a]
MonteCarol_Buffon <- function(a,l,n){
  k <- 0
  for (i in 1:n){
    u1 <- runif(2)
    theta <- u1[1]*pi ###???
    y <- u1[2]*a
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