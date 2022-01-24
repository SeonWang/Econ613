install.packages("withr")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("gdata")
install.packages("boot")
install.packages("xtable")
install.packages("MASS")
install.packages("moments")
install.packages("snow")
install.packages("mvtnorm")
library(ggplot2)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(gdata)
library(boot)
library(xtable)
library(MASS)
library(moments)
library(snow)
library(mvtnorm)

setwd("C:\\Users\\wyz_m\\Desktop\\DUKE\\courses\\ECON 613\\assignment\\A0")
getwd()
ls()
??Hmisc

# Vectors, Matrix, Arrays
vec0 = NULL
vec1 = c(1,2,3,4)
vec1[1] # r中indx起点为1
vec2 = 1:4
vec3 = seq(1,4,1)
vec4 = rep(0,4)
sum(vec1)
str(vec1) # structure
prod(vec1) # product
mat1 = mat.or.vec(2,2) # zero matrix
mat2 = matrix(c(1,2,3,4,5,6), ncol = 2, nrow = 3, byrow = T) # 构建一个matrix，byrow=T表示为按行填充
mat3 = cbind(c(0,0,1),c(0,1,0)) # cbind,按列合并，rbind，按行合并
mat4 = rbind(c(1,1),c(0,0))
mat5 = matrix(1:20,nrow = 5, ncol = 4)
mat5[1:2,3:4]
arr1 = array(0, c(3,2)) #1维array类似于vector，2维度array等于matrix
dim(mat4)
dim(vec2)
length(vec2) # vector长度
length(mat1) # matrix 元素总数
class(mat1) # 2维度array等于matrix

print(Titanic)
titanic = Titanic
dim(titanic)
str(titanic)
class(titanic)

sum(titanic) 
sum(titanic[,,"Adult",])
sum(titanic["Crew",,,])
sum(titanic["3rd",,"Child",])
sum(titanic["2nd","Female","Adult",])
sum(titanic["1st","Male","Child",])
sum(titanic["Crew","Female",,"Yes"])
sum(titanic["1st","Male","Adult","Yes"])

prop.table(titanic["1st","Male","Adult",])["Yes"]
prop.table(titanic["1st","Female","Adult",])["Yes"]
prop.table(titanic["1st","Male","Child",])["Yes"]
prop.table(titanic["3rd","Female","Adult",])["Yes"]
?prop.table

#  Exercise 3 Vectors --- Details
a = 1:50
a = seq(1,50,1)
b = 50:1
b = seq(50,1,-1)
b = rev(a)

a = rep(c(10,19,7),15)
b = rep(c(125,6),8)

c = log(seq(3.1,6,0.1)) * sin(seq(3.1,6,0.1))

a = sample(0:100, size = 90,replace = TRUE) # replace=False代表取出后不放回概率
mean(a)

a = 1:20
b = t(1:15)
sum(exp(sqrt(a))*log(a^5)/(5+cos(a)%*%sin(b))) # %*%为点乘 ****蛮重要的方法
temp = 0
for (a in 1:20){
  b  = 1:a
  temp = temp + sum(exp(sqrt(a))*log(a^5)/(5+exp(a*b)*cos(a)*sin(b)))
}
temp
temp = 0
for (a in 1:20){
  for (b in 1:a){
    temp = temp + exp(sqrt(a))*log(a^5)/(5+exp(a*b)*cos(a)*sin(b))
  }
}
temp

x = seq(3,6,0.1)
a = exp(x)*cos(x)

# Exercise 4 Vectors --- Advanced
xVec = sample(0:999, size = 1000)
yVec = sample(0:999, size = 1000)

zVec = yVec[2:1000] - xVec[1:999]
wVec = sin(yVec[1:999])/cos(xVec[2:1000])

subX = xVec[xVec >= 200]
which(yVec>=600)

# Exercise 5 Matrix
A = matrix(c(1,5,-2,1,2,-1,3,6,-3),3,3)
A%*%A%*%A
A^3 == 0
A%^%3 == 0

A = rbind(A, A[1,]+A[3,])
A[3,] = A[1,]+A[2,]

apply(A, MARGIN = 1, mean) # by row
apply(A, MARGIN = 2, mean) # by column

rowMeans(A) # 求行均值
colMeans(A) # 求列均值

solve(matrix(c(2,1,3,1,1,1,1,3,2),3,3,byrow=T), c(10,6,13))

# Exercise 6 Functions
fun1 = function(a,n){
  return(sum(a^(1:n)/(1:n)))
}

fun2 = function(x){
  if (x < 0){return(x^2+2*x+abs(x))}
  if (x >= 0 & x<2){return(x^2+3+log(1+x))}
  if (x>=2){return(x^2+4*x-14)}
}
fun2(3)

# Exercise 7 Indexes
v1 = sample(1:20,36,replace=T)
v1
v1[-1] # 非首列数
v1[2:36]

v2=v1>5 # bool判断
v2
as.integer(v2) # 将bool类型vector变成0，1变量

v3 = matrix(v1,6,6,T)
v3

x = c(rnorm(10),NA,paste("d",1:16),NA,log(rnorm(10)))
condition = is.na(x) + is.infinite(x) # 与which类似，只不过which直接返回数据，is函数返回符合值的bool
condition
!condition
x[!condition]

# Exercise 8 Data Manipulation
install.packages("AER")
library(car)
library(carData)
library(lmtest)
library(zoo)
library('AER')
data("GSOEP9402",package = "AER") # 从package中导入数据
dat = GSOEP9402 # 令某个变量等于从package中提取的数据

class(dat) # dataframe
nrow(dat) # 返回行数
ncol(dat) # 返回列数
colnames(dat) # dataframe返回列名

install.packages("dplyr") # 筛选汇总的包，有用
require(dplyr) # library和require都可以载入包，但二者存在区别。
require(ggplot2) # 在一个函数中，如果一个包不存在，执行到library将会停止执行，require则会继续执行

ggplot(dat %>% group_by(year) %>% summarise(mean_income = mean(income)) , aes(x=year,y=mean_income)) + geom_line() + ylab("Average Income")
# ggplot对dataframe画图，aes决定横纵轴;ggplot只产生画布，+ geom_line画线 + ylab画纵轴名称
group_by(dat,year) # group_by(.data, ...,
summarise(group_by(dat,year), mean_income = mean(income)) # 对有分组的对象，组内summary，且可自己命名新变量，此处为mean_income

gender = dat %>% group_by(gender) %>% summarise(mean_income=mean(income))
gender
school = dat %>% group_by(school) %>% summarise(mean_income=mean(income))
memployment = dat %>% group_by(memployment) %>% summarise(mean_income=mean(income))
incomes = c('male-female'=gender$mean_income[[1]]-gender$mean_income[[2]], 
            'gymnasium-hauptschule'=school$mean_income[[3]]-school$mean_income[[1]], 
            'realschule-hauptschule'=school$mean_income[[2]]-school$mean_income[[1]],
            'none-fulltime'=memployment$mean_income[[3]]-memployment$mean_income[[1]], 
            'none-parttime'= memployment$mean_income[[2]]-memployment$mean_income[[1]])

# Exercise 9 First Regression
data("CASchools", package = "AER")
dat1 = CASchools
colnames(dat1)

dat1$school= factor(dat1$school) # 需要把非数字类型转为factor，方便后续分类统计
dat1$district= factor(dat1$district)
?factor
reg1 = lm(read ~ .-math, dat1) # "-"表示不包含这个变量
summary(reg1) # 查看回归结果
?lm

formula = y~x.lm(formula) # ?????
reg2 = lm(read ~ .-math, dat1[1:200,])
summary(reg2)

# Exercise 10 Advanced indexing
install.packages("actuar")
require(actuar)
lu = rpareto(200, 1, 1)  
lu > 10
length(lu[lu > 10])
lu[lu>10] = rlogis(length(lu[lu > 10]),6.5,0.5)
lu>10

de = rnorm(200,1,1)
de = log(de)
de[which(is.nan(de))] = rtruncnorm(length(which(is.nan(de))))
de[which(de<0)] = rtruncnorm(length(which(de<0)), a=0)

orig = runif(200,0,1)
dest = runif(200,0,1)

hist = matrix(runif(200*200,0,1),200,200)
dist = matrix(runif(200*200,0,1),200,200)

a = c(1,2)
b = matrix(c(2,2,3,3), 2,2)
a/b # ****一个vector除一个矩阵是按列除的
a + b # ****一个vector加一个矩阵是按列加的
c = t(c(3,4))
a + c # 转置vector相加不会得到matrix

# *****重要,outer函数可以做外积、外加等
int1 = outer(orig, dest, "+")+dist
outer(c(1,2),c(2,3), "+")
? outer
su = log(int1)/(1+log(int1))
int2 = outer(orig, dest, "+")+hist
se = exp(int2)/(1+exp(int2))

r = 0.05
getQ = function(w) {
  frac = outer(r+de, r+de, "/") 
  one = frac * w
  two = lu * log(w)
  three = lu * (1+log(w))
  mid = outer(two,three,"-")
  four = frac * (rowSums(su)-diag(su)) - matrix(rep((rowSums(su)-diag(su)),200),200,200,byrow=T)
  five = frac * (rowSums(se)-diag(se)) - matrix(rep((rowSums(se)-diag(se)),200),200,200,byrow=T)
  return(one + mid + four + five)
}
diag(a)
getQ(9245)
?rowSums
a = c(1,2)
b = c(2,3)
outer(a,a,"/")
c = matrix(c(1:4),2,2)
c
rowSums(c)
c*a

getQ2 = function(w){
  m = matrix(NA,nrow=200,ncol=200)
  for (j in 1:200){
    for (l in 1:200) {
      sum1_su = 0; sum2_su = 0
      sum1_se = 0; sum2_se = 0
      term1 = (r+de[j])/(r+de[l])*w 
      term2 = lu[j]*log(w) 
      term3 = lu[l]*(1+log(w)) 
      for (k in 1:200) {
        if (k!=j){
          sum1_su = sum1_su + su[j,k]
          sum1_se = sum1_se + se[j,k]
        }
        if (k!=l){
          sum2_su = sum2_su + su[l,k]
          sum2_se = sum2_se + se[l,k]
        }
      }
      term4 = (r+de[j])/(r+de[l])*sum1_su - sum2_su
      term5 = (r+de[j])/(r+de[l])*sum1_se - sum2_se
      m[j,l] = term1+term2-term3+term4+term5
    }
  }
  return(m)
}  
getQ2(9245)
install.packages("pracma")
require(pracma)
gridw = seq(9100,55240,50)

system.time(sapply(gridw, FUN=getQ, simplify = F)) # sapply可直接用于vector，如果是matrix等，可用apply中的margin进行调整
?sapply

# LIST
mat1
li = list() # R 中的list是一个大融合，可以放很多类型的数据
li[[1]] = mat1
?list
li[[2]] = Titanic
li1 = list(x=mat1,y=Titanic,z=su) # list中可以用数字index，也可以自己命名，如本句
li1$x
li1$y

# Dataframe
data=data.frame(x=rnorm(100),y=runif(100))
data
View(data)
edit(data)
?browse
data[,1]
data[1,]
data$x
names(data)
colnames(data)
attach(data)
?attach # attach后可以直接输入colname进行操作
x
detach(data)
y

test = c(1,2,3, NA,factor(1),1/0)
is.na(test)
is.list(test)
as.list(test)
is.factor(test)
is.matrix(test)
is.vector(test)
is.array(test)
is.finite(test)
test>1

# Exercise 11 Test and Indexing
is.array(c(1,2,3))
is.vector(c(1,2,3))
is.matrix(c(1,2,3))

x0 = rnorm(1000)
table(x0>0) # table就好像会对数据进行一定的判断
table(x0>0)[2]
class(table(x0>0))
?table

x1 = cut2(runif(100,0,1),g=10) # g = number of quantile groups, 或者自己输入如c(),分成interval后，元素变为interval，即level
x1
levels(x1)=paste("q",1:10,sep="") # 连接strings，并做循环,重新赋值level后，cut后的数据也会变换成新的level
x1
?cut2
?paste

is.factor(x1) # cut了之后数据直接变成了factor

table(x1 == "q1")[2] # 对于factor类型数据，可以用table来进行统计

as.numeric(x1) # factor变成了整数

rand = rnorm(1000)

which(rand>0)

x = rnorm(1000)
w = x[which(x>0)]
w = subset(x,x>0) # subset(x, subset, ...)
2 = x[x>0]
?subset

# Basic Functions
x = rnorm(1000)
x = abs(x)
sqrt(x)
ceiling(x) # 向上趋近整数
floor(x) # 向下趋近整数
trunc(x) # 去尾
?trunc
round(x,digits = 1) # 四舍五入
x
signif(x,digits = 3) # 取有效数字后四舍五入

x="abcde"
substr(x,1,3)
grep("ab",x) # pattern可以是自己输入，也可以是正则化表达
?grep()
x = sub("ab","6666",x)
x
y = strsplit(x, 'c') # 以某个标志分开，不包含标志
y
paste(1:3,"a",sep=",")
paste(1:3,c("a","b","c"),sep=",")
toupper(x) # 小写变大写
tolower(x)

# Exercise 12 Programming
curr = 0
for (i in 1:400){
  curr = curr + i ^ 2
}
curr

?by()
require(stats)
warpbreaks = warpbreaks
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary) # 对dataframe中不同factor进行函数操作
f = function(x){return(x+1)}

curr = 0
for (i in 1:249){
  curr = curr + i * (i + 1)
}
curr

crra = function(c, theta){
  if (theta >= 0.97 & theta <=1.03){theta = 1}
  return(c^(1-theta)/(1-theta))
}

fact = function(x){
  temp = 1
  for (i in 1:x){
    temp = temp * i
  }
  return(temp)
}
fact(4)

# Exercise 13 Apply Functions
m = matrix(c(rnorm(20,0,10), rnorm(20,-1,10)), nrow = 20, ncol = 2)
apply(m,MARGIN = 1,FUN = mean)
apply(m,2,mean)

apply(m,1,median)
apply(m,2,median)

apply(m,1,sd)
apply(m,2,sd)

require(datasets)
data("iris",package = "datasets")
iris = iris

by(iris[,c("Sepal.Length",'Sepal.Width')], iris[,"Species"], colMeans) # 求多列值用colMeans
f = function(x){
  return(sum(log(x)))
}
by(iris[,c("Sepal.Length",'Sepal.Width')], iris[,"Species"], f)
y1 = NULL; for (i in 1:100) y1[i]=exp(i)
y2 = exp(1:100)
y3 = sapply(1:100,exp)
y1
y2
y3
system.time(for (i in 1:100) y1[i]=exp(i))
system.time(exp(1:100))
system.time(sapply(1:100,exp))

# Exercise 14 Simulating and Computing
x = rnorm(10000)
summary(x)

dsummary = function(x){
  dat = data.frame(min = min(x),first_decile = quantile(x,0.1))
  return(dat)
}
dsummary(c(1,2,3))
vec = c(1,2,3)
quantile(vec,0.1)
quantile(vec,c(0.1,0.5,0.9)) # 分位数函数可以返回好几个分位数

dnorm(0.5, mean=2,sd=0.05) # 概率密度函数
pnorm(2.5,mean=2,sd=0.05) # 概率累计函数
qnorm(0.95,mean=2,sd=0.05) # 分位数函数

dt(0.5, df=5)

dpareto(0.5,3,1)

# Exercise 15 Moments
v = rnorm(100,-2,5)
length(v)
m = mean(v)
m
var(v)
skewness(v)
kurtosis(v)

# OLS
X = matrix(rbeta(10000,2,1),1000,10)
length(x[x<=0])

sigma_2 = 0.5
beta = rgamma(10,2,1)

epsilon = rnorm(1000)
Y = X %*% beta + sqrt(sigma_2) * epsilon
beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y # solve函数单个参数时即为求逆

y_hat = X %*% beta
epsilon_hat = y_hat - Y
hist(epsilon_hat, col="gray", freq = F) # F是density，T是frequency
plot(density(epsilon_hat))
sigma_2 = t(epsilon_hat)%*%epsilon_hat/(1000-10-1)
V_beta_hat = as.numeric(sigma_2)*solve(t(X)%*%X)
param = cbind(beta,sqrt(V_beta_hat)) 
fit0 = lm(Y~0+X)
summary(fit0)
confint(fit0) # 回归系数置信区间

sigma_2 = 0.01
beta = rgamma(10,2,1)

epsilon = rnorm(1000)
Y = X %*% beta + sqrt(sigma_2) * epsilon
beta_hat = solve(t(X)%*%X)%*%t(X)%*%Y # solve函数单个参数时即为求逆

y_hat = X %*% beta
epsilon_hat = y_hat - Y
hist(epsilon_hat, col="gray", freq = F) # F是density，T是frequency
plot(density(epsilon_hat))
sigma_2 = t(epsilon_hat)%*%epsilon_hat/(1000-10-1)
V_beta_hat = as.numeric(sigma_2)*solve(t(X)%*%X)
param = cbind(beta,sqrt(V_beta_hat)) 
fit1 = lm(Y~0+X)
summary(fit1)
confint(fit1) # 回归系数置信区间
confint(fit0)
