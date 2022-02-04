require(stats)
require(base)
library(ggplot2)
library(patchwork)
library(data.table)
library(bit64)
library(dplyr)
setwd("C:\\Users\\wyz_m\\Desktop\\DUKE\\courses\\ECON 613\\assignment\\A2")
getwd()


# Exercise 1 OLS estimate

## Import data.
datind2009 = fread('./data/datind2009.csv')
dat = na.omit(datind2009[,c('wage','age')])
dat = dat[dat$wage!=0,]
X = cbind(rep(1,length(dat$age)),dat$age)
Y = dat$wage

## Calculate the correlation between Y and X(age).
age = dat$age
corr = sum((age-mean(age))*(Y-mean(Y))) /
  (sqrt(sum((age-mean(age))^2))*sqrt(sum((Y-mean(Y))^2))) 
corr


## Calculate the coefficients on this regression
beta_hat = c(solve(t(X)%*%X)%*%t(X)%*%Y)
names(beta_hat) = c('intercept','beta1(age)')
beta_hat


## Calculate the standard errors of beta.
# 1.OLS formula
error = Y - X %*% beta_hat
s_sqr = as.numeric((t(error) %*% error) / (length(Y) - 2))
se_beta_hat = diag(sqrt(s_sqr * solve(t(X) %*% X))) # (645.2348  14.8774)
names(se_beta_hat) = c('se_intercept(ols)','se_beta1(ols)')
# 2. Bootstrap
set.seed(123)
boot = function(data, boot_n){
  inter_result = c()
  beta1_result = c()
  for (i in 1:boot_n){
    indices = sample(1:nrow(data) ,nrow(data) ,replace = T)
    d = dat[indices,]
    Y = as.matrix(d[,1])
    X = as.matrix(cbind(rep(1,length(d[,2])),d[,2]))
    beta_hat_f = c(solve(t(X)%*%X)%*%t(X)%*%Y)
    inter_result = c(inter_result, beta_hat_f[1])
    beta1_result = c(beta1_result, beta_hat_f[2])
  }
  return(data.frame(se_intercept = inter_result, se_beta1 = beta1_result))
}
results1 = apply(boot(dat,49),2,function (x) sd(x))
names(results1) = c('se_intercept(boot49)','se_beta1(boot49)')
results2 = apply(boot(dat,499),2,function (x) sd(x))
names(results2) = c('se_intercept(boot499)','se_beta1(boot499)')

se_beta_hat # OLS
results1 # 49 boot
results2 # 499 boot
# Comment: Two strategies gives similar results. When you bootstrap more, you give more similar results.

# Exercise 2 Detrend Data
## Create a categorical variable
datind_dirs = paste("./data/datind",2005:2018,sep = "")%>%paste(".csv",sep = "")
datind_total = fread(datind_dirs[1])
for (i in c(2:length(datind_dirs))){
  dat_temp = fread(datind_dirs[i])
  dat_temp$idind = as.integer64(dat_temp$idind)
  datind_total = rbind(datind_total, dat_temp)
}
datind_total = datind_total[,-1]
datind_total = datind_total[!is.na(datind_total$age)&datind_total$wage!=0,]

ag_list = c()
for (ag in datind_total$age){
  if (ag < 18){ag_list = c(ag_list, "<18")}
  if (ag >= 18 & ag <= 25){ag_list = c(ag_list, "18-25")}
  if (ag >= 26 & ag <= 30){ag_list = c(ag_list, "26-30")}
  if (ag >= 31 & ag <= 35){ag_list = c(ag_list, "31-35")}
  if (ag >= 36 & ag <= 40){ag_list = c(ag_list, "36-40")}
  if (ag >= 41 & ag <= 45){ag_list = c(ag_list, "41-45")}
  if (ag >= 46 & ag <= 50){ag_list = c(ag_list, "46-50")}
  if (ag >= 51 & ag <= 55){ag_list = c(ag_list, "51-55")}
  if (ag >= 56 & ag <= 60){ag_list = c(ag_list, "56-60")}
  if (ag > 60){ag_list = c(ag_list, "60+")}
}



## Plot the wage of each age group across years
datind_total$ag = ag_list
datind_total$ag = factor(datind_total$ag)
datind_total$year = factor(datind_total$year)

age_year_matrix = by(datind_total$wage,datind_total[,c('ag','year')],mean)

for (y in 1:14){
  year = y + 2004
  plot_dat = data.frame(age_group = names(age_year_matrix[,y]),
                        wage_mean = age_year_matrix[,y])
  plot_temp = ggplot(plot_dat,aes(age_group,wage_mean)) + 
    geom_bar(stat = 'identity') +
    ggtitle(label=year) +
    theme(axis.text.x = element_text(size=5), 
          plot.margin = unit(rep(0.1,4),"cm"))
  if (y == 1){p1 = plot_temp}else{p1 = p1 + plot_temp}
}

for (a in 1:10){
  age_group = names(age_year_matrix[,1])[a]
  plot_dat = data.frame(wage_mean = age_year_matrix[a,],
                        year = names(age_year_matrix[1,]))
  plot_temp = ggplot(plot_dat,aes(year,wage_mean)) + 
    geom_bar(stat = 'identity') +
    ggtitle(label=age_group) +
    theme(axis.text.x = element_text(size=5), 
          plot.margin = unit(rep(0.1,4),"cm"))
  if (a == 1){p2 = plot_temp}else{p2 = p2 + plot_temp}
}

library(RColorBrewer)
p3 = ggplot(datind_total,aes(x=year,y=wage,fill=ag))+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = quantile(datind_total$wage, c(0.1, 0.9)))+
  scale_fill_brewer()+
  ggtitle(label='wage of each age group across years')

p1 # mean of wage in each year
p2 # mean of wage in each ag
p3 # distribution in each year

# Answer: Yes. Wage increases from 18 to 55 years old and starts to decrease  after 56 years old.

## After including a time fixed effect, how do the estimated coefficients change?
datind_total$year_2005 = as.numeric(datind_total$year==2005)
datind_total$year_2006 = as.numeric(datind_total$year==2006)
datind_total$year_2007 = as.numeric(datind_total$year==2007)
datind_total$year_2008 = as.numeric(datind_total$year==2008)
datind_total$year_2009 = as.numeric(datind_total$year==2009)
datind_total$year_2010 = as.numeric(datind_total$year==2010)
datind_total$year_2011 = as.numeric(datind_total$year==2011)
datind_total$year_2012 = as.numeric(datind_total$year==2012)
datind_total$year_2013 = as.numeric(datind_total$year==2013)
datind_total$year_2014 = as.numeric(datind_total$year==2014)
datind_total$year_2015 = as.numeric(datind_total$year==2015)
datind_total$year_2016 = as.numeric(datind_total$year==2016)
datind_total$year_2017 = as.numeric(datind_total$year==2017)
datind_total$year_2018 = as.numeric(datind_total$year==2018)

X = as.matrix(cbind(rep(1,length(datind_total$age)),datind_total$age)%>%
                cbind(datind_total[,c('year_2006','year_2007','year_2008','year_2009','year_2010',
                                      'year_2011','year_2012','year_2013','year_2014','year_2015',
                                      'year_2016','year_2017','year_2018')]))
Y = datind_total$wage
beta_hat = c(solve(t(X)%*%X)%*%t(X)%*%Y)
names(beta_hat) = c('intercept','beta1(age)','year_2006','year_2007','year_2008',
                    'year_2009','year_2010','year_2011','year_2012','year_2013',
                    'year_2014','year_2015','year_2016','year_2017','year_2018')
beta_hat
# Answer: After including a time fixed effect, estimated coefficient of age is larger.


# Exercise 3 Numerical Optimization
## Exclude all individuals who are inactive
datind2007 = fread('./data/datind2007.csv')[,-1]
datind2007 = datind2007[datind2007$empstat != 'Inactive' & datind2007$empstat != 'Retired',]

## Write a function that returns the likelihood of the probit of being employed
flike = function(beta,x,y)
{
  x_beta = beta[1] + beta[2]*x
  pr = pnorm(x_beta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

## Optimize the model and interpret the coefficients
set.seed(123)
x = datind2007$age
y = as.numeric(datind2007$empstat == 'Employed')

ntry = 100
out = mat.or.vec(ntry,3)
for (i in 1:ntry){
  start = runif(2,-10,10)
  capture.output(res <- optim(start,
                              fn = flike,
                              method = "BFGS",
                              control = list(trace=6,maxit=1000),
                              x = x,
                              y = y))
  out[i,c(1,2)] = res$par
  out[i,3] = res$value
}
out = data.frame(out)
colnames(out) = c('intercept', 'beta1_hat(age)', '-likelihood')
out[which(out$`-likelihood` == min(out$`-likelihood`)),]

## Can you estimate the same model including wages as a determinant of labor market participation?
datind2007 = datind2007[!is.na(datind2007$wage),]
# Write a function that returns the likelihood of the probit of being employed
flike = function(beta,x1,x2,y)
{
  x_beta = beta[1] + beta[2]*x1 + beta[3]*x2
  pr = pnorm(x_beta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

# Optimize the model and interpret the coefficients
set.seed(123)
x1 = datind2007$age
x2 = datind2007$wage
y = as.numeric(datind2007$empstat == 'Employed')

ntry = 100
out = mat.or.vec(ntry,4)
for (i in 1:ntry){
  start = c(runif(1,-0.5,0.5),runif(1,-0.01,0.01),runif(1,-0.0001,0.0001))
  capture.output(res <- optim(start,
                              fn=flike,
                              method="BFGS",
                              control=list(trace=6,maxit=1000),
                              x1=x1,
                              x2=x2,
                              y=y))
  out[i,c(1,2,3)] = res$par
  out[i,4] = res$value
}
out = data.frame(out)
colnames(out) = c('intercept', 'beta1_hat(age)','beta2_hat(wage)', '-likelihood')
out[which(out$`-likelihood` == min(out$`-likelihood`)),]
# Answer: No. From the results, we can interpret that both age and wage have the positive effect on participation but age is not significant any more. The reason is that there are some unemployed individuals who have zero wage. This significantly influence the contribution of age. So, it is not reasonable to include the wage as a determinant.

# Exercise 4 Discrete choice
## Exclude all individuals who are inactive
datind_dirs = paste("./data/datind",2005:2015,sep = "")%>%paste(".csv",sep = "")
datind_total = fread(datind_dirs[1])
for (i in c(2:length(datind_dirs))){
  dat_temp = fread(datind_dirs[i])
  dat_temp$idind = as.integer64(dat_temp$idind)
  datind_total = rbind(datind_total, dat_temp)
}
datind_total = datind_total[,-1]
datind_total = datind_total[!is.na(datind_total$age)
                            &!is.na(datind_total$wage)
                            &datind_total$empstat!='Inactive'
                            &datind_total$empstat!='Retired',]
datind_total$year_2005 = as.numeric(datind_total$year==2005)
datind_total$year_2006 = as.numeric(datind_total$year==2006)
datind_total$year_2007 = as.numeric(datind_total$year==2007)
datind_total$year_2008 = as.numeric(datind_total$year==2008)
datind_total$year_2009 = as.numeric(datind_total$year==2009)
datind_total$year_2010 = as.numeric(datind_total$year==2010)
datind_total$year_2011 = as.numeric(datind_total$year==2011)
datind_total$year_2012 = as.numeric(datind_total$year==2012)
datind_total$year_2013 = as.numeric(datind_total$year==2013)
datind_total$year_2014 = as.numeric(datind_total$year==2014)
datind_total$year_2015 = as.numeric(datind_total$year==2015)


## Write and optimize the probit, logit, and the linear probability models
set.seed(123)
x1 = datind_total$age
year_2006 = datind_total$year_2006
year_2007 = datind_total$year_2007
year_2008 = datind_total$year_2008
year_2009 = datind_total$year_2009
year_2010 = datind_total$year_2010
year_2011 = datind_total$year_2011
year_2012 = datind_total$year_2012
year_2013 = datind_total$year_2013
year_2014 = datind_total$year_2014
year_2015 = datind_total$year_2015
y = as.numeric(datind_total$empstat == 'Employed')

# probit
flike = function(beta,x1,
                 year_2006,year_2007,
                 year_2008,year_2009,
                 year_2010,year_2011,
                 year_2012,year_2013,
                 year_2014,year_2015,
                 y)
{
  x_beta = beta[1] + beta[2]*x1 +
    beta[3]*year_2006+beta[4]*year_2007+
    beta[5]*year_2008+beta[6]*year_2009+
    beta[7]*year_2010+beta[8]*year_2011+
    beta[9]*year_2012+beta[10]*year_2013+
    beta[11]*year_2014+beta[12]*year_2015
  pr = pnorm(x_beta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

ntry = 50
out = mat.or.vec(ntry,13)
hessian_list = list()
for (i in 1:ntry){
  start = c(runif(1,-1,1),runif(11,-0.15,0.15))
  capture.output(res <- optim(start,
                              fn=flike,
                              method="BFGS",
                              control=list(trace=6,maxit=1000),
                              x1=x1,
                              year_2006=year_2006,year_2007=year_2007,
                              year_2008=year_2008,year_2009=year_2009,
                              year_2010=year_2010,year_2011=year_2011,
                              year_2012=year_2012,year_2013=year_2013,
                              year_2014=year_2014,year_2015=year_2015,
                              y=y,
                              hessian=TRUE))
  out[i,1:12] = res$par
  out[i,13] = res$value
  hessian_list[[i]] = res$hessian
}
out = data.frame(out)
colnames(out) = c('intercept', 'beta1_hat(age)',
                  'year_2006','year_2007','year_2008','year_2009','year_2010',
                  'year_2011','year_2012','year_2013','year_2014','year_2015', 
                  '-likelihood')
probit_out = out[which(out$`-likelihood` == min(out$`-likelihood`)),]
fisher = solve(hessian_list[[as.numeric(row.names(probit_out))]])
sigma  = sqrt(diag(fisher))
z = probit_out/sigma
significance = apply(z,2,function(x) if(x>=1.96|x<=-1.96){return('yes')}else{return('no')})
probit_out = rbind(probit_out,sigma,z,significance)[,-13]
rownames(probit_out) = c('coefficient','std.error','z_value','significant_or_not(p=0.05)')

# logit
flike = function(beta,x1,
                 year_2006,year_2007,
                 year_2008,year_2009,
                 year_2010,year_2011,
                 year_2012,year_2013,
                 year_2014,year_2015,
                 y)
{
  x_beta = beta[1] + beta[2]*x1 +
    beta[3]*year_2006+beta[4]*year_2007+
    beta[5]*year_2008+beta[6]*year_2009+
    beta[7]*year_2010+beta[8]*year_2011+
    beta[9]*year_2012+beta[10]*year_2013+
    beta[11]*year_2014+beta[12]*year_2015
  pr = 1/(1+exp(-x_beta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

ntry = 50
out = mat.or.vec(ntry,13)
hessian_list = list()
for (i in 1:ntry){
  start = c(runif(1,-1.5,1.5),runif(11,-0.3,0.3))
  capture.output(res <- optim(start,
                              fn=flike,
                              method="BFGS",
                              control=list(trace=6,maxit=1000),
                              x1=x1,
                              year_2006=year_2006,year_2007=year_2007,
                              year_2008=year_2008,year_2009=year_2009,
                              year_2010=year_2010,year_2011=year_2011,
                              year_2012=year_2012,year_2013=year_2013,
                              year_2014=year_2014,year_2015=year_2015,
                              y=y,
                              hessian=TRUE))
  out[i,1:12] = res$par
  out[i,13] = res$value
  hessian_list[[i]] = res$hessian
}
out = data.frame(out)
colnames(out) = c('intercept', 'beta1_hat(age)',
                  'year_2006','year_2007','year_2008','year_2009','year_2010',
                  'year_2011','year_2012','year_2013','year_2014','year_2015', 
                  '-likelihood')
logit_out = out[which(out$`-likelihood` == min(out$`-likelihood`)),]
fisher = solve(hessian_list[[as.numeric(row.names(logit_out))]])
sigma  = sqrt(diag(fisher))
z = logit_out/sigma
significance = apply(z,2,function(x) if(x>=1.96|x<=-1.96){return('yes')}else{return('no')})
logit_out = rbind(logit_out,sigma,z,significance)[,-13]
rownames(logit_out) = c('coefficient','std.error','z_value','significant_or_not(p=0.05)')

# linear
X = cbind(rep(1,length(x1)),x1,
          year_2006,year_2007,
          year_2008,year_2009,
          year_2010,year_2011,
          year_2012,year_2013,
          year_2014,year_2015)
Y = y
beta_hat = c(solve(t(X)%*%X)%*%t(X)%*%Y)
linear_out = data.frame(t(beta_hat))
colnames(linear_out) = c('intercept', 'beta1_hat(age)',
                         'year_2006','year_2007','year_2008','year_2009','year_2010',
                         'year_2011','year_2012','year_2013','year_2014','year_2015')
error = Y - X %*% beta_hat
s_sqr = as.numeric((t(error) %*% error) / (length(Y) - 12))
se_beta_hat = diag(sqrt(s_sqr * solve(t(X) %*% X))) 
t = linear_out/se_beta_hat
p_t = data.frame(t(apply(t,2,function(x) if(x>0){return(2*(1 - pt(x,df = length(y)-12)))}
                         else{return(2*pt(x,df = length(y)-12))})))
significance = apply(p_t,2,function(x) if(x<=0.05){return('yes')}else{return('no')})

names(se_beta_hat) = colnames(linear_out)
names(significance) = colnames(linear_out)
colnames(p_t) = colnames(linear_out)
linear_out = rbind(linear_out,se_beta_hat,t,p_t,significance)
rownames(linear_out) = c('coefficient','std.error','t_value','Pr(>|t|)','significant_or_not(p=0.05)')

# outcomes
t(probit_out)
t(logit_out)
t(linear_out)

# Answer: In all three method, the age is positively significant. However, the coefficients are different. But this is not a problem, because the coefficient doesn't mean anything here. We care about the marginal effect.

# Exercise 5 Marginal Effects
x1_bar = mean(x1)
year_2006_bar = mean(datind_total$year_2006)
year_2007_bar = mean(datind_total$year_2007)
year_2008_bar = mean(datind_total$year_2008)
year_2009_bar = mean(datind_total$year_2009)
year_2010_bar = mean(datind_total$year_2010)
year_2011_bar = mean(datind_total$year_2011)
year_2012_bar = mean(datind_total$year_2012)
year_2013_bar = mean(datind_total$year_2013)
year_2014_bar = mean(datind_total$year_2014)
year_2015_bar = mean(datind_total$year_2015)
x_bar = c(1,x1_bar,
            year_2006_bar,year_2007_bar,
            year_2008_bar,year_2009_bar,
            year_2010_bar,year_2011_bar,
            year_2012_bar,year_2013_bar,
            year_2014_bar,year_2015_bar)

## marginal effect
# probit marginal effect
x_bar_beta = sum(as.numeric(probit_out[1,]) * x_bar)
probit_me = dnorm(x_bar_beta) * as.numeric(probit_out[1,])
names(probit_me) = c('intercept_me', 'beta1_hat(age)_me',
                  'year_2006_me','year_2007_me','year_2008_me',
                  'year_2009_me','year_2010_me','year_2011_me',
                  'year_2012_me','year_2013_me','year_2014_me',
                  'year_2015_me')


# logit marginal effect
x_bar_beta = sum(as.numeric(logit_out[1,]) * x_bar)
logit_me = exp(-x_bar_beta)/(1+exp(-x_bar_beta))^2 * as.numeric(logit_out[1,])
names(logit_me) = c('intercept_me', 'beta1_hat(age)_me',
                  'year_2006_me','year_2007_me','year_2008_me',
                  'year_2009_me','year_2010_me','year_2011_me',
                  'year_2012_me','year_2013_me','year_2014_me',
                  'year_2015_me')

# results
probit_me #probit me
logit_me #logit me

## standard error
# probit
flike = function(beta,x1,
                 year_2006,year_2007,
                 year_2008,year_2009,
                 year_2010,year_2011,
                 year_2012,year_2013,
                 year_2014,year_2015,
                 y)
{
  x_beta = beta[1] + beta[2]*x1 +
           beta[3]*year_2006+beta[4]*year_2007+
           beta[5]*year_2008+beta[6]*year_2009+
           beta[7]*year_2010+beta[8]*year_2011+
           beta[9]*year_2012+beta[10]*year_2013+
           beta[11]*year_2014+beta[12]*year_2015
  pr = pnorm(x_beta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

for (boot_n in 1:10){
  datind_boot = datind_total[sample(1:nrow(datind_total),nrow(datind_total),replace = T),]
  datind_boot$year_2005 = as.numeric(datind_boot$year==2005)
  datind_boot$year_2006 = as.numeric(datind_boot$year==2006)
  datind_boot$year_2007 = as.numeric(datind_boot$year==2007)
  datind_boot$year_2008 = as.numeric(datind_boot$year==2008)
  datind_boot$year_2009 = as.numeric(datind_boot$year==2009)
  datind_boot$year_2010 = as.numeric(datind_boot$year==2010)
  datind_boot$year_2011 = as.numeric(datind_boot$year==2011)
  datind_boot$year_2012 = as.numeric(datind_boot$year==2012)
  datind_boot$year_2013 = as.numeric(datind_boot$year==2013)
  datind_boot$year_2014 = as.numeric(datind_boot$year==2014)
  datind_boot$year_2015 = as.numeric(datind_boot$year==2015)
  
  x1 = datind_boot$age
  year_2006 = datind_boot$year_2006
  year_2007 = datind_boot$year_2007
  year_2008 = datind_boot$year_2008
  year_2009 = datind_boot$year_2009
  year_2010 = datind_boot$year_2010
  year_2011 = datind_boot$year_2011
  year_2012 = datind_boot$year_2012
  year_2013 = datind_boot$year_2013
  year_2014 = datind_boot$year_2014
  year_2015 = datind_boot$year_2015
  y = as.numeric(datind_boot$empstat == 'Employed')
  
  ntry = 50
  out = mat.or.vec(ntry,13)
  for (i in 1:ntry){
    start = c(runif(1,-5,5),runif(11,-1,1))
    capture.output(res <- optim(start,
                fn=flike,
                method="BFGS",
                control=list(trace=6,maxit=1000),
                x1=x1,
                year_2006=year_2006,year_2007=year_2007,
                year_2008=year_2008,year_2009=year_2009,
                year_2010=year_2010,year_2011=year_2011,
                year_2012=year_2012,year_2013=year_2013,
                year_2014=year_2014,year_2015=year_2015,
                y=y))
    out[i,1:12] = res$par
    out[i,13] = res$value
  }
  out = data.frame(out)
  colnames(out) = c('intercept', 'beta1_hat(age)',
                  'year_2006','year_2007','year_2008','year_2009','year_2010',
                  'year_2011','year_2012','year_2013','year_2014','year_2015', 
                  '-likelihood')
  out = out[which(out$`-likelihood` == min(out$`-likelihood`)),]
  out = out[1,-13]
  
  x1_bar = mean(x1)
  year_2006_bar = mean(datind_total$year_2006)
  year_2007_bar = mean(datind_total$year_2007)
  year_2008_bar = mean(datind_total$year_2008)
  year_2009_bar = mean(datind_total$year_2009)
  year_2010_bar = mean(datind_total$year_2010)
  year_2011_bar = mean(datind_total$year_2011)
  year_2012_bar = mean(datind_total$year_2012)
  year_2013_bar = mean(datind_total$year_2013)
  year_2014_bar = mean(datind_total$year_2014)
  year_2015_bar = mean(datind_total$year_2015)
  x_bar = c(1,x1_bar,
              year_2006_bar,year_2007_bar,
              year_2008_bar,year_2009_bar,
              year_2010_bar,year_2011_bar,
              year_2012_bar,year_2013_bar,
              year_2014_bar,year_2015_bar)
  
  x_bar_beta = sum(as.numeric(out[1,]) * x_bar)
  me = dnorm(x_bar_beta) * as.numeric(out[1,])
  names(me) = c('intercept_me', 'beta1_hat(age)_me',
                    'year_2006_me','year_2007_me','year_2008_me',
                    'year_2009_me','year_2010_me','year_2011_me',
                    'year_2012_me','year_2013_me','year_2014_me',
                    'year_2015_me')
  if (boot_n==1){
    probit_me = me
  }else{
    probit_me = rbind(probit_me,me)
  }
}

# logit
flike = function(beta,x1,
                 year_2006,year_2007,
                 year_2008,year_2009,
                 year_2010,year_2011,
                 year_2012,year_2013,
                 year_2014,year_2015,
                 y)
{
  x_beta = beta[1] + beta[2]*x1 +
           beta[3]*year_2006+beta[4]*year_2007+
           beta[5]*year_2008+beta[6]*year_2009+
           beta[7]*year_2010+beta[8]*year_2011+
           beta[9]*year_2012+beta[10]*year_2013+
           beta[11]*year_2014+beta[12]*year_2015
  pr = 1/(1+exp(-x_beta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

for (boot_n in 1:10){
  datind_boot = datind_total[sample(1:nrow(datind_total),nrow(datind_total),replace = T),]
  datind_boot$year_2005 = as.numeric(datind_boot$year==2005)
  datind_boot$year_2006 = as.numeric(datind_boot$year==2006)
  datind_boot$year_2007 = as.numeric(datind_boot$year==2007)
  datind_boot$year_2008 = as.numeric(datind_boot$year==2008)
  datind_boot$year_2009 = as.numeric(datind_boot$year==2009)
  datind_boot$year_2010 = as.numeric(datind_boot$year==2010)
  datind_boot$year_2011 = as.numeric(datind_boot$year==2011)
  datind_boot$year_2012 = as.numeric(datind_boot$year==2012)
  datind_boot$year_2013 = as.numeric(datind_boot$year==2013)
  datind_boot$year_2014 = as.numeric(datind_boot$year==2014)
  datind_boot$year_2015 = as.numeric(datind_boot$year==2015)
  
  x1 = datind_boot$age
  year_2006 = datind_boot$year_2006
  year_2007 = datind_boot$year_2007
  year_2008 = datind_boot$year_2008
  year_2009 = datind_boot$year_2009
  year_2010 = datind_boot$year_2010
  year_2011 = datind_boot$year_2011
  year_2012 = datind_boot$year_2012
  year_2013 = datind_boot$year_2013
  year_2014 = datind_boot$year_2014
  year_2015 = datind_boot$year_2015
  y = as.numeric(datind_boot$empstat == 'Employed')
  
  ntry = 50
  out = mat.or.vec(ntry,13)
  for (i in 1:ntry){
    start = c(runif(1,-5,5),runif(11,-1,1))
    capture.output(res <- optim(start,
                fn=flike,
                method="BFGS",
                control=list(trace=6,maxit=1000),
                x1=x1,
                year_2006=year_2006,year_2007=year_2007,
                year_2008=year_2008,year_2009=year_2009,
                year_2010=year_2010,year_2011=year_2011,
                year_2012=year_2012,year_2013=year_2013,
                year_2014=year_2014,year_2015=year_2015,
                y=y))
    out[i,1:12] = res$par
    out[i,13] = res$value
  }
  out = data.frame(out)
  colnames(out) = c('intercept', 'beta1_hat(age)',
                  'year_2006','year_2007','year_2008','year_2009','year_2010',
                  'year_2011','year_2012','year_2013','year_2014','year_2015', 
                  '-likelihood')
  out = out[which(out$`-likelihood` == min(out$`-likelihood`)),]
  out = out[1,-13]
  
  x1_bar = mean(x1)
  year_2006_bar = mean(datind_total$year_2006)
  year_2007_bar = mean(datind_total$year_2007)
  year_2008_bar = mean(datind_total$year_2008)
  year_2009_bar = mean(datind_total$year_2009)
  year_2010_bar = mean(datind_total$year_2010)
  year_2011_bar = mean(datind_total$year_2011)
  year_2012_bar = mean(datind_total$year_2012)
  year_2013_bar = mean(datind_total$year_2013)
  year_2014_bar = mean(datind_total$year_2014)
  year_2015_bar = mean(datind_total$year_2015)
  x_bar = c(1,x1_bar,
              year_2006_bar,year_2007_bar,
              year_2008_bar,year_2009_bar,
              year_2010_bar,year_2011_bar,
              year_2012_bar,year_2013_bar,
              year_2014_bar,year_2015_bar)
  
  x_bar_beta = sum(as.numeric(out[1,]) * x_bar)
  me = exp(-x_bar_beta)/(1+exp(-x_bar_beta))^2 * as.numeric(out[1,])
  names(me) = c('intercept_me', 'beta1_hat(age)_me',
                    'year_2006_me','year_2007_me','year_2008_me',
                    'year_2009_me','year_2010_me','year_2011_me',
                    'year_2012_me','year_2013_me','year_2014_me',
                    'year_2015_me')
  if (boot_n==1){
    logit_me = me
  }else{
    logit_me = rbind(logit_me,me)
  }
}

results1 = apply(probit_me,2,sd)
results2 = apply(logit_me,2,sd)

# results
results1 #probit me sd
results2 #logit me sd