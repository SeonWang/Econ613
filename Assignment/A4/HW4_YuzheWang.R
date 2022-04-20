require(stats)
require(base)
library(ggplot2)
library(patchwork)
library(data.table)
library(bit64)
library(dplyr)
library(RColorBrewer)
library(stargazer)

setwd("C:\\Users\\wyz_m\\Desktop\\DUKE\\courses\\ECON 613\\assignment\\A4")
getwd()

# Exercise 1 Preparing the Data
## 1.1.1 age
dat_nlsy97 = fread('./data/dat_A4.csv')
dat_nlsy97$age = 2019 - dat_nlsy97$KEY_BDATE_Y_1997

## 1.1.2work_exp
job_week = dat_nlsy97[,paste('CV_WKSWK_JOB_DLI.',
                       c(paste(0,1:9,sep=''),10,11),
                       '_2019',
                       sep='')]
job_week[is.na(job_week)] = 0
dat_nlsy97$work_exp = apply(job_week,1,sum)*7/365

## 1.2 years of schooling
# drop na
dat_nlsy97 = dat_nlsy97[!is.na(dat_nlsy97$YSCH.3113_2019)]

# Assumptions:
# GED == 4 years of schooling
# phd == 23 years of schooling
# Professional degree == 21 years of schooling
fun_ex1_2 = function(x){
  if(x == 1){y = 0}
  if(x == 2){y = 4}
  if(x == 3){y = 12}
  if(x == 4){y = 15}
  if(x == 5){y = 16}
  if(x == 6){y = 18}
  if(x == 7){y = 23}
  if(x == 8){y = 21}
  return(y)
}
dat_nlsy97$edu = sapply(dat_nlsy97$YSCH.3113_2019,fun_ex1_2)

## 1.3 visualizations
## 1.3.1 Plot the income data (where income is positive) by i) age groups, ii) gender groups and iii) number of children
# drop na
dat_nlsy97 = dat_nlsy97[!is.na(dat_nlsy97$YINC_1700_2019)]
dat_nlsy97 = dat_nlsy97[!is.na(dat_nlsy97$CV_BIO_CHILD_HH_U18_2019)]

dat_nlsy97$age = as.factor(dat_nlsy97$age)
dat_nlsy97$KEY_SEX_1997 = as.factor(dat_nlsy97$KEY_SEX_1997)
dat_nlsy97$CV_BIO_CHILD_HH_U18_2019 = as.factor(dat_nlsy97$CV_BIO_CHILD_HH_U18_2019)

p1 = ggplot(dat_nlsy97,aes(x=age,y=YINC_1700_2019))+
  geom_boxplot()+
  ggtitle(label='income by age')

p2 = ggplot(dat_nlsy97,aes(x=KEY_SEX_1997,y=YINC_1700_2019))+
  geom_boxplot()+
  ggtitle(label='income by gender')

p3 = ggplot(dat_nlsy97,aes(x=CV_BIO_CHILD_HH_U18_2019,y=YINC_1700_2019))+
  geom_boxplot()+
  ggtitle(label='income by children number')
p1
p2
p3

## 1.3.2 ¨C Table the share of ¡±0¡± in the income data by i) age groups, ii) gender groups, iii) number of children and marital status
fun_ex1_3 = function(x){
  return(sum(x == 0))
}
p4 = summarise(group_by(dat_nlsy97, age), 
          zero_share = fun_ex1_3(YINC_1700_2019)/length(YINC_1700_2019))%>%
  ggplot(aes(x=age,y=zero_share))+
  geom_bar(stat = 'identity')+
  ggtitle(label='zero income share by age')

p5 = summarise(group_by(dat_nlsy97, KEY_SEX_1997), 
               zero_share = fun_ex1_3(YINC_1700_2019)/length(YINC_1700_2019))%>%
  ggplot(aes(x=KEY_SEX_1997,y=zero_share))+
  geom_bar(stat = 'identity')+
  ggtitle(label='zero income share by gender')

p6 = summarise(group_by(dat_nlsy97, CV_BIO_CHILD_HH_U18_2019), 
               zero_share = fun_ex1_3(YINC_1700_2019)/length(YINC_1700_2019))%>%
  ggplot(aes(x=CV_BIO_CHILD_HH_U18_2019,y=zero_share))+
  geom_bar(stat = 'identity')+
  ggtitle(label='zero income share by children number')
p4
p5
p6

# Exercise 2 Heckman Selection Model
## 2.1 Specify and estimate an OLS model to explain the income variable (where income is positive).
dat_nlsy97$age = as.numeric(as.character(dat_nlsy97$age))
dat_nlsy97$CV_BIO_CHILD_HH_U18_2019 = as.numeric(as.character(dat_nlsy97$CV_BIO_CHILD_HH_U18_2019))
dat_nlsy97$KEY_SEX_1997 = as.numeric(as.character(dat_nlsy97$KEY_SEX_1997))
dat_nlsy97$KEY_SEX_1997 = dat_nlsy97$KEY_SEX_1997-1

dat_nlsy97_positive_income = dat_nlsy97[dat_nlsy97$YINC_1700_2019>0]
ols_reg = lm(YINC_1700_2019~age+work_exp+edu+KEY_SEX_1997+CV_BIO_CHILD_HH_U18_2019,data=dat_nlsy97_positive_income)
summary(ols_reg)

# Interpret the estimation results:
# When work experience increases 1 year, the income increases 1042 dollars.
# When years of schooling increases 1 year, the income increases 2119 dollars.
# If female, the income is 19836 dollars lower than male.
# When there is one more children, the income increases 1136 dollars.
# Age has insignificant effect to income.

# Explain why there might be a selection problem when estimating an OLS this way:
# There are some people in the sample have zero income. 
# However, when estimate the effect of independent to the income.
# We actually presume that he/she has non-zero income.
# The people in the sample we selected mostly have non-zero income.
# Therefore, we overlook the effect to the zero income samples.

## 2.2 Explain why the Heckman model can deal with the selection problem.
# Because Heckman model helps to estimate the potential income of those with zero income.

## 2.3 Estimate a Heckman selection model
## step1:probit
dat_nlsy97$income_positive = as.numeric(dat_nlsy97$YINC_1700_2019>0)
flike = function(beta,age,work_exp,edu,female,ch_num,y_real,y)
{
  x_beta = beta[1] + beta[2]*age +
    beta[3]*work_exp+beta[4]*edu +
    beta[5]*female+beta[6]*ch_num
  s = sd(y_real - x_beta)
  pr = pnorm(x_beta/s)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood = y*log(pr) + (1-y)*log(1-pr)
  return(-sum(likelihood))
}

set.seed(100)

ntry = 50
out = mat.or.vec(ntry,7)
for (i in 1:ntry){
  start = c(runif(1,-15000,15000),runif(1,-1000,1000),runif(1,-10000,10000),
            runif(1,-10000,10000),runif(1,-30000,-10000),runif(1,-10000,10000))
  capture.output(res <- optim(start,
                              fn=flike,
                              method="BFGS",
                              control=list(trace=6,maxit=2000),
                              age=dat_nlsy97$age,
                              work_exp=dat_nlsy97$work_exp,
                              edu=dat_nlsy97$edu,
                              female=dat_nlsy97$KEY_SEX_1997,
                              ch_num=dat_nlsy97$CV_BIO_CHILD_HH_U18_2019,
                              y_real=dat_nlsy97$YINC_1700_2019,
                              y=dat_nlsy97$income_positive))
  out[i,c(1:6)] = res$par
  out[i,7] = res$value
}
out = data.frame(out)
par = out[which(out$X7==min(out$X7)),1:6]

step1_fits = apply(dat_nlsy97[,c('age','work_exp','edu','KEY_SEX_1997','CV_BIO_CHILD_HH_U18_2019')],
        1, function(x) return(sum(x * as.numeric(par[2:6]))+as.numeric(par[1])))
sig = sd(dat_nlsy97$YINC_1700_2019-step1_fits)
imr = dnorm(step1_fits/sig)/pnorm(step1_fits/sig)

## step2:OLS estimation
X = as.matrix(cbind(rep(1,length(imr)),dat_nlsy97[,c('age','work_exp','edu','KEY_SEX_1997','CV_BIO_CHILD_HH_U18_2019')],imr))
Y = dat_nlsy97$YINC_1700_2019
step2_beta = solve(t(X)%*%X)%*%t(X)%*%Y
step2_ols_check = lm(dat_nlsy97$YINC_1700_2019~
                   dat_nlsy97$age+
                   dat_nlsy97$work_exp+
                   dat_nlsy97$edu+
                   dat_nlsy97$KEY_SEX_1997+
                   dat_nlsy97$CV_BIO_CHILD_HH_U18_2019+
                   imr)
summary(step2_ols_check)
step2_beta # my results

# Interpret the results from the Heckman selection model and compare the results to OLS results.
# Age now has smaller effect on income, which seems to be more reasonable.
# The effect of work experience is slightly smaller.
# The effects of education is slightly smaller.
# The effects of gender is slightly smaller.
# The effect of children number becomes insignificant while it is significantly positive in ols.
# The difference mainly comes from the problem of sample selection. When we include the samples with zero income, the slopes are overestimated.
# However, with Heckman method, we calculate the potential income for those with zero income, which lower the slope.

# Exercise 3 Censoring
## 3.1 Plot a histogram to check whether the distribution of the income variable.
p7 = ggplot(data=dat_nlsy97,aes(YINC_1700_2019))+
  geom_histogram()+
  ggtitle(label='Wage Distribution in 2005&2019')
p7

## 3.2 Propose a model to deal with the censoring problem
# Censored regression model
## 3.3 Estimate the appropriate model with the censored data
flike2 = function(beta,age,work_exp,edu,female,ch_num,y_real,y,i_a,i_b)
{
  a=0
  b=100000
  x_beta = beta[1] + beta[2]*age +
    beta[3]*work_exp+beta[4]*edu+
    beta[5]*female+beta[6]*ch_num
  
  s = sd(y_real - x_beta)
  pr_a = pnorm((a - x_beta)/s)
  pr_b = pnorm((x_beta - b)/s)
  
  pr_a[pr_a>0.999999] = 0.999999
  pr_a[pr_a<0.000001] = 0.000001
  pr_b[pr_b>0.999999] = 0.999999
  pr_b[pr_b<0.000001] = 0.000001
  
  likelihood = i_a*log(pr_a) + i_b*log(pr_b) + 
    (1 - i_a - i_b) * (log(dnorm((y_real - x_beta)/s)) - log(s))
  return(-sum(likelihood))
}

dat_nlsy97$i_a = as.numeric(dat_nlsy97$YINC_1700_2019 == 0)
dat_nlsy97$i_b = as.numeric(dat_nlsy97$YINC_1700_2019 == 100000)

ntry = 50
out = mat.or.vec(ntry,7)
for (i in 1:ntry){
  start = c(runif(1,-15000,15000),runif(1,-1000,1000),runif(1,-10000,10000),
            runif(1,-10000,10000),runif(1,-30000,-10000),runif(1,-10000,10000))
  capture.output(res <- optim(start,
                              fn=flike2,
                              method="BFGS",
                              control=list(trace=6,maxit=2000),
                              age=dat_nlsy97$age,
                              work_exp=dat_nlsy97$work_exp,
                              edu=dat_nlsy97$edu,
                              female=dat_nlsy97$KEY_SEX_1997,
                              ch_num=dat_nlsy97$CV_BIO_CHILD_HH_U18_2019,
                              y_real=dat_nlsy97$YINC_1700_2019,
                              y=dat_nlsy97$income_positive,
                              i_a=dat_nlsy97$i_a,
                              i_b=dat_nlsy97$i_b))
  out[i,c(1:6)] = res$par
  out[i,7] = res$value
}
out = data.frame(out)
par_tobit = out[which(out$X7==min(out$X7)),1:6]
ols_coe = ols_reg$coefficients
heckman_coe = step2_beta[1:6]
tobit_coe = as.numeric(par_tobit[1:6])
reslts_compare = data.frame(ols= ols_coe, heckman=heckman_coe,tobit=tobit_coe)
reslts_compare
# As we can observe, heckman model gives smaller effect of independent variables than ols model, because we estimate the potential income of those with zero income.
# The tobit model gives larger effect than ols and heckman, because we estimate the potential income of those with income = 100000.
# People with 100000 actually have income larger than 100000. Therefore, if we still treat them as 100000, the efect will be underestimated.
# With Tobit model, we can see that the effect becomes larger becuase we consider the income > 100000.
# take the effect of working experience as an example:
working_experience = rep(dat_nlsy97$work_exp,3)
model = c(rep('OLS',length(dat_nlsy97$V1)),
          rep('Heckman',length(dat_nlsy97$V1)),
          rep('Tobit',length(dat_nlsy97$V1)))
x = cbind(rep(mean(dat_nlsy97$age),length(dat_nlsy97$V1)),
      dat_nlsy97$work_exp,
      rep(mean(dat_nlsy97$edu),length(dat_nlsy97$V1)),
      rep(mean(dat_nlsy97$KEY_SEX_1997),length(dat_nlsy97$V1)),
      rep(mean(dat_nlsy97$CV_BIO_CHILD_HH_U18_2019),length(dat_nlsy97$V1)))
income = c(apply(x,1, function(x) return(sum(x * ols_coe[2:6])+ols_coe[1])),
           apply(x,1, function(x) return(sum(x * heckman_coe[2:6])+heckman_coe[1])),
           apply(x,1, function(x) return(sum(x * tobit_coe[2:6])+tobit_coe[1])))
compare_dat = data.frame(working_experience=working_experience,
                            income=income,
                            model=model)
p8 = ggplot(data=compare_dat,aes(working_experience,income))+
  geom_line(aes(color=model))+
  ggtitle(label='fitted income with working experience, other independent variables take mean value')
p8

#Exercise 4 Panel Data
dat_panel = fread('./data/dat_A4_panel.csv')
for(year in c(1997:2011,c(2013,2015,2017,2019))){
  dat_panel[,paste('age_',year,sep='')] = year - dat_panel$KEY_BDATE_Y_1997
}

fun = function(x){
  return(sum(x[!is.na(x)]))
}
dat_panel$we1997 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:7,sep='')),'_1997',sep='')],1,fun)*7/365
dat_panel$we1998 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_1998',sep='')],1,fun)*7/365
dat_panel$we1999 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_1999',sep='')],1,fun)*7/365
dat_panel$we2000 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_2000',sep='')],1,fun)*7/365
dat_panel$we2001 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:8,sep='')),'_2001',sep='')],1,fun)*7/365
dat_panel$we2002 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10,11),'_2002',sep='')],1,fun)*7/365
dat_panel$we2003 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10),'_2003',sep='')],1,fun)*7/365
dat_panel$we2004 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:7,sep='')),'_2004',sep='')],1,fun)*7/365
dat_panel$we2005 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_2005',sep='')],1,fun)*7/365
dat_panel$we2006 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_2006',sep='')],1,fun)*7/365
dat_panel$we2007 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:8,sep='')),'_2007',sep='')],1,fun)*7/365
dat_panel$we2008 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:8,sep='')),'_2008',sep='')],1,fun)*7/365
dat_panel$we2009 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_2009',sep='')],1,fun)*7/365
dat_panel$we2010 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep='')),'_2010',sep='')],1,fun)*7/365
dat_panel$we2011 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10,11,12,13),'_2011',sep='')],1,fun)*7/365
dat_panel$we2013 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10),'_2013',sep='')],1,fun)*7/365
dat_panel$we2015 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10,11,12),'_2015',sep='')],1,fun)*7/365
dat_panel$we2017 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10:15),'_2017',sep='')],1,fun)*7/365
dat_panel$we2019 = 
  apply(dat_panel[,paste('CV_WKSWK_JOB_DLI.',
                         c(paste(0,1:9,sep=''),10,11),'_2019',sep='')],1,fun)*7/365
dat_panel$CV_MARSTAT_COLLAPSED_1997

for(year in c(1997:2011,c(2013,2015,2017,2019))){
  for(marstat in 0:4){
    dat_panel[,paste('marstat_',marstat,'_',year,sep='')] = 
      case_when(dat_panel[,paste('CV_MARSTAT_COLLAPSED_',year,sep=''),with=FALSE]==marstat~1)
  }
}

for(year in 1997:2009){
  temp = paste(substr(year+1,3,4),substr(year+2,3,4),sep='')
  dat_panel[,paste('edu',year,sep='')]=case_when(
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==0~0,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==1~4,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==2~12,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==3~15,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==4~16,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==5~18,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==6~23,
    dat_panel[,paste('CV_HIGHEST_DEGREE_',temp,'_',year+1,sep=''),with=FALSE]==7~21,
  )
}

for(year in c(2010, 2011, 2013,2015,2017,2019)){
  dat_panel[,paste('edu',year,sep='')]=case_when(
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==0~0,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==1~4,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==2~12,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==3~15,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==4~16,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==5~18,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==6~23,
    dat_panel[,paste('CV_HIGHEST_DEGREE_EVER_EDT_',year,sep=''),with=FALSE]==7~21,
  )
}

year = 1997
dat_panel2 = dat_panel[,c('PUBID_1997',paste(c('YINC-1700_','age_','edu','we',
                                         'marstat_0_','marstat_1_','marstat_2_',
                                         'marstat_3_','marstat_4_'),year,sep='')),with=FALSE]
colnames(dat_panel2) = c('id','income','age','edu','work_exp','Nevermarried','Married',
                   'Separated','Divorced','Widowed')
dat_panel2 = dat_panel2[!is.na(dat_panel2$income)&!is.na(dat_panel2$age)&
                         !is.na(dat_panel2$edu)&!is.na(dat_panel2$work_exp)]
dat_panel2 = dat_panel2[!is.na(dat_panel2$Nevermarried)|!is.na(dat_panel2$Married)|
                          !is.na(dat_panel2$Separated)|!is.na(dat_panel2$Divorced)|
                          !is.na(dat_panel2$Widowed)]
dat_panel2[is.na(dat_panel2)] = 0
dat_panel2$year = year
for(year in c(1998:2011,c(2013,2015,2017,2019))){
  temp = dat_panel[,c('PUBID_1997',paste(c('YINC-1700_','age_','edu','we',
                            'marstat_0_','marstat_1_','marstat_2_',
                            'marstat_3_','marstat_4_'),year,sep='')),with=FALSE]
  colnames(temp) = c('id','income','age','edu','work_exp','Nevermarried','Married',
                     'Separated','Divorced','Widowed')
  temp = temp[!is.na(temp$income)&!is.na(temp$age)&
                            !is.na(temp$edu)&!is.na(temp$work_exp)]
  temp = temp[!is.na(temp$Nevermarried)|!is.na(temp$Married)|
                            !is.na(temp$Separated)|!is.na(temp$Divorced)|
                            !is.na(temp$Widowed)]
  temp[is.na(temp)] = 0
  temp$year = year
  dat_panel2 = rbind(dat_panel2,temp)
}
dat_panel2 = dat_panel2[order(dat_panel2$id),]
## within
mean_panel = mutate(group_by(dat_panel2, id), 
                       income = mean(income),
                       age = mean(age),
                       edu = mean(edu),
                       work_exp = mean(work_exp),
                       Nevermarried = mean(Nevermarried), 
                       Married = mean(Married), 
                       Separated = mean(Separated), 
                       Divorced = mean(Divorced), 
                       Widowed = mean(Widowed),
                       year = mean(year))

within_panel = as.data.frame(dat_panel2) - as.data.frame(mean_panel)
within_results = lm(income~age+edu+work_exp+Married+Separated+Divorced+Widowed-1,data=within_panel)
summary(within_results)

## between
between = summarise(group_by(dat_panel2, id), 
                 income = mean(income),
                 age = mean(age),
                 edu = mean(edu),
                 work_exp = mean(work_exp),
                 Nevermarried = mean(Nevermarried), 
                 Married = mean(Married), 
                 Separated = mean(Separated), 
                 Divorced = mean(Divorced), 
                 Widowed = mean(Widowed),
                 year = mean(year))
between = as.data.frame(between)
between_results = lm(income~age+edu+work_exp+Married+Separated+Divorced+Widowed,data=between)
summary(between_results)

## difference
diff_panel = mutate(group_by(dat_panel2, id), 
                    income = lag(income,n=1,default = NA),
                    age = lag(age,n=1,default = NA),
                    edu = lag(edu,n=1,default = NA),
                    work_exp = lag(work_exp,n=1,default = NA),
                    Nevermarried = lag(Nevermarried,n=1,default = NA), 
                    Married = lag(Married,n=1,default = NA), 
                    Separated = lag(Separated,n=1,default = NA), 
                    Divorced = lag(Divorced,n=1,default = NA), 
                    Widowed = lag(Widowed,n=1,default = NA),
                    year = lag(year,n=1,default = NA))
diff_panel = na.omit(as.data.frame(dat_panel2) - as.data.frame(diff_panel))
diff_results = lm(income~age+edu+work_exp+Married+Separated+Divorced+Widowed-1,data=diff_panel)
summary(diff_results)

dat_r = data.frame(within = c('',within_results$coefficients),
                   between = between_results$coefficients,
                   diff = c('',diff_results$coefficients))
