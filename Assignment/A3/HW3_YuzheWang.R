require(stats)
require(base)
library(ggplot2)
library(patchwork)
library(data.table)
library(bit64)
library(dplyr)

setwd("C:\\Users\\wyz_m\\Desktop\\DUKE\\courses\\ECON 613\\assignment\\A3")
getwd()

# Exercise 1 Basic Statistics
datstu = fread('./data/datstu_v2.csv')
datjss = fread('./data/datjss.csv')
datsss = fread('./data/datsss.csv')

## Number of students, schools, programs
stu_number = dim(datstu)[1]
sch_number = length(unique(c(datstu$schoolcode1, datstu$schoolcode2, 
                             datstu$schoolcode3, datstu$schoolcode4, 
                             datstu$schoolcode5, datstu$schoolcode6))) - 1 # minus NA
pgm_number = length(unique(c(datstu$choicepgm1, datstu$choicepgm2, 
                             datstu$choicepgm3, datstu$choicepgm4, 
                             datstu$choicepgm5, datstu$choicepgm6)))
stu_number
sch_number
pgm_number

## Number of choices (school, program)
cho_number = length(unique(c(paste(datstu$schoolcode1,datstu$choicepgm1,sep=''),
                             paste(datstu$schoolcode2,datstu$choicepgm2,sep=''),
                             paste(datstu$schoolcode3,datstu$choicepgm3,sep=''),
                             paste(datstu$schoolcode4,datstu$choicepgm4,sep=''),
                             paste(datstu$schoolcode5,datstu$choicepgm5,sep=''),
                             paste(datstu$schoolcode6,datstu$choicepgm6,sep=''))))
cho_number  # with NA

## Number of students applying to at least one senior high schools in the same district to home
datsss = datsss[!duplicated(datsss$schoolcode),]
datsss2 = datsss[datsss$sssdistrict != '',]

temp = as.data.frame(matrix(nrow=dim(datstu)[1],ncol=6))
colnames(temp) = paste('schoolcode',1:6,sep = '')

for(i in 5:10){
  dat_l = datstu[,c(1,i,17),with=FALSE]
  dat_l[is.na(dat_l)] = 0
  colnames(dat_l) = c('V1','schoolcode','jssdistrict')
  a = merge(dat_l, datsss2,
            by = 'schoolcode',
            all.x = T)
  a[is.na(a)] = 0
  a = a[order(a$V1.x)]
  c = as.numeric(a$jssdistrict == a$sssdistrict)
  temp[paste('schoolcode', i-4,sep='')] = c
}
app_same_dis = sum(as.numeric(apply(temp,1,sum)>=1))
app_same_dis

## Number of students each senior high school admitted
temp = datstu[,c(5,6,7,8,9,10,18)]
temp$na = c(rep(0,dim(temp)[1]))
temp = temp[,c(1,2,3,4,5,6,8,7)]
temp[is.na(temp)] = 7
ad = apply(temp, 1, function(x) return(x[x[8]]))
ad2 = ad[ad!=0]
admit_num = data.frame(table(ad2))
admit_num$schoolcode = as.numeric(as.character(admit_num$ad2))
admit_num = admit_num[,c(3,2)]
colnames(admit_num) = c('schoolcode','number_admitted_school')
admit_num[1:10,]

## The cutoff of senior high schools (the lowest score to be admitted)s
## The quality of senior high schools (the average score of students admitted)
sc = datstu$score
ad_sc_dat = data.frame('admitted'=ad,'score'=sc)
ad_sc_dat = na.omit(ad_sc_dat)
ad_sc_dat = ad_sc_dat[ad_sc_dat$admitted!=0,]
cutoff = by(ad_sc_dat$score,ad_sc_dat$admitted,min)
cutoff = data.frame(schoolcode = as.numeric(names(cutoff)), 'cutoff_school' = matrix(cutoff))
quality = by(ad_sc_dat$score,ad_sc_dat$admitted,mean)
quality = data.frame(schoolcode = as.numeric(names(quality)), 'quality_school' = matrix(quality))
cutoff[1:10,]
quality[1:10,]

# Exercise 2 Data
choices = data.frame('choice1' = paste(datstu$schoolcode1,datstu$choicepgm1,sep='-'),
                     'choice2' = paste(datstu$schoolcode2,datstu$choicepgm2,sep='-'),
                     'choice3' = paste(datstu$schoolcode3,datstu$choicepgm3,sep='-'),
                     'choice4' = paste(datstu$schoolcode4,datstu$choicepgm4,sep='-'),
                     'choice5' = paste(datstu$schoolcode5,datstu$choicepgm5,sep='-'),
                     'choice6' = paste(datstu$schoolcode6,datstu$choicepgm6,sep='-'),
                     'score' = datstu$score,
                     'admit' = datstu$rankplace)
## Number of students each choice admitted
temp = choices[,c(1,2,3,4,5,6,8)]
temp$na = c(rep(0,dim(temp)[1]))
temp = temp[,c(1,2,3,4,5,6,8,7)]
temp[is.na(temp)] = 7
ad = apply(temp, 1, function(x) return(x[as.numeric(x[8])]))
ad2 = ad[ad!=0]
admit_num_choice = data.frame(table(ad2))
admit_num_choice$choices = as.character(admit_num_choice$ad2)
admit_num_choice = admit_num_choice[,c(3,2)]
colnames(admit_num_choice) = c('choices','number_admitted_choices')
admit_num_choice[1:10,]
## The cutoff of senior high schools (the lowest score to be admitted)s
## The quality of senior high schools (the average score of students admitted)
sc = choices$score
ad_sc_dat = data.frame('admitted'=ad,'score'=sc)
ad_sc_dat = na.omit(ad_sc_dat)
ad_sc_dat = ad_sc_dat[ad_sc_dat$admitted!=0,]
cutoff_choice = by(ad_sc_dat$score,ad_sc_dat$admitted,min)
cutoff_choice = data.frame(choices = names(cutoff_choice), 'cutoff_choice' = matrix(cutoff_choice))
quality_choice = by(ad_sc_dat$score,ad_sc_dat$admitted,mean)
quality_choice = data.frame(choices = names(quality_choice), 'quality_choice' = matrix(quality_choice))

school_dat = data.frame('choices' = unique(c(paste(datstu$schoolcode1,datstu$choicepgm1,sep='-'),
                                             paste(datstu$schoolcode2,datstu$choicepgm2,sep='-'),
                                             paste(datstu$schoolcode3,datstu$choicepgm3,sep='-'),
                                             paste(datstu$schoolcode4,datstu$choicepgm4,sep='-'),
                                             paste(datstu$schoolcode5,datstu$choicepgm5,sep='-'),
                                             paste(datstu$schoolcode6,datstu$choicepgm6,sep='-'))))
school_dat$schoolcode = apply(school_dat,1,function(x) return(as.numeric(strsplit(x,'-')[[1]][1])))
school_dat$program = apply(school_dat,1,function(x) return(strsplit(x,'-')[[1]][2]))
school_dat = merge(school_dat, datsss,by = 'schoolcode')%>%
  merge(admit_num,by='schoolcode')%>%
  merge(cutoff,by = 'schoolcode')%>%
  merge(quality,by = 'schoolcode')%>%
  merge(admit_num_choice,by='choices')%>%
  merge(cutoff_choice,by = 'choices')%>%
  merge(quality_choice,by = 'choices')
school_dat = school_dat[,-4]
school_dat[1:10,]

# Exercise 3 Distance
dist = datstu[,c(1,5,6,7,8,9,10,17)]
dat_r = datsss[,c('schoolcode','ssslong','ssslat')]
dist = merge(dist, datjss, by='jssdistrict',all.x = T)
dist = dist[,-9]
dist = merge(dist, dat_r, by.x = 'schoolcode1', by.y = 'schoolcode',all.x = T)
colnames(dist)[11:12] = paste(colnames(dist)[11:12],'_1',sep = '')
dist = merge(dist, dat_r, by.x = 'schoolcode2', by.y = 'schoolcode',all.x = T)
colnames(dist)[13:14] = paste(colnames(dist)[13:14],'_2',sep = '')
dist = merge(dist, dat_r, by.x = 'schoolcode3', by.y = 'schoolcode',all.x = T)
colnames(dist)[15:16] = paste(colnames(dist)[15:16],'_3',sep = '')
dist = merge(dist, dat_r, by.x = 'schoolcode4', by.y = 'schoolcode',all.x = T)
colnames(dist)[17:18] = paste(colnames(dist)[17:18],'_4',sep = '')
dist = merge(dist, dat_r, by.x = 'schoolcode5', by.y = 'schoolcode',all.x = T)
colnames(dist)[19:20] = paste(colnames(dist)[19:20],'_5',sep = '')
dist = merge(dist, dat_r, by.x = 'schoolcode6', by.y = 'schoolcode',all.x = T)
colnames(dist)[21:22] = paste(colnames(dist)[21:22],'_6',sep = '')

dist_fun = function(x){
  jsslong = x[1]
  jsslat =x[2]
  ssslong = x[3]
  ssslat = x[4]
  return(sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2+(69.172*(ssslat-jsslat)^2)))
}
dist$distance1 = apply(dist[,c('point_x','point_y','ssslong_1','ssslat_1')],1,dist_fun)
dist$distance2 = apply(dist[,c('point_x','point_y','ssslong_2','ssslat_2')],1,dist_fun)
dist$distance3 = apply(dist[,c('point_x','point_y','ssslong_3','ssslat_3')],1,dist_fun)
dist$distance4 = apply(dist[,c('point_x','point_y','ssslong_4','ssslat_4')],1,dist_fun)
dist$distance5 = apply(dist[,c('point_x','point_y','ssslong_5','ssslat_5')],1,dist_fun)
dist$distance6 = apply(dist[,c('point_x','point_y','ssslong_6','ssslat_6')],1,dist_fun)
dist = dist[order(dist$V1.x)]
dist = dist[,c('V1.x',paste('schoolcode',1:6,sep=''),paste('distance',1:6,sep = ''))]
dist[1:10,]

# Exercise 4 Dimensionality Reduction
## Recode the schoolcode into its first three digits (substr). Call this new variable scode rev.
to_blank = function(x){
  if(x == '' | is.na(x)){return('')}else{return(x)}
}
datstu$scode_rev1 = sapply(substr(datstu$schoolcode1,1,3),to_blank)
datstu$scode_rev2 = sapply(substr(datstu$schoolcode2,1,3),to_blank)
datstu$scode_rev3 = sapply(substr(datstu$schoolcode3,1,3),to_blank)
datstu$scode_rev4 = sapply(substr(datstu$schoolcode4,1,3),to_blank)
datstu$scode_rev5 = sapply(substr(datstu$schoolcode5,1,3),to_blank)
datstu$scode_rev6 = sapply(substr(datstu$schoolcode6,1,3),to_blank)

## Recode the program variable into 4 categories:
to_cat = function(x){
  y = 'others'
  if(x == 'General Arts' | x == 'Visual Arts'){y = 'arts'}
  if(x == 'Business' | x == 'Home Economics'){y = 'economics'}
  if(x == 'General Arts' | x == 'General Science'){y = 'science'}
  return(y)
}

datstu$pgm_rev1 = sapply(datstu$choicepgm1,to_cat)
datstu$pgm_rev2 = sapply(datstu$choicepgm2,to_cat)
datstu$pgm_rev3 = sapply(datstu$choicepgm3,to_cat)
datstu$pgm_rev4 = sapply(datstu$choicepgm4,to_cat)
datstu$pgm_rev5 = sapply(datstu$choicepgm5,to_cat)
datstu$pgm_rev6 = sapply(datstu$choicepgm6,to_cat)

## Create a new choice variable choice rev.
datstu$choice_rev1 = paste(datstu$scode_rev1,datstu$pgm_rev1,sep='-')
datstu$choice_rev2 = paste(datstu$scode_rev2,datstu$pgm_rev1,sep='-')
datstu$choice_rev3 = paste(datstu$scode_rev3,datstu$pgm_rev1,sep='-')
datstu$choice_rev4 = paste(datstu$scode_rev4,datstu$pgm_rev1,sep='-')
datstu$choice_rev5 = paste(datstu$scode_rev5,datstu$pgm_rev1,sep='-')
datstu$choice_rev6 = paste(datstu$scode_rev6,datstu$pgm_rev1,sep='-')

## Recalculate the cutoff and the quality for each recoded choice.
temp = datstu[,c(31,32,33,34,35,36,18)]
temp$na = c(rep(0,dim(temp)[1]))
temp = temp[,c(1,2,3,4,5,6,8,7)]
temp[is.na(temp)] = 7
ad = apply(temp, 1, function(x) return(x[as.numeric(x[8])]))
sc = datstu$score
ad_sc_dat = data.frame('admitted'=ad,'score'=sc)
ad_sc_dat = na.omit(ad_sc_dat)
ad_sc_dat = ad_sc_dat[ad_sc_dat$admitted!=0,]
cutoff_choice = by(ad_sc_dat$score,ad_sc_dat$admitted,min)
cutoff_choice = data.frame(choices = names(cutoff_choice), 'cutoff_choice' = matrix(cutoff_choice))
quality_choice = by(ad_sc_dat$score,ad_sc_dat$admitted,mean)
quality_choice = data.frame(choices = names(quality_choice), 'quality_choice' = matrix(quality_choice))
cutoff_choice[1:20,]
quality_choice[1:20,]

## Consider the 20,000 highest score students.
dist = dist[,c('V1.x',paste('distance',1:6,sep=''))]
datstu = merge(datstu,dist,by.x = 'V1', by.y = 'V1.x',all.x = T)
colnames(datstu)[37:42] = paste(paste('choice_rev',1:6,sep=''),'distance',sep='.')

datstu = merge(datstu,cutoff_choice,by.x='choice_rev1',by.y='choices')
colnames(datstu)[43] = 'choice_rev1.cutoff'
datstu = merge(datstu,cutoff_choice,by.x='choice_rev2',by.y='choices')
colnames(datstu)[44] = 'choice_rev2.cutoff'
datstu = merge(datstu,cutoff_choice,by.x='choice_rev3',by.y='choices')
colnames(datstu)[45] = 'choice_rev3.cutoff'
datstu = merge(datstu,cutoff_choice,by.x='choice_rev4',by.y='choices')
colnames(datstu)[46] = 'choice_rev4.cutoff'
datstu = merge(datstu,cutoff_choice,by.x='choice_rev5',by.y='choices')
colnames(datstu)[47] = 'choice_rev5.cutoff'
datstu = merge(datstu,cutoff_choice,by.x='choice_rev6',by.y='choices')
colnames(datstu)[48] = 'choice_rev6.cutoff'

datstu = merge(datstu,quality_choice,by.x='choice_rev1',by.y='choices')
colnames(datstu)[49] = 'choice_rev1.quality'
datstu = merge(datstu,quality_choice,by.x='choice_rev2',by.y='choices')
colnames(datstu)[50] = 'choice_rev2.quality'
datstu = merge(datstu,quality_choice,by.x='choice_rev3',by.y='choices')
colnames(datstu)[51] = 'choice_rev3.quality'
datstu = merge(datstu,quality_choice,by.x='choice_rev4',by.y='choices')
colnames(datstu)[52] = 'choice_rev4.quality'
datstu = merge(datstu,quality_choice,by.x='choice_rev5',by.y='choices')
colnames(datstu)[53] = 'choice_rev5.quality'
datstu = merge(datstu,quality_choice,by.x='choice_rev6',by.y='choices')
colnames(datstu)[54] = 'choice_rev6.quality'

datstu = datstu[order(-score)][1:20000,]
datstu[1:10,]

#Exercise 5 First Model
dat = datstu[,c(paste('choice_rev',1:6,sep=''),'score','agey','male',
                paste(paste('choice_rev',1:6,sep=''),'distance',sep='.'),
                paste(paste('choice_rev',1:6,sep=''),'cutoff',sep='.'),
                paste(paste('choice_rev',1:6,sep=''),'quality',sep='.'),'jssdistrict')]

dat_ex5 = dat[dat$choice_rev1 %in% names(table(dat$choice_rev1))[table(dat$choice_rev1)>1],
              c('choice_rev1','score','agey','male','choice_rev1.quality','choice_rev1.cutoff','jssdistrict')]

dat_ex5 = merge(dat_ex5,datjss[,c(2,3,4)],by='jssdistrict')

cutoff_choice = cutoff_choice[cutoff_choice$choices %in% unique(dat_ex5$choice_rev1),]
quality_choice = quality_choice[quality_choice$choices %in% unique(dat_ex5$choice_rev1),]

datsss3 = datsss[,c('schoolcode','ssslong', 'ssslat')]
datsss3 = na.omit(datsss3)
datsss3$schoolcode = sapply(datsss3$schoolcode, function(x){return(substr(x,1,3))})
cutoff_choice$schoolcode = sapply(cutoff_choice$choices, function(x){return(substr(x,1,3))})
cutoff_choice = merge(cutoff_choice,
                      summarize(group_by(datsss3,schoolcode),long=mean(ssslong),lat=mean(ssslat)),
                      by='schoolcode')

dist_fun2 = function(x,ssslong,ssslat){
  jsslong = x[1]
  jsslat =x[2]
  return(sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2+(69.172*(ssslat-jsslat)^2)))
}

for(ch in cutoff_choice$choices){
  ssslong = cutoff_choice[cutoff_choice$choices==ch,'long']
  ssslat = cutoff_choice[cutoff_choice$choices==ch,'lat']
  dat_ex5[,paste('distance',ch,sep ='.')] = 
    apply(dat_ex5[,c('point_x','point_y')],1,function(x) return(dist_fun2(x,ssslong,ssslat)))
}

for(ch in  cutoff_choice$choices){
  dat_ex5[,paste('cutoff',ch,sep ='.')] = 
    cutoff_choice[cutoff_choice$choices==ch,'cutoff_choice']
}

for(ch in  quality_choice$choices){
  dat_ex5[,paste('quality',ch,sep ='.')] = 
    quality_choice[quality_choice$choices==ch,'quality_choice']
}
dat_ex5 = dat_ex5[,-c(1,8,9)]
temp = data.frame(choices = cutoff_choice$choices, ch = 1:nrow(cutoff_choice))
dat_ex5 = merge(dat_ex5,temp,by.x = 'choice_rev1',by.y = 'choices')
dat_ex5 = na.omit(dat_ex5)
dat_ex5[1,]

## build the log likelihood function
rm(dat)
like_fun = function(beta, dat, choice_number, 
                    conditional_v_num, conditional_v_start,
                    multinomial_v_num, multinomial_v_start,model_type){
  N = nrow(dat)
  pij = mat.or.vec(N,choice_number)
  ch = dat$ch
  if(model_type == 'conditional'){
    pij[,1] = exp(0 + apply(dat[,conditional_v_start + 
                                  choice_number * seq(0,conditional_v_num-1),
                                with=FALSE]*beta[seq(choice_number,
                                                     choice_number+conditional_v_num-1)],
                            1,sum))
    for(j in seq(1,choice_number-1)){
      pij[,j+1] = exp(beta[j] + 
                  apply(dat[,conditional_v_start + 
                            choice_number * seq(0,conditional_v_num-1) + j,
                          with=FALSE] * beta[seq(choice_number,
                                                 choice_number+conditional_v_num-1)],
                        1,sum))
    }
  }
  if(model_type == 'multinomial'){
    pij[,1] = 1
    for(j in seq(1,choice_number-1)){
      pij[,j+1] = exp(beta[j] + 
                      apply(dat[,seq(multinomial_v_start, 
                                         multinomial_v_start + multinomial_v_num - 1),
                                    with=FALSE]* 
                              beta[choice_number + j - 1 + 
                                     choice_number * seq(0,multinomial_v_num-1)],1,sum))
    }
  }
  prob   = sweep(pij,MARGIN=1,FUN="/",STATS=rowSums(pij))
  probc = NULL
  for (i in 1:N)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

prob_fun = function(beta, dat, choice_number, 
                    conditional_v_num, conditional_v_start,
                    multinomial_v_num, multinomial_v_start,model_type){
  N = nrow(dat)
  pij = mat.or.vec(N,choice_number)
  ch = dat$ch
  if(model_type == 'conditional'){
    pij[,1] = exp(0 + apply(dat[,conditional_v_start + 
                                  choice_number * seq(0,conditional_v_num-1),
                                with=FALSE]*
                              beta[seq(choice_number,choice_number+conditional_v_num-1)],
                            1,sum))
    for(j in seq(1,choice_number-1)){
      pij[,j+1] = exp(beta[j] + 
                        apply(dat[,conditional_v_start + 
                                    choice_number * seq(0,conditional_v_num-1) + j,
                                  with=FALSE] * 
                                beta[seq(choice_number,choice_number+conditional_v_num-1)],
                              1,sum))
    }
  }
  if(model_type == 'multinomial'){
    pij[,1] = 1
    for(j in seq(1,choice_number-1)){
      pij[,j+1] = exp(beta[j] + 
                        apply(dat[,seq(multinomial_v_start, 
                                       multinomial_v_start + multinomial_v_num - 1),
                                  with=FALSE]* 
                                beta[choice_number + j - 1 + 
                                       choice_number * seq(0,multinomial_v_num-1)],1,sum))
    }
  }
  prob   = sweep(pij,MARGIN=1,FUN="/",STATS=rowSums(pij))
  probc = NULL
  for (i in 1:N)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(prob)
}


# Exercise 5 First Model
coe_ex5_start = fread('./ex5.csv')
start = c(coe_ex5_start$`(Intercept)`,coe_ex5_start$score)

choice_name = unique(dat_ex5$choice_rev1)[2:180]
capture.output(res <- optim(start,
             fn=like_fun,
             method="BFGS",
             control=list(trace=6,maxit=100),
             dat=dat_ex5, 
             choice_number=180, 
             conditional_v_num=3, 
             conditional_v_start=7,
             multinomial_v_num=1, 
             multinomial_v_start=2,
             model_type='multinomial'))
res = list(par = start, value = like_fun(start,
                                         dat=dat_ex5, 
                                         choice_number=180, 
                                         conditional_v_num=3, 
                                         conditional_v_start=7,
                                         multinomial_v_num=1, 
                                         multinomial_v_start=2,
                                         model_type='multinomial'))
coe_ex5 = res$par
names(coe_ex5) = c(paste(choice_name,'intercept',sep='.'), paste(choice_name,'score',sep='.'))
like_ex5 = res$value

prob_ex5 = prob_fun(res$par,
                    dat=dat_ex5, 
                    choice_number=180, 
                    conditional_v_num=3, 
                    conditional_v_start=7,
                    multinomial_v_num=1, 
                    multinomial_v_start=2,
                    model_type='multinomial')

beta_i_bar =  apply(prob_ex5[,2:180],1,function(x) return(sum(x * res$par[180:358])))
me_ex5 = data.frame(prob_ex5[,2:180] * res$par[180:358] - prob_ex5[,2:180] * beta_i_bar)
  
me_ex5 = apply(me_ex5,2,mean)
names(me_ex5) = paste(choice_name,'score',sep='.')

coe_ex5 # coefficient
me_ex5 # ME
like_ex5 # likelihood


# Exercise 6 Second Model
coe_ex6_start = fread('./ex6.csv')
start = coe_ex6_start$x

dat_ex6 = dat_ex5
capture.output(res <- optim(start,
             fn=like_fun,
             method="L-BFGS-B",
             control=list(trace=6,maxit=100),
             dat=dat_ex6, 
             choice_number=180, 
             conditional_v_num=3, 
             conditional_v_start=7,
             multinomial_v_num=1, 
             multinomial_v_start=2,
             model_type='conditional'))
res = list(par=start,value=like_fun(start,
                    dat=dat_ex6, 
                    choice_number=180, 
                    conditional_v_num=1, 
                    conditional_v_start=367,
                    multinomial_v_num=1, 
                    multinomial_v_start=2,
                    model_type='conditional'))
coe_ex6 = res$par[180]
like_ex6 = res$value

prob_ex6 = prob_fun(start,
                    dat=dat_ex6, 
                    choice_number=180, 
                    conditional_v_num=1, 
                    conditional_v_start=367,
                    multinomial_v_num=1, 
                    multinomial_v_start=2,
                    model_type='conditional')


delta_ijk = mat.or.vec(nrow(dat_ex6),180)
for(i in 1:nrow(dat_ex6)){
  delta_ijk[i,dat_ex6$ch[i]] = 1
}
pik = mat.or.vec(nrow(dat_ex6),180)
for(i in 1:nrow(dat_ex6)){
  pik[i,] = prob_ex6[i,dat_ex6$ch[i]]
}
me_ex6_mat = prob_ex6 * (delta_ijk - pik) * res$par[180]
me_ex6 = mat.or.vec(180,1)
for(j in 1:180){
  me_ex6[j] = unique(me_ex6_mat[dat_ex6$ch == j,j])
}
names(me_ex6) = unique(dat_ex6$choice_rev1)

coe_ex6 # coefficient of quality
me_ex6 # ME


# Exercise 7 Counterfactual simulations
# I think the second model is more appropriate. 
# The reason is that because the we are comparing different effect of score among different choices.
# When we exclude "Others" in the first model, there are fewer choices and what we are comparing is changing.
# However, the effect of quality is less likely to change when there are fewer choices,
# because the quality is the characteristic of choice (school-pgm) itself.

## Calculate choice probabilities under the appropriate model
dat_ex7 = dat_ex6
dat_ex7$pgm = sapply(dat_ex6$choice_rev1,function(x) return(strsplit(x,'-')[[1]][2]))
others_position = dat_ex7$pgm == 'others'
dat_ex7 = dat_ex7[dat_ex7$pgm != 'others',]
choice_name = unique(dat_ex6$choice_rev1)
choice_name_pgm = sapply(choice_name,function(x) return(strsplit(x,'-')[[1]][2]))
choice_name_pgm_location = (choice_name_pgm != 'others') * 1:180
choice_name_pgm_location = choice_name_pgm_location[choice_name_pgm_location != 0]
choice_name_pgm_location = c(6 + choice_name_pgm_location, 
                             186 + choice_name_pgm_location,
                             366 + choice_name_pgm_location)
dat_ex7 = dat_ex7[,c(1:6,choice_name_pgm_location),with=FALSE]
temp = data.frame(choices = unique(dat_ex7$choice_rev1), ch = 1:length(unique(dat_ex7$choice_rev1)))
dat_ex7 = merge(dat_ex7,temp,by.x = 'choice_rev1',by.y = 'choices')


coe_ex7_start = fread('./ex7_2.csv')
start = coe_ex7_start$x

prob_ex7 = prob_fun(start,
                    dat=dat_ex7, 
                    choice_number=146, 
                    conditional_v_num=1, 
                    conditional_v_start=299,
                    multinomial_v_num=1, 
                    multinomial_v_start=2,
                    model_type='conditional')
prob_each_choice_ex7 = prob_ex7[1,]
names(prob_each_choice_ex7) = unique(dat_ex7$choice_rev1)
prob_each_choice_ex7
sum(prob_each_choice_ex7) == 1

## Simulate how these choice probabilities change when these choices are excluded.
prob_each_choice_ex6 = prob_ex6[1,]
names(prob_each_choice_ex6) = unique(dat_ex6$choice_rev1)
prob_each_choice_ex6
sum(prob_each_choice_ex6) == 1

prob_change = prob_each_choice_ex7 - prob_each_choice_ex6[unique(dat_ex7$choice_rev1)]
prob_change
