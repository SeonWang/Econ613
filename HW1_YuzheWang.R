library(ggplot2)
library(patchwork)
library(data.table)
library(bit64)
library(dineq)
install.packages("dineq")
setwd("C:\\Users\\wyz_m\\Desktop\\DUKE\\courses\\ECON 613\\assignment\\A1")
getwd()

####### Exercise 1 Basic Statistics
# Number of households surveyed in 2007
dathh2007 = fread('./data/dathh2007.csv')
length(unique(na.omit(dathh2007$idmen))) # 10498

# Number of households with a marital status "Couple with kids" in 2005.
dathh2005 = fread('./data/dathh2005.csv')
length(dathh2005$mstatus[dathh2005$mstatus=="Couple, with Kids"]) # 3374

# Number of individuals surveyed in 2008
datind2008 = fread('./data/datind2008.csv')
length(unique(na.omit(datind2008$idind))) # 25510

# Number of individuals aged between 25 and 35 in 2016
datind2016 = fread('./data/datind2016.csv')
length(which(datind2016$age>=25 & datind2016$age<=35))

# Cross-table gender/profession in 2009
datind2009 = fread('./data/datind2009.csv')
table(datind2009$gender, datind2009$profession)

# Distribution of wages in 2005 and 2019
datind2005 = na.omit(fread('./data/datind2005.csv')[,c('idind','wage','year')])
datind2019 = na.omit(fread('./data/datind2019.csv')[,c('idind','wage','year')])
datind2019$idind = as.integer64(datind2019$idind)

dat = rbind(datind2005,datind2019)
dat$year = factor(dat$year)

fun = function(x){
  gini = sum(abs(outer(x,x,"-")))/(2*length(x)^2*mean(x))
  vec=round(c(mean(x),sd(x),quantile(x,0.9)/quantile(x,0.1),gini,max(x)),4)
  names(vec)=c("mean","sd","D9/D1","Gini_coefficient","max")
  return(vec)
  }
by(dat$wage,dat$year,fun)

ggplot(data=dat,aes(wage,fill=year))+
  geom_histogram(binwidth=10000,alpha=0.5)+
  facet_wrap(.~year,1,2, scales="free")+
  ggtitle(label='Wage Distribution in 2005&2019')

# drop some people that have zero wage
dat = dat[dat$wage!=0,] 
by(dat$wage,dat$year,fun)
ggplot(data=dat,aes(wage,fill=year))+
  geom_histogram(binwidth=10000,alpha=0.5)+
  facet_wrap(.~year,1,2, scales="free")+
  ggtitle(label='Wage Distribution in 2005&2019(exclude wage=0)')


# Distribution of age in 2010
datind2010 = fread('./data/datind2010.csv')
datind2010_age = na.omit(datind2010[,c('idind','age','gender')])
summary(datind2010_age)
skewness(datind2010_age$age)
kurtosis(datind2010_age$age)

datind2010_age$gender = factor(datind2010_age$gender)
by(datind2010_age$age,datind2010_age$gender,summary)
by(datind2010_age$age,datind2010_age$gender,skewness)
by(datind2010_age$age,datind2010_age$gender,kurtosis)

ggplot(data=datind2010_age,aes(age))+
  geom_histogram(aes(y=..density..),fill="white",color="black",binwidth=5)+
  geom_density(fill="red",color="black",alpha=.2)+
  ggtitle(label='Total age distribution')

ggplot(data=datind2010_age,aes(age,fill=gender))+
  geom_histogram(binwidth=5,color="black",alpha=0.5)+
  facet_grid(.~gender)+
  ggtitle(label='Age distribution by gender in 2010')

# Number of individuals in Paris in 2011.
datind2011 = fread('./data/datind2011.csv')
dathh2011 = fread('./data/dathh2011.csv')
dat2011 = merge(datind2011,dathh2011,by=c("idmen", "year" ),all=T)
dat2011$idind = factor(dat2011$idind)
sum(by(dat2011$location, dat2011$idind, function(x) "Paris" %in% x)>0)


####### Exercise 2 Merge Datasets

# Read all individual datasets from 2004 to 2019. Append all these datasets
require(dplyr)
datind_dirs = paste("./data/datind",2004:2019,sep = "")%>%paste(".csv",sep = "")
datind_total = fread(datind_dirs[1])
for (i in c(2:length(datind_dirs))){
  dat_temp = fread(datind_dirs[i])
  dat_temp$idind = as.integer64(dat_temp$idind)
  datind_total = rbind(datind_total, dat_temp)
}
datind_total = datind_total[,-1]

# Read all household datasets from 2004 to 2019. Append all these datasets
dathh_dirs = paste("./data/dathh",2004:2019,sep = "")%>%paste(".csv",sep = "")
dathh_total = fread(dathh_dirs[1])
for (i in c(2:length(dathh_dirs))){
  dat_temp = fread(dathh_dirs[i])
  dathh_total = rbind(dathh_total, dat_temp)
}
dathh_total = dathh_total[,-1]

# List the variables that are simultaneously present in the individual and household datasets
colnames(datind_total)
colnames(dathh_total)
same_var = c()
for (datind_name in colnames(datind_total)){
  for (dathh_name in colnames(dathh_total)){
    if (datind_name == dathh_name){
      same_var = append(same_var,datind_name)
    }
  }
}
same_var

# Merge the appended individual and household datasets, 
# Drop the cases that are totally the same (including year, idind, idmen, and ......)
# If individul's idmen is not in all dathh, we still keep it, but with NAs in all dathh characteristics
dat_total = data.frame(distinct(merge(datind_total, dathh_total, by = same_var, all = T),.keep_all=T))

# Number of households in which there are more than four family members
dat_total$idmen = factor(dat_total$idmen)
dat_total$year = factor(dat_total$year)
sum(by(dat_total$idind,dat_total$idmen,function(x) length(unique(x)))>4) # find the number of household whose number of members > 4 in the whole history.

member = by(dat_total$idind,dat_total[c('idmen','year')],function(x) length(unique(x)))
member[is.na(member)] = 0
sum(apply(member,MARGIN = 1, function(x) sum(x>4)!=0)) # find the number of household once number of members > 4 in some years.

# find the number of household once number members of which > 4 in each year.
data.frame(year = 2004:2019, number = apply(member, MARGIN = 2, function(x) sum(x>4))) %>%
  ggplot(aes(year,number))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=number, y=number+10), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label=' Number of households in which there are more than four family members') 
sum(apply(member, MARGIN = 2, function(x) sum(x>4)))

# Number of households in which at least one member is unemployed
sum(by(dat_total$empstat,dat_total$idmen,function(x)'Unemployed' %in% x)) # find the household once number of which at least one member is unemployed in some years.

emp = by(dat_total$empstat,dat_total[c('idmen','year')],function(x)'Unemployed' %in% x)
emp[is.na(emp)] = 0
data.frame(year = 2004:2019, number = apply(emp,2,sum)) %>% 
  ggplot(aes(year,number))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=number, y=number+10), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='Number of households in which at least one member is unemployed')# find the number of household which at least one member is unemployed in each year.
sum(apply(emp,2,sum))

# Number of households in which at least two members are of the same profession
prof = dat_total[!is.na(dat_total$profession) & dat_total$profession != "",]
prof = by(prof$profession,prof[c('idmen','year')],function(x) length(x)-length(unique(x)))>0
prof[is.na(prof)] = 0

sum(apply(prof,MARGIN = 1, function(x) sum(x>0)!=0)) # find the number of household once in which at least two members are of the same profession in some years.

data.frame(year = 2004:2019, number = apply(prof,2,function(x) sum(x>0)))%>%
   ggplot(aes(year,number))+
   geom_bar(stat = 'identity')+
   geom_text(aes(label=number, y=number+10), position="dodge", ,size = 8/.pt) +
   ggtitle(label=' Number of households in which at least two members are of the same profession') # find the number of household once in which at least two members are of the same profession in each year
sum(apply(prof,2,function(x) sum(x>0)))

# Number of individuals in the panel that are from household-Couple with kids
dat_total$idind = factor(dat_total$idind)
sum(by(dat_total$mstatus,dat_total$idind, function(x) "Couple, with Kids" %in% x)) # find the individuals once who are from household-Couple with kids in some years

mstatus_ind = by(dat_total$mstatus,dat_total[c("idind","year")], function(x) "Couple, with Kids" %in% x)
mstatus_ind[is.na(mstatus_ind)] = 0
data.frame(year = 2004:2019, number = apply(mstatus_ind,2,sum))%>%
  ggplot(aes(year,number))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=number, y=number+100), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='Number of individuals in the panel that are from household-Couple with kids') # find the individuals once who are from household-Couple with kids in each year.

# Number of individuals in the panel that are from Paris
sum(by(dat_total$location,dat_total$idind, function(x) "Paris" %in% x)) # find the number of individuals that are from Paris once he is from Paris in some year

location_ind = by(dat_total$location,dat_total[c("idind","year")], function(x) "Paris" %in% x)
location_ind[is.na(location_ind)] = 0
data.frame(year = 2004:2019, number = apply(location_ind,2,sum))%>%
  ggplot(aes(year,number))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=number, y=number+10), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='Number of individuals in the panel that are from Paris')# find the number of individuals that are from Paris in each year

# Find the household with the most number of family members. Report its idmen
rownames(member[which(member == max(member)) %% length(rownames(member)),]) # Find the household with the most number of family members in the whole history

# Find the household with the most number of family members in each year
max_member = apply(member,2,max) 
max_member
max_member_list = list()
for (i in 1:length(colnames(member))){
  for (j in 1:length(rownames(member))){
    if (member[j,i] == max_member[i]){
      if (i-1==length(max_member_list)){
        max_member_list[[i]] = paste(rownames(member)[j],":",max_member[i],"family members")
      }
      else{
        max_member_list[[i]] = c(max_member_list[[i]], paste(rownames(member)[j],":",max_member[i],"family members"))
      }
    }
  }
}
names(max_member_list) = 2004:2019
max_member_list
apply(member,2,function(x) sum(x == 10))

# Number of households present in 2010 and 2011.
dathh2010 = fread('./data/dathh2010.csv')
dathh2011 = fread('./data/dathh2011.csv')
length(intersect(na.omit(dathh2010$idmen), na.omit(dathh2011$idmen)))


###### Exercise3 Migration
# Find out the year each household enters and exit the panel. Report the length of years each household stay in the panel
dat_total$year = as.numeric(as.character(dat_total$year))
# the year each household enters the panel (report first 10 househlod)
by(dat_total$year,dat_total$idmen,min)[1:10] 
# the year each household exits the panel (report first 10 househlod)
by(dat_total$year,dat_total$idmen,max)[1:10] 
#length of years each household stay in the panel (report first 10 househlod)
stay_years = by(dat_total$year,dat_total$idmen,function(x) length(unique(x)))
data.frame(stay_years = stay_years[1:length(stay_years)]) %>%
  ggplot(aes(stay_years))+
  geom_histogram(fill="white",color="black",binwidth=0.5)+
  ggtitle(label='Distribution of years household staying in the panel')

# Base on datent, identify whether or not household moved into its current dwelling at the year of survey
dat_total$year = factor(dat_total$year)
dat_total$idind = as.integer64(as.character(dat_total$idind))
year_matrix = matrix(rep(2004:2019,length(unique(dat_total$idmen))),
                     length(unique(dat_total$idmen)),length(unique(dat_total$year)))
datent = by(dat_total$datent,dat_total[c('idmen','year')],unique) == year_matrix
# matrix shows that whether or not household moved into its current dwelling at the year of survey
datent 
datent[is.na(datent)] = 0

hh_surveyed_in_years =  by(dat_total$idmen,dat_total$year,
                            function(x) length(unique(na.omit(x))))[1:16]
hh_datent_na_in_years =  by(dat_total$datent,dat_total[c('idmen','year')],function(x) is.na(unique(x))) == TRUE
hh_datent_na_in_years[is.na(hh_datent_na_in_years)] = 0
hh_datent_na_in_years = apply(hh_datent_na_in_years, 2, sum)

hh_numbers_move_dw = data.frame(year = 2004:2019, number = apply(datent,2,sum), 
                                move_or_not = rep("Move to current dwelling",16))
hh_numbers_not_move_dw = data.frame(year = 2004:2019, 
                                    number = hh_surveyed_in_years - hh_datent_na_in_years - apply(datent,2,sum), 
                                    move_or_not = rep("Not move to current dwelling",16))
hh_numbers_na_dw = data.frame(year = 2004:2019, number = hh_datent_na_in_years, 
                              move_or_not = rep("NA(datent)",16))

datent_hist = rbind(hh_numbers_move_dw, hh_numbers_not_move_dw) %>% rbind(hh_numbers_na_dw)

ind_move_dw = c()
ind_unique = c()
for (y in 2004:2019){
  ind_move = length(na.omit(unique(dat_total[dat_total$datent==y & dat_total$year == y,'idind'])))
  ind_move_dw = c(ind_move_dw, ind_move)
}
for (y in 2004:2019){
  ind = length(na.omit(unique(dat_total[dat_total$year == y,'idind'])))
  ind_unique = c(ind_unique, ind)
}
ind_share_move_dw = data.frame(year = 2004:2019, 
                               share = round(ind_move_dw / ind_unique,4))

move_dw_plot = 
  ggplot(data=datent_hist,aes(x=year, y=number, fill=move_or_not))+
  geom_bar(stat = 'identity')+
  facet_wrap(.~move_or_not,1,3,scales="free_y")+
  ggtitle(label='Number of households whether or not move into its current dwelling')+
  theme(legend.position = "bottom")
move_dw_plot

ind_share_move_dw
move_dw_share_plot = 
  ggplot(data=ind_share_move_dw,aes(x=year, y=share))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=share, y=share+0.001), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='Share of individuals whether or not move into its current dwelling')
move_dw_share_plot

# Base on myear and move, identify whether or not household migrated at the year of survey. 
# Report the first 10 rows of your result and plot your identifier across years
migrate1 = by(dat_total$myear,dat_total[c('idmen','year')], 
             function(x) return(unique(x))) == year_matrix
migrate2 = by(dat_total$move,dat_total[c('idmen','year')], 
              function(x) return(unique(x))) == 2
migrate1[is.na(migrate1)] = 0
migrate2[is.na(migrate2)] = 0
migrate = migrate1 + migrate2 > 0
migrate[1:10,]

hh_myear_na_in_years = by(dat_total$myear,dat_total[c('idmen','year')],function(x) is.na(unique(x))) == TRUE
hh_myear_na_in_years[is.na(hh_myear_na_in_years)] = 0

hh_move_na_in_years = by(dat_total$move,dat_total[c('idmen','year')],function(x) is.na(unique(x))) == TRUE
hh_move_na_in_years[is.na(hh_move_na_in_years)] = 0

hh_migrate_na_in_years = apply(hh_myear_na_in_years + hh_move_na_in_years == 2,2,sum)


hh_numbers_migrate = data.frame(year = 2004:2019, number = apply(migrate, 2, sum),
                                migrate_or_not = rep("Migrate",16))
hh_numbers_not_migrate = data.frame(year = 2004:2019, 
                                    number = hh_surveyed_in_years - hh_migrate_na_in_years - apply(migrate, 2, sum),
                                    migrate_or_not = rep("Not migrate",16))
hh_numbers_na_migrate = data.frame(year = 2004:2019, 
                                   number = hh_migrate_na_in_years,
                                   migrate_or_not = rep("NA(myear&move)",16))

ind_migrate_dw = c()
for (y in 2004:2019){
  ind_migrate = length(na.omit(unique(dat_total[(dat_total$myear == y 
                                             | dat_total$move == 2) 
                                             & dat_total$year == y,'idind'])))
  ind_migrate_dw = c(ind_migrate_dw, ind_migrate)
}
ind_share_migrate_dw = data.frame(year = 2004:2019, 
                               share = round(ind_migrate_dw / ind_unique,4))

migrate_hist = rbind(hh_numbers_migrate, hh_numbers_not_migrate) %>% rbind(hh_numbers_na_migrate)
migrate_plot = 
  ggplot(data=migrate_hist,aes(x=year, y=number, fill=migrate_or_not))+
  geom_bar(stat = 'identity')+
  facet_wrap(.~migrate_or_not,1,3,scales="free_y")+
  ggtitle(label='Number of households migrated')+
  theme(legend.position = "bottom")
migrate_plot

ind_share_migrate_dw
migrate_share_plot = 
  ggplot(data=ind_share_migrate_dw,aes(x=year, y=share))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=share, y=share+0.001), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='Share of individuals migrated')
migrate_share_plot

# Mix the two plots you created above in one graph, clearly label the graph. 
# Do you like one identifier over the other?
move_dw_plot+migrate_plot+plot_layout(ncol=1,nrow=2)
move_dw_share_plot+migrate_share_plot+plot_layout(ncol=1,nrow=2)


# For households that migrate, find out how many households had at least one family 
# member changed his/her profession or employment status.
# find the change in each year.
migrate_hh = apply(migrate,1,sum)>0
migrate_hh = names(migrate_hh[migrate_hh==TRUE])
dat_total$year = as.numeric(as.character(dat_total$year))

years = 2004:2019
change_number = c(0)
for(y in 2:16){
  year_change = 0
  # check household migrated some year in each year from 2005
  for(hh in migrate_hh){
    hh_change = 0
    temp = dat_total[dat_total$idmen==hh&dat_total$year==years[y],]
    # check whether the household is surveyed in that year
    if (dim(temp)[1]>0){
      temp[is.na(temp)] = 0
      # check whether the household migrated that year
      if (unique(temp$myear)==unique(temp$year) | unique(temp$move)==2){
        temp = dat_total[dat_total$idmen==hh&dat_total$year%in%c(years[y-1],years[y]),]
        ids_name = unique(temp$idind)
        temp[is.na(temp)] = -1
        # check whether each member this year change prof or emp
        for (id in 1:length(ids_name)){
          id_temp = temp[which(temp$idind==ids_name[id]),]
          # if the member is not in the household last year, skip
          if (dim(id_temp)[1]==2){
            # if member's prof of epm changes from NA or changes to NA, he/she is also included
            if (length(unique(id_temp$profession))>1 | length(unique(id_temp$empstat))>1){
              hh_change = hh_change + 1
            }
          }
        }
      }
    }
    if (hh_change > 0){year_change = year_change + 1}
  }
  change_number = c(change_number, year_change)
}
change_number_dat = data.frame(year=years, number=change_number)
ggplot(change_number_dat,aes(year,number))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=number, y=number+0.5), position="dodge", vjust=0,size = 8/.pt) +
  ggtitle(label='For households that migrate, households had at least one family 
          member changedhis/her profession or employment status')

# Exercise 4 Attrition
dat_total$idind = as.integer64(as.character(dat_total$idind))
attrition = c(0)
attrition_rate = c(0)
for (y in 2005:2019){
  number_stay = 0
  id_first = unique(dat_total[dat_total$year==y-1,'idind'])
  id_second = unique(dat_total[dat_total$year==y,'idind'])
  for (i in 1:length(id_first)){
    if (id_first[i] %in% id_second) {number_stay = number_stay + 1}
  }
  attrition = c(attrition, length(id_first) - number_stay)
  attrition_rate = c(attrition_rate, (length(id_first) - number_stay)/length(id_first))
}
attrition
attrition_rate
data.frame(years = 2004:2019, attrition_rate = attrition_rate) %>%
  ggplot(aes(x = years, y = attrition_rate)) +
  geom_bar(stat = 'identity')+
  ggtitle(label='Attrition Rate')

