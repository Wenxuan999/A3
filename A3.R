setwd("/Users/wenxuan/Desktop/assignment3")
library("data.table")
library("dplyr")
#=============================================
# Exercise 1: 
#=============================================
datstu=fread("datstu_v2.csv")
datjss=fread("datjss.csv")
datsss=fread("datsss.csv")
#Take the longest name for the school name
datsss=datsss %>%
  group_by(schoolcode) %>%
  filter(nchar(schoolname) == max(nchar(schoolname))) %>%
  ungroup() 

#1.Number of students, schools, programs
#student
length(unique(datstu$V1))
#340823

#school
length(unique(datsss$schoolcode))
#898
length(unique(c(datstu$schoolcode1,datstu$schoolcode2,datstu$schoolcode3,datstu$schoolcode4,datstu$schoolcode5,datstu$schoolcode6)))
#The answer is 641, but when we see the result, we can find 1 NA, thus the number of schools is 640
#640

#program
length(unique(c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)))
#33

#2.Number of choices (school, program)
choice1=cbind(datstu$schoolcode1,datstu$choicepgm1)
choice2=cbind(datstu$schoolcode2,datstu$choicepgm2)
choice3=cbind(datstu$schoolcode3,datstu$choicepgm3)
choice4=cbind(datstu$schoolcode4,datstu$choicepgm4)
choice5=cbind(datstu$schoolcode5,datstu$choicepgm5)
choice6=cbind(datstu$schoolcode6,datstu$choicepgm6)
all_choice=unique(rbind(choice1,choice2,choice3,choice4,choice5,choice6))
colnames(all_choice)=c("schoolcode","program")
length(all_choice)/2
#3086



#3.Number of students applying to at least one senior high schools in the same district to home
#merge the schoolcode and the district
#remove the duplicate values of dataset datsss
datsss_dedup = unique(datsss)
#creat 6 tables with column schoolcode and district
data_district1=cbind(datsss_dedup[,3],datsss_dedup[,4])
data_district2=cbind(datsss_dedup[,3],datsss_dedup[,4])
data_district3=cbind(datsss_dedup[,3],datsss_dedup[,4])
data_district4=cbind(datsss_dedup[,3],datsss_dedup[,4])
data_district5=cbind(datsss_dedup[,3],datsss_dedup[,4])
data_district6=cbind(datsss_dedup[,3],datsss_dedup[,4])

data_district1=unique(data_district1)
data_district2=unique(data_district2)
data_district3=unique(data_district3)
data_district4=unique(data_district4)
data_district5=unique(data_district5)
data_district6=unique(data_district6)

#rename the column in the 6 tables
colnames(data_district1) = c("schoolcode1","sssdistrict1")
colnames(data_district2) = c("schoolcode2","sssdistrict2")
colnames(data_district3) = c("schoolcode3","sssdistrict3")
colnames(data_district4) = c("schoolcode4","sssdistrict4")
colnames(data_district5) = c("schoolcode5","sssdistrict5")
colnames(data_district6) = c("schoolcode6","sssdistrict6")
#merge the schoolcode to find the district of school that each student apply
datstu_district=datstu
datstu_district=merge(datstu_district,data_district1,by="schoolcode1",all.x=TRUE)
datstu_district=merge(datstu_district,data_district2,by="schoolcode2",all.x=TRUE)
datstu_district=merge(datstu_district,data_district3,by="schoolcode3",all.x=TRUE)
datstu_district=merge(datstu_district,data_district4,by="schoolcode4",all.x=TRUE)
datstu_district=merge(datstu_district,data_district5,by="schoolcode5",all.x=TRUE)
datstu_district=merge(datstu_district,data_district6,by="schoolcode6",all.x=TRUE)
#delete if the  junior high schools is empty
datstu_district=datstu_district%>%filter(datstu_district$jssdistrict!='')
b=datstu_district[,17]==datstu_district[,19]|datstu_district[,17]==datstu_district[,20]|datstu_district[,17]==datstu_district[,21]|datstu_district[,17]==datstu_district[,22]|datstu_district[,17]==datstu_district[,23]|datstu_district[,17]==datstu_district[,24]
b=b[complete.cases(b), ]

summary(b)

#4.Number of students each senior high school admitted
datstu[datstu$rankplace=="1", "school"] = datstu[datstu$rankplace=="1",schoolcode1]
datstu[datstu$rankplace=="2", "school"] = datstu[datstu$rankplace=="2",schoolcode2]
datstu[datstu$rankplace=="3", "school"] = datstu[datstu$rankplace=="3",schoolcode3]
datstu[datstu$rankplace=="4", "school"] = datstu[datstu$rankplace=="4",schoolcode4]
datstu[datstu$rankplace=="5", "school"] = datstu[datstu$rankplace=="5",schoolcode5]
datstu[datstu$rankplace=="6", "school"] = datstu[datstu$rankplace=="6",schoolcode6]
datstu=datstu[complete.cases(datstu$school), ]
num=datstu%>%group_by(school)%>% summarise(count = n())


#5.The cutoff of senior high schools (the lowest score to be admitted)
score1 = aggregate(datstu$score, by=list(datstu$school), FUN=min)
colnames(score1)[1] = 'schoolcode'
colnames(score1)[2] = 'cutoff'
#6.The quality of senior high schools (the average score of students admitted)
score2 = aggregate(datstu$score, by=list(datstu$school), FUN=mean)
colnames(score2)[1] = 'schoolcode'
colnames(score2)[2] = 'quality'


#=============================================
# Exercise 2: 
#=============================================
#1. the district where the school is located
#2.the latitude and longitude of the district
#3. cutoff (the lowest score to be admitted)
#4.quality (the average score of the students admitted)
#5. size (number of students admitted)

datsss=datsss[,-1]

datsss_dedup = unique(datsss)
data_school=merge(all_choice,datsss_dedup,all.x=TRUE)

colnames(score1)[1] = 'schoolcode'
colnames(score1)[2] = 'cutoff'
colnames(score2)[1] = 'schoolcode'
colnames(score2)[2] = 'quality'
colnames(num)[1] = 'schoolcode'

data_school=merge(data_school,score1,by="schoolcode",all.x=TRUE)
data_school=merge(data_school,score2,by="schoolcode",all.x=TRUE)
data_school=merge(data_school,num,by="schoolcode",all.x=TRUE)


#=============================================
# Exercise 3: 
#=============================================
#1.Using the formula where ssslong and ssslat are the coordinates of the district of the school (students apply to), while jsslong and jsslat are the coordinates of the junior high school, calculate the distance between junior high school, and senior high school. You should generate a value of distance for each of students’ choices.
#First, remove the duplicate values of dataset datsss
datsss_dedup = unique(datsss)
#Second, create 7 databases containing longitude, latitude, or district
merge1=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge2=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge3=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge4=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge5=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge6=cbind(datsss_dedup$schoolcode,datsss_dedup$ssslong,datsss_dedup$ssslat)
merge7=cbind(datsss_dedup$ssslong,datsss_dedup$ssslat,datsss_dedup$sssdistrict)
merge7=as.data.frame(merge7)
merge7=unique(merge7)
merge7=na.omit(merge7)
#rename the column
colnames(merge1) = c("schoolcode1","ssslong1","ssslat1")
colnames(merge2) = c("schoolcode2","ssslong2","ssslat2")
colnames(merge3) = c("schoolcode3","ssslong3","ssslat3")
colnames(merge4) = c("schoolcode4","ssslong4","ssslat4")
colnames(merge5) = c("schoolcode5","ssslong5","ssslat5")
colnames(merge6) = c("schoolcode6","ssslong6","ssslat6")
colnames(merge7) = c("jssslong","jssslat","jssdistrict")

#merge all the 7 dataset with datstu
merge=merge(datstu,merge1,by="schoolcode1",all.x=TRUE)
merge=merge(merge,merge2,by="schoolcode2",all.x=TRUE)
merge=merge(merge,merge3,by="schoolcode3",all.x=TRUE)
merge=merge(merge,merge4,by="schoolcode4",all.x=TRUE)
merge=merge(merge,merge5,by="schoolcode5",all.x=TRUE)
merge=merge(merge,merge6,by="schoolcode6",all.x=TRUE)
merge=merge(merge,merge7,by="jssdistrict",all.x=TRUE)
merge$jssslong = as.numeric(merge$jssslong)
merge$jssslat=as.numeric(merge$jssslat)
#we can calculate the distance with NA　a or without NA, now I get the result with NA
#merge=na.omit(merge)
merge$distance1= sqrt((69.172*(merge$ssslong1-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat1-merge$jssslat))^2))
merge$distance2= sqrt((69.172*(merge$ssslong2-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat2-merge$jssslat))^2))
merge$distance3= sqrt((69.172*(merge$ssslong3-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat3-merge$jssslat))^2))
merge$distance4= sqrt((69.172*(merge$ssslong4-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat4-merge$jssslat))^2))
merge$distance5= sqrt((69.172*(merge$ssslong5-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat5-merge$jssslat))^2))
merge$distance6= sqrt((69.172*(merge$ssslong6-merge$jssslong)*cos((merge$jssslat/57.3))^2+(69.172*(merge$ssslat6-merge$jssslat))^2))



#=============================================
# Exercise 4: 
#=============================================
#1.Recode the schoolcode into its first three digits (substr). Call this new variable scode rev.
all_choice_new=data.frame(all_choice)
all_choice_new$scode_rev=substring(all_choice_new$schoolcode,1,3)

#2.Recode the program variable into 4 categories: arts (general arts and visual arts), economics (business,and home economics), science (general science) and others. Call this new variable pgm rev.
all_choice_new$pgm_rev[all_choice_new$program == "General Arts"] = "arts"
all_choice_new$pgm_rev[all_choice_new$program == "Visual Arts"] = "arts"
all_choice_new$pgm_rev[all_choice_new$program == "Business"] = "economics"
all_choice_new$pgm_rev[all_choice_new$program == "Home Economics"] = "economics"
all_choice_new$pgm_rev[all_choice_new$program == "General Science"] = "science"
all_choice_new$pgm_rev[is.na(all_choice_new$pgm_rev) ==TRUE] = "others"
all_choice_new

#3.Create a new choice variable choice_rev.
all_choice_new$choice_rev=paste(all_choice_new$scode_rev,all_choice_new$pgm_rev)

#4.Recalculate the cutoff and the quality for each recoded choice.
#admitted school
datstu[datstu$rankplace=="1", "school"] = datstu[datstu$rankplace=="1",schoolcode1]
datstu[datstu$rankplace=="2", "school"] = datstu[datstu$rankplace=="2",schoolcode2]
datstu[datstu$rankplace=="3", "school"] = datstu[datstu$rankplace=="3",schoolcode3]
datstu[datstu$rankplace=="4", "school"] = datstu[datstu$rankplace=="4",schoolcode4]
datstu[datstu$rankplace=="5", "school"] = datstu[datstu$rankplace=="5",schoolcode5]
datstu[datstu$rankplace=="6", "school"] = datstu[datstu$rankplace=="6",schoolcode6]
datstu=datstu[complete.cases(datstu$school), ]
datstu$school_new=substring(datstu$school,1,3)
#admitted program
datstu[datstu$rankplace=="1", "program"] = datstu[datstu$rankplace=="1",choicepgm1]
datstu[datstu$rankplace=="2", "program"] = datstu[datstu$rankplace=="2",choicepgm2]
datstu[datstu$rankplace=="3", "program"] = datstu[datstu$rankplace=="3",choicepgm3]
datstu[datstu$rankplace=="4", "program"] = datstu[datstu$rankplace=="4",choicepgm4]
datstu[datstu$rankplace=="5", "program"] = datstu[datstu$rankplace=="5",choicepgm5]
datstu[datstu$rankplace=="6", "program"] = datstu[datstu$rankplace=="6",choicepgm6]
datstu=datstu[complete.cases(datstu$program), ]
#program categories
datstu$program_new[datstu$program == "General Arts"] = "arts"
datstu$program_new[datstu$program == "Visual Arts"] = "arts"
datstu$program_new[datstu$program == "Business"] = "economics"
datstu$program_new[datstu$program == "Home Economics"] = "economics"
datstu$program_new[datstu$program == "General Science"] = "science"
datstu$program_new[is.na(datstu$program_new) ==TRUE] = "others"
#Create a new choice variable choice_rev
datstu$choice_rev=paste(datstu$school_new,datstu$program_new)

#The cutoff of each recoded choice (the lowest score to be admitted)
score3 = aggregate(datstu$score, by=list(datstu$choice_rev), FUN=min)
colnames(score3)=c("choice_rev","cutoff")
#The quality of each recoded choice (the average score of students admitted)
score4 = aggregate(datstu$score, by=list(datstu$choice_rev), FUN=mean)
colnames(score4)=c("choice_rev","quality")
datstu1=cbind(datstu[,23],datstu[,2])


#5.Consider the 20,000 highest score students.
datstu=merge(datstu,score4,by="choice_rev",all.x=TRUE)
datstu_20000=head(datstu[order(datstu$score,decreasing = TRUE), ], 20000)
datstu_20000=cbind(datstu_20000[,1],datstu_20000[,3],datstu_20000[,24])
unique(datstu_20000$choice_rev)
#=============================================
# Exercise 5: 
#=============================================
#1.Propose a model specification. Write the Likelihood function.
#Consider the 20,000 highest score students.




datstu111=cbind(datstu_20000[,1],datstu_20000[,2])
datstu2 = as.data.frame(datstu111)
class(datstu2)
like_fun = function(param,dat_stu)
{
  score =  as.vector(dat_stu$score)
  cats = as.vector(sort(unique(dat_stu[,1])))
  ch = as.vector(match(dat_stu[,1], cats))
  ni = nrow(dat_stu)
  nj = length(cats)
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = score*pn1[j] + pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc)) / ni
  return(-like)
}
nj = length(as.vector(unique(datstu2[,1])))
print(nj)
res1 = optim(runif(2 * nj,min=-0.1,max=0),fn=like_fun,
             method="BFGS",control=list(trace=5,REPORT=1,maxit=10),
             dat_stu=datstu2,hessian=FALSE)





#2.Estimate parameters and compute the marginal effect of the proposed model.
parameters=res1$par
parameters

#marginal effect
#after we get the parameters, write it in the matrix way
beta = mat.or.vec(2,nj)
beta[1,] = res1$par[1:271]
beta[2,] = res1$par[272:542]
# build a matrix with columns "1" and scores
X=cbind(rep(1,20000),datstu2$score)
#creat a matrix with 20000 row and 271 columns
a=mat.or.vec(20000,271)
#the sum of the exponential function
exponential=exp(X%*%beta[,2:271])
rowSums(exponential)
sum=as.matrix(rowSums(exponential))
a=mat.or.vec(20000,271)
#give value for the matrix a
a[,1]=1/(1+sum)
a[,2]=exp(X %*% beta[,2])/(1+sum)

for (i in 1:270){
  value=exp(X %*% beta[,i+1])/(1+sum)
  a[,i+1]=value
}

#creat a 20000*271 matrix with the first column 0
b=mat.or.vec(20000,271)
bbeta=c(res1$par[272:542])
#marginal effect:{exp(X *beta[,i+1])/(1+sum)}*(par-sum beta)
for (i in 1:20000) {
  b[i,]=a[i,]*(bbeta-sum(a[i,]*bbeta))
}

marginal_effect1=apply(b, 2, mean)
marginal_effect1






#=============================================
# Exercise 6: 
#=============================================
#1. Propose a model specification. Write the Likelihood function.
#create the new dataset with the column  school quality on the first choice
datstu_quality22=cbind(datstu_20000[,1],datstu_20000[,3])
datstu_quality22 = as.data.frame(datstu_quality22)
class(datstu_quality22)
like_fun2 = function(param,dat_stu)
{
  quality =  as.vector(dat_stu$quality)
  cats = as.vector(sort(unique(dat_stu[,1])))
  ch = as.vector(match(dat_stu[,1], cats))
  ni = nrow(dat_stu)
  nj = length(cats)
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = quality*pn1[j] + pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc)) / ni
  return(-like)
}
nj = length(as.vector(unique(datstu_quality22[,1])))
print(nj)

res7 = optim(runif(2 * nj,min=-0.1,max=0),
             fn=like_fun2,method="BFGS",control=list(trace=5,REPORT=1,maxit=10),
             dat_stu=datstu_quality22,hessian=FALSE)





#2.Estimate parameters and compute marginal effect of the proposed model.
parameters2=res7$par

parameters2


#marginal effect
#after we get the parameters, write it in the matrix way
beta1 = mat.or.vec(2,nj)
beta1[1,] = res7$par[1:271]
beta1[2,] = res7$par[272:542]
# build a matrix with columns "1" and scores
X1=cbind(rep(1,20000),datstu_quality22$quality)
#creat a matrix with 20000 row and 271 columns
a=mat.or.vec(20000,271)
#the sum of the exponential function
exponential1=exp(X1%*%beta1[,2:271])
rowSums(exponential1)
sum1=as.matrix(rowSums(exponential1))
a1=mat.or.vec(20000,271)
#give value for the matrix a
a1[,1]=1/(1+sum1)
a1[,2]=exp(X1 %*% beta1[,2])/(1+sum1)

for (i in 1:270){
  value1=exp(X1 %*% beta1[,i+1])/(1+sum1)
  a1[,i+1]=value1
}

#creat a 2000*271 matrix with the first column 0
b1=mat.or.vec(20000,271)
bbeta1=c(res7$par[272:542])
for (i in 1:20000) {
  b1[i,]=a1[i,]*(bbeta1-sum(a1[i,]*bbeta1))
}

marginal_effect2=apply(b1, 2, mean)
marginal_effect2
#=============================================
# Exercise 7: 
#=============================================
datstu7 = datstu[-which(datstu$program_new == "others"),]
datstu7 = as.data.frame(datstu7)

#1.Explain and justify, which model (first or second model) you think is appropriate to conduct this exercise.
#I think the second model is appropriate, because in this question, it deleted the choices of others, but the choices of each individual have not changed, the type of choice has changed, so it is more suitable to estimate with the average score of each choice (quality).


#the first model
datstu7_1=cbind(datstu7[,1],datstu7[,3])
datstu7_1 = as.data.frame(datstu7_1)
class(datstu7_1)
colnames(datstu7_1)=c("choice_rev","score")

like_fun = function(param,dat_stu)
{
  score =  as.vector(dat_stu$score)
  cats = as.vector(sort(unique(dat_stu[,1])))
  ch = as.vector(match(dat_stu[,1], cats))
  ni = nrow(dat_stu)
  nj = length(cats)
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = score*pn1[j] + pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc)) / ni
  return(-like)
}

nj = length(as.vector(unique(datstu7_1[,1])))
print(nj)
res8 = optim(runif(2 * nj,min=-0.1,max=0),fn=like_fun,
             method="BFGS",control=list(trace=5,REPORT=1,maxit=10),
             dat_stu=datstu7_1,hessian=FALSE)




#the second model
datstu7_2=cbind(datstu7[,1],datstu7[,24])
datstu7_2 = as.data.frame(datstu7_2)
class(datstu7_2)
colnames(datstu7_2)=c("choice_rev","quality")



like_fun2 = function(param,dat_stu)
{
  quality =  as.vector(dat_stu$quality)
  cats = as.vector(sort(unique(dat_stu[,1])))
  ch = as.vector(match(dat_stu[,1], cats))
  ni = nrow(dat_stu)
  nj = length(cats)
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:nj]
  pn2    = param[(nj+1):(2*nj)]
  for (j in 1:nj)
  {
    ut[,j] = quality*pn1[j] + pn2[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) 
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,ch[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc)) / ni
  return(-like)
}
nj = length(as.vector(unique(datstu_quality22[,1])))
print(nj)

res9 = optim(runif(2 * nj,min=-0.1,max=0),
             fn=like_fun2,method="BFGS",control=list(trace=5,REPORT=1,maxit=10),
             dat_stu=datstu7_2,hessian=FALSE)

#2.Calculate choice probabilities under the appropriate model.

beta1



#3. Simulate how these choice probabilities change when these choices are excluded.
