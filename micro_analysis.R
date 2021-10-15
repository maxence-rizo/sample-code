library(MatchIt)
library(zoo)
library(stargazer)
library(cobalt)
library("WeightIt")
library("survey")

#The data are from the EU-LFS european Labor Force survey from 2010 to 2017 (I do not have access to more recent years)
#as these data are really long, I will not be sharing the databases ( 700 000 K observations for females with 2 kids or less) )


#The call to ungroup is needed to update dplyr Version as the database was constructed and cleaned back in 2019

q<-ungroup(femalesWA)%>%filter(as.numeric(hhnbchld)<=2)


# cleaning  the format and grouping of control variables

q$hat11lev<-as.factor(q$hat11lev)
q$agecat<-cut(q$age,c(19,30,40,50,60),labels=c("20-29","30-39","40-49","50-59"))
q$hatlev1d<-cut(as.numeric(q$hat11lev),c(0,4,5,10),labels= c("low","med","high"))
q$hhageyg<-as.numeric(q$hhageyg)

q$youngcat<-cut(q$hhageyg,c(0,3,6,12,18,25),labels = c("0 to 3","4 to 6","7 to 12","12 to 18","18 to 25"))

#dummy coding

q$inactivity_rate<-q$inactivity_rate*100
q$mother<-ifelse(q$hhnbchld>0,1,0)
q$dif0<-q$mother*q$treated
q$incdecil<-as.integer(q$incdecil)

# trying to  implement a  bunching regression at the neighborhood
#of the administrative threshold,very low quality of response concerning income,
#does not allow any analysis

#q<-left_join(q,ilc_di01,by="incdecil")
#q$hhnbpers<-as.numeric(q$hhnbpers)
#q$hhnbchld<-as.numeric(q$hhnbchld)
#q$hhadult<-q$hhnbpers-q$hhnbchld
#q$hhincome<-q$monthly*q$hhadult
#q$treshold<-q$hhnbpers*800
#q$diftres<-q$hhincome-q$treshold
#q$neartreshold<-ifelse(q$diftres<=1500 &q$hhincome-q$treshold>=0,1,0)#variables pour coder tout ceux ? moins de 1000 zlotys du seuil
#q$neartreshold<-as.factor(q$neartreshold)
#summary(q$diftres)


# Regressions on the inactivity rates and working hours. treatment groups are mother or mother of two,
#control groups are childless or mother of one.

q<-q%>% group_by(hhnbchld,quarter,hatlev1d)%>%mutate(inactivity_rate2= mean(inactive,na.rm=T))


didt<-lm(inactive~treated+mother+mother:treated+agecat*dif0+hatlev1d*dif0+degurba*dif0,q)


did1<-lm(inactivity_rate~treated+mother1+dif1+agecat+hatlev1d+degurba+hatlev1d:dif1+dif1:degurba+dif1:agecat,filter(q,hhnbchld<2))

did2<-lm(inactivity_rate~treated+mother2+dif2+agecat+hatlev1d+degurba+hatlev1d:dif2+dif2:degurba+dif2:agecat,filter(q,hhnbchld==2|hhnbchld==0))



q$degurba<-as.factor(q$degurba)

# calculate marginal effect of children using stepwise addition for controls, unbalanced.


q$difm<-q$dif0*q$mother1
a<-lm(inactive~treated+mother+mother1+treated:mother+treated:mother1+mother:mother1+difm+hatlev1d+agecat+hhpartnr+degurba,q)
b<-lm(inactive~treated*mother1+hatlev1d+agecat+hhpartnr+degurba,q)
c<-lm(inactive~treated*mother2+hatlev1d+agecat+hhpartnr+degurba,q)
d<-lm(inactive~treated*mother+hatlev1d+agecat+hhpartnr+degurba,q)
stargazer(a,b,c,no.space=T)

#balance table

bal.tab(mother~agecat+hhpartnr+degurba+hatlev1d,q,estimand="ATT")


#we need to reweight the control group for age and marital status

weightmar<- weightit(mother2~agecat+hhpartnr,q,method = "ebal",estimand = "ATT")

d.wmar <- svydesign(ids = ~1, weights = get.w(weightmar),
                 data = q)
fitmar <- svyglm(inactive~treated+mother+mother1+treated:mother+treated:mother1+mother:mother1+difm+hatlev1d+agecat+hhpartnr+degurba, design = d.wmar)
stargazer(fitmar,no.space=T)

#rates 1-0

g<-q%>%filter(hhnbchld!=0)
a<-lm(inactive~treated*mother2+hatlev1d+agecat+hhpartnr+degurba,g)
summary(a)

# it seems to show the result I guessed so far

b<-lm(unc~treated+mother1+dif1,filter(q,hhnbchld!=0))




#entropy balancing


#to assess everything treatment = dif0
Weight<- weightit(mother~hatlev1d+agecat+hhpartnr+degurba,q,method = "ps",estimand = "ATT")
weight<- weightit(mother~agecat+hhpartnr,q,method = "ebal",estimand = "ATT")

d.w <- svydesign(ids = ~1, weights = get.w(Weight),
                 data = q)
fit <- svyglm(inactive ~ mother+treated+mother:treated+hatlev1d+hhpartnr+degurba, design = d.w)
summary(fitul)
fitful<- svyglm(inactive ~ mother+treated+dif0+hatlev1d*dif0+hhpartnr*dif0+degurba*dif0+agecat*dif0, design = d.w)

#on hatlev1d educational groups : low = high school, med = some college, high = degree

weightlow<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==L),method = "ebal",estimand = "ATT")
weightmed<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==M),method = "ebal",estimand = "ATT")
weighthigh<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==H),method = "ebal",estimand = "ATT")

d.wlow <- svydesign(ids = ~1, weights = get.w(weightlow),
                 data = filter(q,hatlev1d==L))
d.wmed <- svydesign(ids = ~1, weights = get.w(weightmed),
                    data = filter(q,hatlev1d==M))
d.whigh <- svydesign(ids = ~1, weights = get.w(weighthigh),
                    data = filter(q,hatlev1d==H))

fitlow <- svyglm(inactive ~ mother1+treated+mother:treated+hhpartnr+degurba, design = d.wlow)
coef(fitlow)


fitmed <- svyglm(inactive ~ mother+treated+mother:treated+hhpartnr+degurba, design = d.wmed)
coef(fitmed)

fithigh <- svyglm(inactive ~ mother+treated+mother:treated+hhpartnr+degurba, design = d.whigh)
coef(fithigh)


dlow<-lm(inactivity_rate2 ~ mother+treated+mother:treated+hhpartnr+degurba,filter(q,hatlev1d=="low"))
dmed<-lm(inactivity_rate2 ~ mother+treated+mother:treated+hhpartnr+degurba,filter(q,hatlev1d=="med"))
dhigh<-lm(inactivity_rate2 ~ mother+treated+mother:treated+hhpartnr+degurba,filter(q,hatlev1d=="high"))
summary(dlow)
summary(dhigh)



#to assess unconditional vs conditional, treatment is dif2, compared with hhnbchld1
# the triple differences matches and compare mother of onlychildren and of 2 children
# the first ones only receive benefit if they have low income, the second children receive it unconditional


q2<-filter(q,hhnbchld>0)
Weight3<- weightit(mother2~hat11lev+agecat+hhpartnr+degurba,q2,method = "ps",estimand = "ATT")


# selecting and balancing with income does not work sadly

#j<-filter(q,is.na(monthly)==F)
#j<-filter(j,mother1==1)
#j<-filter(j,diftres<1000)

#nrow(filter(j,neartreshold==1))
#weightj<- weightit(neartreshold~hatlev1d+hhpartnr,data=j,method = "ps",estimand = "ATT")
#didj<-lm(inactivity_rate~youngcat+incdecil+hhpartnr+degurba+neartreshold*treated,j)
#d.wj <- svydesign(ids = ~1, weights = get.w(weightj), data = j)
#fitj<- svyglm(inactivity_rate ~ neartreshold+treated+neartreshold:treated+hatlev1d+hhpartnr+degurba+youngcat, design = d.wj)
#summary(fitj)
#j$incdecil<-as.factor(j$incdecil)
#bal.tab(didj)




#robustness checks, I construct a placebo pre reform and look at my DiD regression

placebo<-filter(q,date<="2016-04-01")
placebo$dummy<-ifelse(placebo$date>="2014-06-01",1,0)
commontrend<- lm( inactive~mother*dummy+hhpartnr+degurba,placebo)
summary(commontrend)

#pas de common trend pour les plus éduquées ... serait il possible qu'elles aient anticipé plus?


#repeat for different educational background
placebl<-filter(placebo,hatlev1d=="low")
 commontrendl<- lm( inactive~mother*dummy+hhpartnr+degurba,placebl)
 placebm<-filter(placebo,hatlev1d=="med")
 commontrendm<- lm( inactive~mother*dummy+hhpartnr+degurba,placebm)
placebh<-filter(placebo,hatlev1d=="high")
commontrendh<- lm( inactive~mother*dummy+hhpartnr+degurba,placebh)
bal.tab(mother~agecat+hhpartnr+degurba+hatlev1d,placebo)
summary(commontrendh)

 placebl1<-filter(placebo,hatlev1d=="low")
 placebm1<-filter(placebo,hatlev1d=="med")
placebh1<-filter(placebo,hatlev1d=="high")
commontrendh<- lm( inactive~mother*dummy+hhpartnr+degurba,placebh)
 commontrendh1<- lm( inactive~mother1*dummy+hhpartnr+degurba,filter(placebh,hhnbchld!=2))
 commontrendh2<- lm( inactive~mother2*dummy+hhpartnr+degurba,filter(placebh,hhnbchld!=1))
 commontrendh<- lm( inactive~mother*dummy+hhpartnr+degurba,placebh)

commontrendl<- lm( inactive~mother*dummy+hhpartnr+degurba,placebl)
commontrendl1<- lm( inactive~mother1*dummy+hhpartnr+degurba,filter(placebl,hhnbchld!=2))
commontrendl2<- lm( inactive~mother2*dummy+hhpartnr+degurba,filter(placebl,hhnbchld!=1))

 summary(commontrendl1,commontrendl2)
 
 
# do the same with matching design, I comment out this part beacause it takes some times to compute

# weightpl<- weightit(mother~agecat+hhpartnr,placebo,method = "ebal",estimand = "ATT")
# 
# d.wpl <- svydesign(ids = ~1, weights = get.w(weightpl),
#                  data = placebo)
# fitpl <- svyglm( inactive~mother*dummy+hhpartnr+degurba+hatlev1d, design = d.wpl)
# summary(fitpl)


 
 # Second part : assessing the effect on working hours and part time employment : do mother reduce more their hours since 500+?
 
#on worked hours, penser à garder hwusual ftpt et ftptreas

q$hwusual<-ifelse(q$hwusual==00,NA,q$hwusual)
q$hwusual<-ifelse(q$hwusual==99,0,q$hwusual)

#subsetting 

 g<-filter(q,hwusual!=0 &age<51&age>29) #active women 29-51 for a restricted active class

# summary(g$hwusual)
#regressions


q$partime<-ifelse(q$ftpt==2,1,0)
a<-lm(hwusual~treated*mother+agecat+degurba+hhpartnr,filter(q,inactive==0 & hatlev1d=="H"))
summary(a)
bal.tab(mother2~agecat+hhpartnr+degurba+hatlev1d,g,estimand="ATT")
b<-lm(partime~treated*mother+agecat+degurba+hhpartnr,data=filter(q,inactive==0& hatlev1d=="M"))
summary(b)


bal.tab(mother~agecat+hatlev1d+degurba+hhpartnr,data=g,estimand="ATT")


weighthr<- weightit(mother2~agecat+hhpartnr+degurba,g,method = "ebal",estimand = "ATT")

d.whr <- svydesign(ids = ~1, weights = get.w(weighthr),
                   data = g)
fithr <- svyglm( inactive~mother2*treated+hhpartnr+degurba+hatlev1d+agecat+youngcat, design = d.whr)
summary(fithr)
stargazer(fithr,no.space=T)


