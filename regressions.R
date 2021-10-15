#¶all regressions !




#simplest
su
inba<-lm(inactive~mother*treated+hatlev1d+agecat+hhpartnr+degurba,filter(q,age<51))
inbaed<-lm(inactive~mother*treated+hatlev1d*dif0+agecat+hhpartnr+degurba,q)

#balances for did
Weight<- weightit(mother~hatlev1d+agecat+hhpartnr+degurba,filter(q,age<51),method = "ps",estimand = "ATT")
weight<- weightit(mother~agecat+hhpartnr,q,method = "ebal",estimand = "ATT")

d.w <- svydesign(ids = ~1, weights = get.w(Weight),
                 data = q)

d.W <- svydesign(ids = ~1, weights = get.w(Weight),
                 data = filter(q,age<51))

#bal global

fit <- svyglm(inactive ~ mother+treated+mother:treated+hatlev1d+hhpartnr+degurba+agecat, design = d.W)
fitfull <- svyglm(inactive ~ mother+treated+dif0+hatlev1d*dif0+hhpartnr*dif0+degurba*dif0+agecat*dif0, design = d.w)


#hr and ftpt bal
weighthr<- weightit(mother~agecat+hhpartnr,filter(q,inactive==0),method = "ebal",estimand = "ATT")

d.whr <- svydesign(ids = ~1, weights = get.w(weighthr),
                   data = filter(q,inactive==0))
#bal 3d

weightmar<- weightit(mother~agecat+hhpartnr,q,method = "ebal",estimand = "ATT")

d.wmar <- svydesign(ids = ~1, weights = get.w(weightmar),
                    data = q)
fitmar <- svyglm(inactive~treated+mother+mother1+treated:mother+treated:mother1+mother:mother1+difm+hatlev1d+agecat+hhpartnr+degurba, design = d.wmar)
# sep 1 et 2 to go with

weight1<- weightit(mother~agecat+hhpartnr,filter(q,hhnbchld!=2),method = "ebal",estimand = "ATT")

d.w1 <- svydesign(ids = ~1, weights = get.w(weight1),
                 data = filter(q,hhnbchld!=2))
fit1 <- svyglm(inactive ~ mother1+treated+mother1:treated+hatlev1d+hhpartnr+degurba, design = d.w1)



weight2<- weightit(mother~agecat+hhpartnr,filter(q,hhnbchld!=1),method = "ebal",estimand = "ATT")

d.w2 <- svydesign(ids = ~1, weights = get.w(weight2),
                 data = filter(q,hhnbchld!=1))
fit2 <- svyglm(inactive ~ mother2+treated+mother2:treated+hatlev1d+hhpartnr+degurba, design = d.w2)


#inba working hours !
a<-lm(hwusual~treated*mother+hatlev1d+agecat+degurba+hhpartnr,filter(q,inactive==0))
summary(a)
b<-lm(partime~treated*mother+hatlev1d+agecat+degurba+hhpartnr,data=filter(q,inactive==0))
summary(b)

#hr and ftpt bal
weighthr<- weightit(mother~agecat+hhpartnr,filter(q,inactive==0),method = "ebal",estimand = "ATT")

d.whr <- svydesign(ids = ~1, weights = get.w(weighthr),
                   data = filter(q,inactive==0))


fithr <- svyglm(hwusual ~ mother+treated+mother:treated+hatlev1d+hhpartnr+degurba+agecat, design = d.whr)
fitpart <- svyglm(partime ~ mother+treated+mother:treated+hatlev1d+hhpartnr+degurba+agecat, design = d.whr)

fithrfull <- svyglm(partime ~ mother+treated+dif0+hatlev1d*dif0+hhpartnr*dif0+degurba*dif0+agecat*dif0, design = d.whr)
fitpartfull <- svyglm(hwusual ~ mother+treated+dif0+hatlev1d*dif0+hhpartnr*dif0+degurba*dif0+agecat*dif0, design = d.whr)

fitpartmar <- svyglm(partime ~ mother+treated+mother1+mother:treated+mother:mother1+mother1:treated+difm+hatlev1d+hhpartnr+degurba+agecat, design = d.whr)



#bal education


weightlow<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==L),method = "ebal",estimand = "ATT")
weightmed<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==M),method = "ebal",estimand = "ATT")
weighthigh<- weightit(mother~agecat+hhpartnr,filter(q,hatlev1d==H),method = "ebal",estimand = "ATT")


d.wlow <- svydesign(ids = ~1, weights = get.w(weightlow),
                    data = filter(q,hatlev1d==L))
d.wmed <- svydesign(ids = ~1, weights = get.w(weightmed),
                    data = filter(q,hatlev1d==M))
d.whigh <- svydesign(ids = ~1, weights = get.w(weighthigh),
                     data = filter(q,hatlev1d==H))

fitlow <- svyglm(inactive ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wlow)
fitlowp <- svyglm(partime ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wlow)
fitlowh <- svyglm(hwusual ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wlow)


fitmed <- svyglm(inactive ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wmed)
fitmedp <- svyglm(partime ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wmed)
fitmedh <- svyglm(hwusual ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.wmed)



fithigh <- svyglm(inactive ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.whigh)
fithighp <- svyglm(partime ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.whigh)
fithighh <- svyglm(hwusual ~ mother+treated+mother:treated+hhpartnr+degurba+agecat, design = d.whigh)


qC<-filter(q,hhnbchld>0)
weightmarC<- weightit(mother2~agecat+hhpartnr,qC,method = "ebal",estimand = "ATT")

d.wmarC <- svydesign(ids = ~1, weights = get.w(weightmarC),
                    data = qC)
fitmarC <- svyglm(inactive~treated*mother2+hatlev1d+hhpartnr+degurba+agecat, design = d.wmarC)

stargazer(firmarC,no.space=T)

marC<-lm(inactive~treated*mother2+hatlev1d+hhpartnr+degurba+agecat,qC)


Qrev<-filter(q,incdecil>0&incdecil<11)
