t<-ggplot(q,aes(x=quarter,y=inactivity_rate,col=as.factor(hhnbchld)))+geom_point()+geom_smooth()+
  geom_vline(xintercept = 2016.25)+
  ggtitle("Inactivity rates for Polish women",subtitle = "aged 20-59")+facet_wrap(~hat11lev)+
  scale_x_yearqtr(breaks = seq(from = min(q$quarter), to = max(q$quarter), by = 0.25),format = "%YQ%q")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g<-g%>%group_by(quarter,hatlev1d,hhnbchld)%>%mutate(hours=mean(hwusual))


beau<-ggplot(q,aes(x=quarter,y=inactivity_rate2,col=as.factor(hhnbchld)))+geom_point()+geom_smooth()+
  geom_vline(xintercept = 2016.25)+
  ggtitle("Inactivity rates for Polish women",subtitle = "aged 18-59, by level of education")+facet_wrap(~hatlev1d,scales = "free_y")+
  scale_x_yearqtr(breaks = seq(from = min(q$quarter), to = max(q$quarter), by = 0.25),format = "%YQ%q")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_colour_discrete(name = "Number of children")

plothr<-ggplot(filter(q,inactive==0&agecat=="30-39"|agecat=="40-49"),aes(x=quarter,y=hours,col=as.factor(hhnbchld)))+geom_point()+geom_smooth()+
  geom_vline(xintercept = 2016.25)+
  ggtitle("Hours worked for Polish women",subtitle = "aged 18-59, by level of education")+facet_wrap(~hatlev1d,scales = "free_y")+
  scale_x_yearqtr(breaks = seq(from = min(q$quarter), to = max(q$quarter), by = 0.25),format = "%YQ%q")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_colour_discrete(name = "Number of children")


tj<-ggplot(j,x=quarter,y=inactivity_rate,col=neartreshold)+geom_point()+geom_smooth()+
  geom_vline(xintercept = 2016.25)+
  ggtitle("Inactivity rates for Polish women near treshold for conditional 500+",subtitle = "aged 20-59, by level of education")+facet_wrap(~educat)+
  scale_x_yearqtr(breaks = seq(from = min(q$quarter), to = max(q$quarter), by = 0.25),format = "%YQ%q")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

q2012<-filter(q,year>2012)
t2<-ggplot(q2012,aes(x=quarter,y=unc))+geom_point()+geom_smooth(aes(col="Difference from the two"))+geom_smooth(aes(y=q2012$ina1.y,col="first child"))+geom_smooth(aes(y=q2012$ina2.y,col="second child")) + geom_vline(xintercept = 2016.25)+ggtitle("Marginal effect of the two first children on inactivity rates",subtitle = "And difference over time" )+ylab("Differences in activity rate")


ggplot(filter(femalesWA,hhnbchld<5),aes(x=quarter,y=inactivity_rate,col=as.factor(hhnbchld)))+geom_point()+geom_smooth()+
  geom_vline(xintercept = 2016.25)+
  ggtitle("Inactivity rates for Polish women",subtitle = "aged 20-59 by number of children")+
  scale_x_yearqtr(breaks = seq(from = min(q$quarter), to = max(q$quarter), by = 0.25),format = "%YQ%q")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(q,aes(x=quarter,y=hwusual,col=as.factor(hhnbchld)))+geom_smooth()+labs(color="Number of children")+facet_wrap(~agecat)+geom_vline(xintercept=2016.25)

ggplot(filter(g2),aes(x=quarter,y=partime,col=as.factor(hhnbchld)))+geom_smooth()+labs(color="Number of children")+facet_wrap(~hatlev1d+agecat,scales="free_y" )+geom_vline(xintercept=2016.25)+  theme(axis.text.x = element_text(angle = 90, hjust = 1))




