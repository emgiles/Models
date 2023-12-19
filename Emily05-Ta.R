getwd()
setwd("/Users/egiles/Dropbox/School/Statistics/Stats_Project/Data_3/")

install.packages("lmerTest")
install.packages("ggthemes")
#-----------
# LIBRERIAS
#-----------
library(dplyr)
library(ggplot2) 
library(gridExtra)
library(ggfortify)
library(corrplot)
library(psych)
library(lme4)
library(lmerTest)
library(ggthemes)

#-------------------------------
# AUTUMN
#-------------------------------
datos_a <-read.table("datos_2_autumn.txt",h=T)
str(datos_a)
summary(datos_a)
check<-lm(Ta~amb.Ta, datos_a)
autoplot(check)

# This is the site as a factor
site<-factor(paste(datos_a$Location,datos_a$Sitio))
levels(site)

#Test if factor (site) is significant
m1<-lmer(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_a)
m2<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_a)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.31

# Fixed effects
m3<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_a)
step(m3,directions = "backwards", test="F") ##kept alt, shade, amb.ta

# Final model
m3<-lm(Ta~amb.Ta + Shade + Altitude, datos_a)
autoplot(m3) #assumptions ok

summary(m3) #all coefficients significant
confint(m3) #no CI of coefficients includes 1. Decoupling due to all coefficients?



# Graphs
plot_1<-ggplot(datos_a, 
		aes(x=amb.Ta, y=Ta, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		geom_smooth()+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20),
			legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Sun","Shade"),
			values=c("deeppink3","darkcyan")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("F3,44=145.6, P<2.2e-16") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_1
	ggsave("Autumn-Radiation-Ta.png")

plot_2<-ggplot(datos_a, 
		aes(x=amb.Ta, y=Ta, colour=Wet)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20), 																		legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Dry","Wet"),
			values=c("peru","royalblue")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_2
	ggsave("Autumn-Conductivity-Ta.png")
	
plot_3<-ggplot(datos_a, 
		aes(x=amb.Ta, y=Ta, colour=abs(Latitud)))+
		geom_point()+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20),
			legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.3)) +
			geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_3
plot_4<-grid.arrange(plot_1,plot_2, nrow=1)
ggsave("Autumn-Ta.png")

#-------------------------------
# SPRING
#-------------------------------
datos_b <-read.table("datos_2_spring.txt",h=T)
str(datos_b)

# This is the site as a factor
site<-factor(paste(datos_b$Location,datos_b$Sitio))
levels(site)

#Test if factor (site) is significant
m1<-lmer(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_b)
m2<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_b)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.061

# Fixed effects
m3<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_b)
step(m3,directions = "backwards", test="F") ##kept shade, amb.ta

# Final model
m3<-lm(Ta~amb.Ta + Shade, datos_b)
autoplot(m3) #assumptions ok ish... residuals vs fitted doesn't show complete scatter...

summary(m3) #amb.Ta, ShadeYes significant. Intercept moderately significant.
confint(m3) #CI of intercept and amb.Ta include 1. shade causing decoupling? or no decoupling since intercept CI includes 1... how do you interpret CIs of intercept in the coupling-decoupling context?

# Graphs
plot_1<-ggplot(datos_b, 
		aes(x=amb.Ta, y=Ta, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth()+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20),
			legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Sun","Shade"),
			values=c("deeppink3","darkcyan")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("F2,44=126.8, P<2.2e-16") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_1
	ggsave("Spring-Radiation-Ta.png")

plot_2<-ggplot(datos_b, 
		aes(x=amb.Ta, y=Ta, colour=Wet)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20), 																		legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Dry","Wet"),
			values=c("peru","royalblue")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_2
	ggsave("Spring-Conductivity-Ta.png")
plot_3<-grid.arrange(plot_1,plot_2, nrow=1)
ggsave("Autumn-Ta.png")



#-------------------------------
# SUMMER
#-------------------------------
datos_c <-read.table("datos_2_summer.txt",h=T)
str(datos_c)

# This is the site as a factor
site<-factor(paste(datos_c$Location,datos_c$Sitio))
levels(site)

#Test if factor (site) is significant
m1<-lmer(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_c)
m2<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_c)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.61

# Fixed effects
m3<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_c)
step(m3,directions = "backwards", test="F") ##kept wet, abs(lat), amb.Ta, Alt, Shade

# Final model
m3<-lm(Ta~amb.Ta + Shade + Wet + abs(Latitud) + Altitude, datos_c)
autoplot(m3) #assumptions ok 

summary(m3) #All coeffs significant.
confint(m3) #no CIs include 1. Decoupling due to all coefficients??

# Graphs
plot_1<-ggplot(datos_c, 
		aes(x=amb.Ta, y=Ta, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth()+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20),
			legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Sun","Shade"),
			values=c("deeppink3","darkcyan")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("F5,54=44.86, P<2.2e-16") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_1
	ggsave("Spring-Radiation-Ta.png")

plot_2<-ggplot(datos_c, 
		aes(x=amb.Ta, y=Ta, colour=Wet)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
		theme(legend.title=element_blank(), 
			panel.background=element_blank(), 
			axis.line=element_line(colour="black"), 
			legend.text=element_text(size=20), 																		legend.background=element_rect(colour=NA),
			legend.key=element_rect(colour="white",fill=NA),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			legend.position=c(0.9,0.1)) +
		scale_colour_manual(breaks=c("No","Yes"), 
			labels=c("Dry","Wet"),
			values=c("peru","royalblue")) +
		geom_abline(slope=1,intercept=0,linetype=2) +
		coord_cartesian(xlim=c(0,30),ylim=c(0,30)) +
		xlab("Ta") +
		ylab("Ta") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_2
	ggsave("Spring-Conductivity-Ta.png")
plot_3<-grid.arrange(plot_1,plot_2, nrow=1)
ggsave("Autumn-Ta.png")