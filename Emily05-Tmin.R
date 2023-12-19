getwd()
setwd("/Users/egiles/Dropbox/School/Statistics/Stats_Project/Data_3/")

install.packages("lmerTest")

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

#-------------------------------
# AUTUMN
#-------------------------------
datos_a <-read.table("datos_2_autumn.txt",h=T)
str(datos_a)

# This is the site as a factor
site<-factor(paste(datos_a$Location,datos_a$Sitio))
levels(site)

#Test if factor (site) is significant
m1<-lmer(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_a)
m2<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_a)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.203

# Fixed effects
m3<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_a)
step(m3,directions = "backwards", test="F") ##kept amb.Tmin, wet, abs(lat)

# Final model
m3<-lm(Tmin~amb.Tmin + Wet + abs(Latitud), datos_a)
autoplot(m3) #assumptions ok

summary(m3) #all coefficients are significant.
confint(m3) #CI of intercept, amb.Tmin include 1. Decoupling due to wet and abs(lat)

# Graphs
plot_1<-ggplot(datos_a, 
		aes(x=amb.Tmin, y=Tmin, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("F3,44=186.2, p<2.2e-16") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_1
	ggsave("Autumn-Radiation-Tmin.png")

plot_2<-ggplot(datos_a, 
		aes(x=amb.Tmin, y=Tmin, colour=Wet)) + 
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="S", size=10)
plot_2
	ggsave("Autumn-Conductivity-Tmin.png")
plot_3<-grid.arrange(plot_1,plot_2, nrow=1)
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
m1<-lmer(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_b)
m2<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_b)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.77

# Fixed effects
m3<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_b)
step(m3,directions = "backwards", test="F") ##kept amb.Tmin

# Final model
m3<-lm(Tmin~amb.Tmin, datos_b)
autoplot(m3) #assumptions ok

summary(m3) #all coefficients are significant.
confint(m3) #CI of amb.Tmin includes 1. No decoupling.
AIC(m3f,m3)

ggplot(datos_b, 
	aes(x=amb.Tmin, y=Tmin),) + 
	geom_point()+
	xlab("Tmin")+
	ylab("Tbmin") +
	geom_smooth(method="lm", se=FALSE) 

# Graphs
plot_1<-ggplot(datos_b, 
		aes(x=amb.Tmin, y=Tmin, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("F3,44=145.6, p<2.2e-16") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_1
	ggsave("Spring-Radiation-Tmin.png")

plot_2<-ggplot(datos_b, 
		aes(x=amb.Tmin, y=Tmin, colour=Wet)) + 
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_2
	ggsave("Spring-Conductivity-Tmin.png")
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
m1<-lmer(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude + (1|site), datos_c)
m2<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_c)

x2<- -2*logLik(m2,REML=T)+2*logLik(m1,REML=T)#Watch out with the order
pchisq(x2, df=1, lower.tail=FALSE)# Site is NS. 

rand(m1)#lmerTest random factor not significant p=0.36

# Fixed effects
m3<-lm(Tmin~amb.Tmin + Shade + Wet + abs(Latitud) + Altitude, datos_c)
step(m3,directions = "backwards", test="F") ##kept altitude, wet, abs(lat), amb.min

# Final model
m3<-lm(Tmin~amb.Tmin + Wet + abs(Latitud) + Altitude, datos_c)
autoplot(m3) #assumptions ok

summary(m3) #all coefficients are significant.
confint(m3) #CI of wet includes 1. Decoupling due to amb.tmin, abs(lat), alt.

ggplot(datos_c, 
	aes(x=amb.Tmin, y=Tmin,colour = abs(Latitud)),) + 
	geom_point()+ 
	geom_smooth(method="lm", se=FALSE) 

plot_1<-ggplot(datos_c, 
		aes(x=amb.Tmin, y=Tmin, colour=Shade)) + 
		geom_point(aes(),size=5, alpha=0.4)+ 
		#geom_smooth(method="lm",fill=NA)+
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("F4,55=95.23, p<2.2e-16") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_1
	ggsave("Summer-Radiation-Tmin.png")

plot_2<-ggplot(datos_c, 
		aes(x=amb.Tmin, y=Tmin, colour=Wet)) + 
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
		xlab("Tmin") +
		ylab("Tmin") +
		ggtitle("") +
		annotate("text",x=29,y=30,label="NS", size=10)
plot_2
	ggsave("Spring-Conductivity-Tmin.png")
plot_3<-grid.arrange(plot_1,plot_2, nrow=1)
ggsave("Autumn-Ta.png")
