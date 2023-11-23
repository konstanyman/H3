#setwd("D:/R/")    
library(foreign)
dat<-read.spss("toistetut mittaukset 2015r.sav", to.data.frame=TRUE)
attach(dat)

# Tunnusluvut vuosittain

summary (saasto2013)
summary (saasto2014)
summary (saasto2015)

# Laatikko-jana-kuvio

boxplot(saasto2013,saasto2014,saasto2015)

# Shapiro-Wilkin testit

shapiro.test(saasto2013)
shapiro.test(saasto2014)
shapiro.test(saasto2015)
detach(dat)

# Otetaan käyttöön data, jossa kunkin havainnon kohdalla säästöt ovat alekkain

dat2<-read.spss("toistetut mittaukset alekkain 2015r.sav", to.data.frame=TRUE)
attach(dat2)

# Toistettujen mittausten varianssianalyysi
fit1 = aov(euroa ~vuosi + Error(id/vuosi), data=dat2)
summary(fit1)
detach(dat2)
 
# vertailut parittaisin t-testein

attach (dat)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(dat)

#Friedmanin testi

attach(dat2)
friedman.test(euroa ~vuosi | id, data=dat2)
with(dat2 , boxplot( euroa  ~ vuosi ))  
detach(dat2)

# Wilcoxonin testit

attach (dat)

wilcox.test(saasto2013,saasto2015, paired=T)
wilcox.test(saasto2014,saasto2015, paired=T)

# Tehdään normaalisuustestit taloudellisen tyytyväisyyden tasoilla

with(dat,tapply(saasto2013,taltyyt,shapiro.test))  
with(dat,tapply(saasto2014,taltyyt,shapiro.test))  
with(dat,tapply(saasto2015,taltyyt,shapiro.test))  

# Toistettujen mittausten varianssianalyysi, toistotekijä ja luokitteleva tekijä

fit2<- aov(euroa ~taltyyt * vuosi + Error(id/vuosi), data=dat2)
summary(fit2)

# Yhdysvaikutuksen profiilikuvio

attach(dat2)
interaction.plot(vuosi,taltyyt,euroa, data=dat2)
detach (dat2)

# Vuosien vertailut taloudellisen tyytyväisyyden luokissa

attach(dat)
install.packages("dplyr")
library(dplyr)

data1 <- select(filter(dat, taltyyt=="erittain tyytyvainen"),c(taltyyt,saasto2013,saasto2014,saasto2015))    
attach(data1)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(data1)

data2 <- select(filter(dat, taltyyt=="melko tyytyvainen"),c(taltyyt,saasto2013,saasto2014,saasto2015))    
attach(data2)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(data2)

data3 <- select(filter(dat, taltyyt=="melko tyytymaton"),c(taltyyt,saasto2013,saasto2014,saasto2015))    
attach(data3)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(data3)

data4 <- select(filter(dat, taltyyt=="erittain tyytymaton"),c(taltyyt,saasto2013,saasto2014,saasto2015))    
attach(data4)
t.test(saasto2013,saasto2015, paired=T)
t.test(saasto2014,saasto2015, paired=T)
detach(data4)



