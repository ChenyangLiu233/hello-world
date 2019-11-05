EIA$density <- EIA$count / EIA$area
attach(EIA)
library(dplyr)
library(ggplot2)
newdata <- group_by(EIA, GridCode)%>%
  summarise(x.pos=first(x.pos), y.pos=first(y.pos), area=first(area), density=mean(density))
col<-colorRampPalette(rev(rgb(c(231,117,27),c(41,112,158),c(138,179,119),max=255)))(100)
p<-ggplot(newdata)
p<-p + geom_tile(aes(x=x.pos, y=y.pos, fill=density, height=1000, width=1000)) +
  scale_fill_gradientn(colours=col, space="Lab", na.value="grey50", guide="colourbar")
p + theme_bw() + coord_equal()

fit.full<- lm(density ~ tidestate + observationhour + DayOfMonth +
                MonthOfYear + impact + Year + x.pos + y.pos, data=EIA)
summary(fit.full)
fit.full.fac<- lm(density ~ tidestate + observationhour + DayOfMonth +
                    as.factor(MonthOfYear) + impact + Year +
                    x.pos + y.pos, data=EIA)
summary(fit.full.fac)
AIC(fit.full)
AIC(fit.full.fac)
library(car)
library(psych)
vif(fit.full.fac)
step(fit.full.fac)

install.packages("pedometrics")
library(pedometrics)
stepVIF(fit.full.fac)
