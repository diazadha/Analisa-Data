head(state.x77)
dat<-data.frame(state.x77[,1:8])
dat

#uji linearitas
plot(dat$Population,dat$Illiteracy)
plot(dat$Income,dat$Illiteracy)
plot(dat$Life.Exp,dat$Illiteracy)
plot(dat$Murder,dat$Illiteracy)
plot(dat$HS.Grad,dat$Illiteracy)
#kesimpulan x1 sampai x5 linear dengan y (illeteracy)

#model regresi linear berganda
rlb<-lm(dat$Illiteracy~dat$Population+dat$Income+dat$Life.Exp
        +dat$Murder+dat$HS.Grad)
summary(rlb)

#melihat hasil kebalikan model dengan visual
dim(dat)
obs<-1:50
plot(obs,dat$Illiteracy, type = "l",lwd=3)
lines(obs,fitted.values(rlb),col="red",lwd=3)
