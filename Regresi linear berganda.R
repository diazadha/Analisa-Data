dat<-data.frame(state.x77[1:8])
head(dat)

#uji asusmsi linearitas
plot(state.x77$Population,state.x77$Illiteracy)
library(lmtest)
resettest(state.x77$Illiteracy~state.x77$Population)
plot(state.x77$Income,state.x77$Illiteracy)
plot(state.x77$Life.Exp,state.x77$Income)
plot(state.x77$Murder,state.x77$Illiteracy)
plot (state.x77$HS.Grad,state.x77$Illiteracy)
#kesimpulan x1 sampai x5 bisa digunakan

#pemodelan regresi linear berganda
rlb<-lm(state.x77$Illiteracy~state.x77$Population
        +state.x77$Income+state.x77$Life.Exp+state.x77$Murder
        +state.x77$HS.Grad)
summary(rlb)
