dat<-data.frame(NULL)
fix(dat)
dat
#scatter plot
plot(dat)

#Korelasi
cor(dat$Tinggi,dat$Berat)

#ada atau tidak hubungan tinggi dan berat??
#uji sigifikansi korelasi
cor.test(dat$Tinggi,dat$Berat)

#anggap saja berhubungan tinggi dengan berat
#model regresi
rls<-lm(dat$Berat~dat$Tinggi)
summary(rls)

#tinggi = 170 berat ?
-43.4851 + 0.6199*170

#LATIHAN PAKAI DATASET state.x77
#1. Variabel apa saja yang signifikan berhubungan dengan illiterasi
data<-data.frame(state.x77)
cor.test(data$Population,data$Illiteracy) #tidak berhubungan
cor.test(data$Income,data$Illiteracy) #berhubungan
cor.test(data$Life.Exp,data$Illiteracy) #berhubungan
cor.test(data$Murder, data$Illiteracy) #berhubungan
cor.test(data$HS.Grad, data$Illiteracy) #berhbungan
cor.test(data$Frost, data$Illiteracy) #berhubungan
cor.test(data$Area, data$Illiteracy) #tidak berhbungan
#2. buatlah model regresi linear sederhana dengan variabel
#   dependen illiteracy dan variabel independen adalah 
#   yang paling signifikan berhubungan dengan illiteracy
model<-lm(data$Illiteracy~data$Murder)
summary(model)
#3. Interpretasikan model regresi yang terbentuk
#illiteracy = 0.3162+0.11607 * Murder
#angka pembunuhan (murder) berpengaruh terhadap illiteracy
#jika angka pembunuhan di suatu negara naik 1 persen maka angka buta huruf akan naik
#sebesar 0.11607 persen

#4.tampilkan scatter plot dan garis regresi
plot(data$Murder,data$Illiteracy)
abline(model)
