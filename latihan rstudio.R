#mengecek data set apa saja
data()
#pakai state x.77
head(state.x77) #untuk melihat data awal
help("state.x77")

#statistik deskriptif
state.x77<-data.frame(state.x77)
mean(state.x77$Population) #untuk rata-rata
median(state.x77$Population) #untuk nilai tengah
var(state.x77$Population) #untuk variasi
var(state.x77$Income) #Pendapatan
kv_populasi<-mean(state.x77$Population)/sd(state.x77$Population)
kv_income<-mean(state.x77$Income)/sd(state.x77$Income)
kv_populasi
kv_income

#Korelasi
cor(state.x77)
cor(state.x77[,1:3])
cor(state.x77[1:3,])

#analisis korelasi dengan plot korelasi
library(corrplot)
corrplot(cor(state.x77)) #jika biru maka hubungan kuat positif jika merah hubungan kuat negatif

#tabel frekuensi

#kategori populasi (low:0-3000, medium:3001-10000, high: 10001<)
state.x77$kategori_populasi<-cut(state.x77$Population,breaks = 
                                   c(0,3000,10000,Inf), labels = c("low","medium","high"))
head(state.x77)

#kategori income (poor: <=median, rich: median<)
median(state.x77$Income)
state.x77$kategori_income<-cut(state.x77$Income,breaks = c(0,median(state.x77$Income), Inf), labels = c("poor","rich"))
head(state.x77)

table(state.x77$kategori_income)

#kategori life.exp(short: <=q1, medium: q1-q3, long: q2-q3, very long: q3<)
state.x77$ktaegori_LE<-cut(state.x77$Life.Exp,breaks = c(-Inf,quantile(state.x77$Life.Exp,0.25),quantile(state.x77$Life.Exp,0.5), 
                                                         quantile(state.x77$Life.Exp,0.75), Inf), labels= c("short","medium","long","very long"))
head(state.x77) 

quantile(state.x77$Life.Exp,0.25)

#boxplot (menganalisis hubungan variabel kualitas)
plot(state.x77$kategori_income,state.x77$Murder,
     col=rainbow(2),
     xlab="kategori income", 
     ylab="murder",
     main="Boxplot murder Rate by Income Category")

#scatterplot
plot(state.x77$Illiteracy,state.x77$Murder, col = "red",)

#boxplot (untuk 1 variable)
boxplot(state.x77$Population,state.x77$Income)

#stem and leaf diagaram (untuk melihat distribusi data)
stem(state.x77$Income)

#diagaram batang
tab<-table(state.x77$kategori_income)
barplot(tab, col = (rainbow(2)))
legend("topleft",c("poor","rich"),fill=rainbow(2))
