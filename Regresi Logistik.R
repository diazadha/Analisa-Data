data<-read.csv("D:/Diaz/Kuliah/Semester 3/Analisis Data/dat.csv")
head(data)
#data di atas adalah status diterima atau tidaknya calon mahasiswa
#variabel respon admit (1 diterima, 0 tidak diterima)
#variabel prediktor gre, gpa, rank (1,2,3,4)

str(data)
data$admit<-as.factor(data$admit)
data$rank<-as.factor(data$rank)
fix(data)

model<-glm(data$admit~data$gre+data$gpa+data$rank, family = binomial)
summary(model)
prediksi <- (exp(-3.989979+0.002264*400+0.804038*4+0))/(1+exp(-3.989979+0.002264*400+0.804038*4+0))
prediksi
#kesimpulan = lulus karena peluang >= 0.5

#odds ratio
exp(coef(model))

prediksi2 <- (exp(1.0556-0.3135*40+1.4198*4.5+0+2.5355*1))/(1+exp(1.0556-0.3135*40+1.4198*4.5+2.355*1+0))
prediksi2
