#**********************************************Anova 1 arah**********************************************
#Pakai dataset chickwts
table(chickwts$feed)

#apakah rata-rata berat ayam berbeda menurut jenis makan?
#apakah pakan ayam yang berbeda memberi efek yang berbeda?
#Ho : rata-rata berat ayam yang diberi makan berbeda sama
      #(pemberian pakan yang berbeda tidak memberikan efek yang berbeda)
#H1 : rata-rata berat ayam yang diberi makan berbeda tidak sama
#   (pemberian pakan yang berbeda memberi efek berbeda)

anova1<-aov(weight~feed,data=chickwts)
summary(anova1)
#jika alfa 5 persen maka keputusan tolak H0 (alfa = 5.94e-10)
#artinya pemberian pakan berbeda memberikan efek berbeda
#perbedaannya ada dimana?
TukeyHSD(anova1) #perhatikan p adj
#H0 : mu i = mu j
#H1 ; mu i != mu j

#pakai dataset insect spray
head(InsectSprays)
help("InsectSprays")
table(InsectSprays$spray)
#Apakah keenam jenis semprotan yang ada sama efektifnya? gunakan alfa 10%

#Ho : keenam jenis semprotan sama efektinya
#(pemberian keenam semprotan yang berbeda tidak memberikan efek yang berbeda)
#H1 : keenam jenis semprotan tidak sama efeknya
#   (pemberian keenam semprotan yang berbeda memberi efek berbeda)

anova2<-aov(count~spray,data=InsectSprays)
summary(anova2)
#jika alfa 10% maka tolak H0
#artinya pemberian keenam semprotan yang berbeda memberi efek berbeda

#perbedaannya ada dimana?
TukeyHSD(anova2) #perhatikan p adj
#H0 : mu i = mu j (sama efektif)
#H1 ; mu i != mu j (tidak efektif)

#gunakan dataset Tooth
# apa maksud dari dataset tooth tersebut?
# ujilah rata-rata dari dataset tooth menurut variabel dose. kesimpulannya apa?
 

head(ToothGrowth)
help("ToothGrowth")
table(ToothGrowth$dose)
fix (ToothGrowth)
anova3 <- aov(len~dose, data =ToothGrowth)
summary (anova3)
#Ho : dosis yang berbeda memberi pengaruh yang sama terhadap panjang gigi
#H1 : dosis yang berbeda memberi pengaruh yang berbeda thd panjang gigi
#Keputusan : tolak H0 dengan alfa 10%
#kesimpulan : dosis yang berbeda memberi pengaruh yang berbeda

#pasangan dosis apa yang mempunyai pengaruh yang berbeda?
TukeyHSD(anova3)
#kesimpulan : semua dosis memberikan efek yang berbeda (tolak H0)

#latihan1 :
#Seorang ahli farmasi ingin mengtahui apakah 5 jenis parasetamol memberikan
#efek yang sama terhadap lamannya (dalam menit) menurunkan demam sampai 3 derajat.
#Dengan tingkat kesalahan 5 persen Berikut data yang dikumpulkan :

a<-c(30,25,15,20,20)
b<-c(25,25,20,15,10,10)
c<-c(25,23,24,30,30)
d<-c(10,10,15,30,25,30)
e<-c(20,30,25,26,27,28)

dat<-data.frame()
fix(dat)
dat

#H0 : Jenis Paracetamol memberikan efek yang sama thd lainnya
#H1 : Jenis paracetamol memberikan efek yang berbeda thd lainnya
anova4 <-aov(Waktu~Paracetamol,data=dat) #Numerik selalu sebelah kiri
summary(anova4)
#kesimpulan : terima H0, artinya parasetamol memberikan efek yang sama (pvalue >0.5)

mean(dat$Waktu)

#Latihan
#Nomor 1
dat1<-data.frame()
fix(dat1)
dat1

#H0 : Rata - rata penjualan ke lima rokok tersebut sama
#H1 : Rata-rata penjulan ke lima rokok terebut tidak sama
anova5<-aov(Terjual~Merk,data=dat1)
summary(anova5)

#alpha 5 %
# kesimpulan : Tolak H0, artinya Rata-rata penjualan ke lima rokok tersebut tidak sama
TukeyHSD(anova5)
#kesimpulan : merk yang gak sama c-b

#Nomor 2
dat2<-data.frame()
fix(dat2)
dat2

#H0 : Tidak ada selisih yang signifikan antara nilai rata-rata yang diberikan oleh ketiga dosen tersebut
#H1 : Ada selisih yang signifikan antara nilai rata-rata yang diberikan oleh ketiga dosen tersebut
anova6<-aov(Nilai~Dosen,data=dat2)
summary(anova6)
#alpha 5%
#kesimpulan : Terima H0, Tidak ada selisih yang signifikan antara nila rata-rata

#UAS
nomor1a<-data.frame()
fix(nomor1a)
nomor1a

#H0 : Tidak ada perbedaan rata-rata nilai dilihat dari kelas metode yang berbeda
#H1 : Ada perbedaan rata-rata nilai dilihat dari kelas metode yang berbeda
#alpha : 5 %
anova11<-aov(nilai~kelas,data=nomor1a)
summary(anova11)

#Contoh pakai dataset toothgrowth
#***************************************Anova 2 arah tanpa interaksi*************************
help("ToothGrowth")
#apakah rata-rata gigi babi dipengaruhi oleh dosis vitamin?
#Apkah rata-rata gigi babi dipengaruhi oleh jenis suplemen?
head(ToothGrowth)
fix(ToothGrowth)
anova7<-aov(len~supp+dose, data = ToothGrowth)
summary (anova7)
#Hipotesis 1 :
#H0 : Dosis yang berbeda memberi pengaruh yang sama terhadap gigi
#H1 : Dosis yang berbeda memberi pengaruh yang berbeda terhadap gigi
#Keputusan tolak H0

#Hipotesis 2 :
#H0 : Suplemen yang berbeda memberi pengaruh yang sama terhadap gigi
#H1 : Suplemen yang berbeda memberi pengaruh yang berbeda terhadap gigi
#Keputusan tolak H0


#***************************************Anova 2 arah dengan interaksi**************************************
#Apakah kombinasi antara suplemen dan dosis memberi pengaruh yang berbeda terhadap gigi?
anova8<-aov(len~dose*supp,data=ToothGrowth)
summary(anova8)
#keputusan tolak H0, artinya kombinasi antara dosis dan suplemen yang berbeda memberi pengaruh yang berbeda terhadap gigi

#Latihan
dat3<-data.frame()
fix(dat3)
dat3
#Hipotesis 1:
#H0 : Lingkungan dan galur tidak berinteraksi
#H1 : Lingkungan dan galur berinteraksi
#alpha 0.01
anova9<-aov(Data~Lingkungan*Galur,data=dat3)
summary(anova9)
#kesimpulan Lingkungan an galur tidak berinteraksi

#H0 : Tidak ada perbedaan skor kesalahan akibat perbedan lingkungan
#H1 : ada perbedaan skor kesalahan akibat perbedaan lingkungan
anova10<-aov(Data~Lingkungan+Galur,data=dat3)
summary (anova10)
#Kesimpulan : Tolak H0, artinya ada perbedaan skor kesalahan akibat perbedaan lingkungan

#H0 : Tidak ada perbeddaan skor kesalahan untuk ketiga galur
#H1 : Ada perbedaan skor kesalahan untuk ketiga galur
#Kesimpulan : Tolak H0, artinya ada perbedaan skor kesalahan untuk ketiga galur

#UAS 
#H0 : tidak ada perbedaan rata - rata nilai akibat dari kelas metode dan fasilitas ruang
#H1 : ada perbedaan rata-rata nilai akibat dari kelas metode dan fasilitas ruang

nomor1a<-data.frame()
fix(nomor1a)
nomor1a
anova12 <- aov(nilai~kelas*fasilitas,data=nomor1a)
summary(anova12)
#alpha : 0.05

anova13 <- aov(nilai~kelas+fasilitas,data=nomor1a)
summary(anova13)
