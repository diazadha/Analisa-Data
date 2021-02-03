#pakai dataset chickwts

head(chickwts)
table(chickwts$feed)
dim(chickwts)
#gunakan tingkat kesalahan 5 persen
#ujilah apakah rata-rata berat ayam lebih dari 150 gr (1 Populasi)
#Ho = mu <= 150
#H1 = mu > 150 (greater)
t.test(chickwts$weight,mu=150,alternative = "greater")
#keputusan tolak H0
#artinya : benar bahwa rata-rata berat ayam lebih dari 150 gr

#ujilah apakah berat ayam berdistribusi normal untuk horsebean dan linseed
#H0 : data berdistribusi normal
#H1 : data tidak berdistribusi normal
# visual qqplot
qqnorm(chickwts$weight[chickwts$feed=="horsebean"])
shapiro.test(chickwts$weight[chickwts$feed=="horsebean"])
#keputusan : terima H0
#artinya : benar berat ayam yang makan horsebean berdistribusi normal

qqnorm(chickwts$weight[chickwts$feed=="linseed"])
shapiro.test(chickwts$weight[chickwts$feed=="linseed"])
#keputusan : terima H0
#artinya : benar berat ayam yang makan linseed berdistribusi normal

#uji apakah rata-rata berat ayam yang makan horsebean sama dengan yang makan linseed
#uji kesamaan varians 2 populasi
#H0 : varians 2 populasi sama
#H1 : varians 2 populasi tidak sama
var.test(chickwts$weight[chickwts$feed=="horsebean"],
         chickwts$weight[chickwts$feed=="linseed"])
#Ho : mu horsebean-mu linseed = 0
#H1 : mu horsebean-mu linseed != 0
t.test(chickwts$weight[chickwts$feed=="horsebean"],
       chickwts$weight[chickwts$feed=="linseed"], var.equal=T)
#keputusan : tolak H0
#artinya : rata-rata ayam yang makan horsebean tidak sama dengan yang makan linseed

#kalau tidak sama mana yang lebih bagus makananya
#jadi karena mean linseed>horsebean maka linseed lebih bagus

#Uji hipotesis rata-rata 2 populasi berpasangan
#berikut adalah hasil tes TOEFL sebelum dan sesudah mengikuti les
sebelum<-c(450,470,500,512,460,485,600,500)
sesudah<-c(460,465,500,512,465,476,598,500)
#apakah les efektif meningkatkan skor toefl?
#H0 ; toefl sesudah <= toefl sebelum (tidak efektif)
#H1 ; toefl sesudah > toefl sebelum (efektif)
t.test(sesudah,sebelum,paired = T,alternative = "greater")
#keputusan : terima H0
#artinya : les tidak meningkatkan skor test toefl

#soal 1 :
#seorang rektor berpendapat bahwa rata-rata mahasiswannya yang bekerja di perusahaan asing memiliki gaji minimal 8 juta.
#Hal tersebut diragukan oleh seorang peneliti.Peneliti tersebut mengambil sample lulusan mahasiswa yang bekerja di perusahaan asing
data<-data.frame()
fix(data)
dat
#Dengan tingkat kesalahan 5 persen, kesimpulan apa yang bisa diambil
#H0 :mu<=8
#H1 :mu>8
t.test(dat$Gaji,mu=8,alternative = "greater")
#kesimpulan : terima H0
#artinya :  Tidak benar bahwa rata-rata mahasiswa yanng bekerja di perusahaan asing memiliki gaji minimal 8 juta

#Soal 2:
#Banyak orang yang berpendapat bahwa pelanggan gojek lebih banyak dari grab. Dari 7 hari diambil rata-rata banyaknya penumpang selama tahun 2018. Berikut datanya
#Apakah anggapan banyak orang benar?Gunakan tingkat kesalahan 1 persen

#H0 : Gojek-Grab <=0
#H1 : Gojek-Grab>0
Gojek<-c(150,200,140,211,200,200,201)
Grab<-c(200,150,150,200,180,180,202)
var.test(Gojek,Grab)
t.test(Gojek,Grab,var.equal = T,alternative = "greater")
#kesimpulan : pvalue > 0.01 ---> terima H0
#Arti : Anggapan orang tidak benar bahwa rata-rata pelanggan gojek lebih banyak dari grab

#Soal 3:
#Seseorang sales obat mengatakan bahwa obat yang dijualnya dapat efektif menurunkan berat badan lebih dari 5kg selama 3 bulan
#tanpa harus mengurangi porsi makan. Untuk membuktkannya seorang peneliti mengambil sample orang yang mengkonsumsi obat tersebut.
#berikut data berat badan sample bulan 1 dan bulan 3

#H0 : bulan1-bulan3<=5
#H1 : bulan1 -bulan3>5

bulan1<-c(70,80,67,90,85,70,95,94,97)
bulan3<-c(65,80,62,91,84,65,90,94,96)
t.test(bulan1,bulan3,alternative = "greater",paired = T)
#kesimpulan : obat efektif

#H0 : laki-perempuan<=0
#H1 : Laki-perempuan>0

beratlaki<-c(2.5,3,3.7,3.4,3.5,4,2.4,3.5,3.6)
beratperempuan<-c(3.2,3.3,3,3.3,3,2.5)
var.test(beratlaki,beratperempuan)
t.test(beratlaki,beratperempuan,var.equal = T,alternative = "greater")
#terima h0