#Soal : (2 populasi)
#H0 : Pjkt <= Pjabar
#H1 : Pjkt > Pjabar
#njakarta = 10000
#njabar = 15000
#xjkt = 5000
#xjabar = 6000

mat <-matrix(c(5000,5000,6000,9000),2)
mat
prop.test(mat,alternative = "greater") #greater karena h1nya > kalo lebih kecil berarti less
#keputusan : karena pvalue <alfa(0.05) maka tolak H0
#artinya : pernyataan benar bahwa p jkt > p jabar 

#UAS
#H0 : tidak ada perbedaan proporsi kepuasan kerja antara laki-laki dengan perempuan
#H1 : ada perbedaan proporsi kepuasan kerja antara laki-laki dengan perempuan



#SOal : (k populasi)
#Dalam suatu penelitian, dikumpulkan data untuk menentukan apakah proporsi produk yang cacat oleh pekerja
#yang bertugas pagi,sore,dan malam hari sama atau tidak. Data diperoleh sebagai berikut.
#Banyak produk yang cacat (Pagi) = 45 dari 950 produk
#Banyak produk yang cacat (siang) = 55 dari 945 produk
#Banyak produk yang cacat (malam) = 70 dari 940 produk
#Guanakan taraf nyata 0.025, untuk menentukan apakah proporsi produk yang cacat sama untuk ketiga waktu kerja.

#Ho : Pcacat1 = Pcacat2 = Pcacat3
#H1 : Pcacat1 != Pcacat2 != Pcacat3

tabel <- matrix(c(45,905,55,890,70,870),2)
tabel
chisq.test(tabel)

#keputusan = karena p value > alfa maka trima H0
#Kesimpulan = produksi cacat pagi,siang,malam sama

#UAS
#H0 : tidak ada perbedaan proporsi kepuasan kerja antara laki-laki dengan perempuan
#H1 : ada perbedaan proporsi kepuasan kerja antara laki-laki dengan perempuan
#alpha : 1 %
tabel6<-matrix(c(4,9,40,15,3,7,39,16),4)
tabel6
prop.test(tabel6)

tabel8<-matrix(c(68,65),1)
tabel8
prop.test(tabel8)

#Latihan hal 323 no.4
#H0 : P>= 60%
#H1 : p<60%
#x=110
#n=200
#p=0.6
prop.test(110,200,0.6,alternative = "less")
#kesimpulan = karena p value > dari alfa maka terima H0
#Kesimpulan = benar bahwa sekurang - sekurangnya 60 % penduduk di suatu daerah mendukung perkara aneksasi oleh
#sebuah kota tetatngga yang berdekatan.

#Latihan hal 324 No 7
#X1 : 63
#n1 : 100
#x2 :59
#n2 : 125
#alpha : 0.04

#H0 : p1 = p2
#H1 : p1 != p2
tabel2 <- matrix(c(63,37,59,66), 2)
tabel2
prop.test(tabel2)
#keputusan : karena p value < alfa maka tolak h0
#Artinya : proporsi penduduk kota yang setuju pembangunan
#tidak sama dengan prnduduk sekitar kota

#Latihan hal 324 no 8
#xA : 56
#nA : 200
#xB : 29
#nB : 150
#alpha : 0.06

#H0 : pA<=pB
#H1 : Pa > pB

tabel3 <- matrix(c(56,144,29,121),2)
tabel3
prop.test(tabel3, alternative = "greater")
#kesimpulan : karena pvalue < alfa maka tolak h0
#artinya : merk a terjual lebih banyak dari merk b

#Latihan hal 324 no 9
#x1 : 31
#n1 : 100
#x2 : 24
#n2 : 100
#alfa : 0.01

#H0 : p1<=p2
#H1 : p1>p2

tabel4 <-matrix(c(31,69,24,76),2)
tabel4
prop.test(tabel4,alternative = "greater")
#kesimpulan : terima h0 (Karena pvalue > alfa)
#artinya : plaki<=perempuan

#Hal 336 n0.15
#H0 : P I = P II = P III = P IV
#H1 : minimal ada sepasang yang berbeda
#alfa : 0.05

tabel5 <- matrix(c(29,32,55,41,29,34,33,36,27,28,39,17), 3)
tabel5
chisq.test(tabel5)

#pvalue : 0.01
#kesimpulan : karena p value lebih kecil dari alfa maka tolak ho
#artinya : ada sepasang yang berbeda


tabel7<-matrix(c(1,2,1,0,1,1,1,0,3,3,1,2,1,3,2,1,11,17,8,4,5,13,12,9,2,3,5,5,1,1,3,11), 4)
tabel7
chisq.test(tabel7)

tabel8<-matrix(c(25,43,33,32),1)
tabel8
chisq.test(tabel8)
