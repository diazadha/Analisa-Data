#manajer pelayanan mengevaluasi kecepatan seorang teller
#manajer mengambil sampel 15 nasabah lalu mengukur lamanya waktu dilayani
#standar pelayann di bank itu adalah per nasabah dilayanin tidak lebih dari 5 meniy 
#apakah teller tersebut memenuhi kriteria bank?
#Ho: mu <= 5 menit (memenuhi kriteria)
#H1: mu > 5 menit (tidak memenuhi kriteria)

dt<-data.frame(NULL)
fix(dt)
dt


t.test(dt$lama_waktu,mu=5,alternative= "greater")
#t statistik = 0,80695 p-value =5% Ho <= 5 menit
#dengan tingkat kesalahan 5% di disimpulkan bahwa teller tersebut memenuhi kriteria 
#alternative satu arah pakai greater 
#alternative dua arah tidak usah ditulis 
#alternative kurang dari pakai less

#sekotak susu kemasan ditulis berisi 150 ml di kemasannya
#seorang konsum tidak percaya lalu membeli 10 buah susu tersebut 
#apakah isi usu tersebut memang bukan 150ml? gunakan tkt.kesalahan 10%
#datanya : 145,155,150,161,148,149,150,147,151
#Ho : mu 150 H1 != 150
susu<-c(145,155,150,161,148,149,150,147,151)
t.test(susu,mu=150)
#p-value = 0,6833 lebih besar dari 10% maka terima Ho
#maka susu tersebut 150 ml

#pertanyaan uji kedua (2 populasi indenpenden)

#seorang konsultan ingin meneliti apakah benar dugaan mahasiswa UAI
#bahwa pengunjung indomaret lebih banyak dari alfamart
#diambil sample 10 toko indomart dan 12 toko alfamart
#Ho : muind-mualfa<=0 H1:muind-mualfa > 0
ind<-c(10,20,15,16,17,18,14,15,17,16)
alf<-c(8,7,10,15,18,20,6,8,9,10,11,12)
#uji kesamaan varians 
var.test(ind,alf)
t.test(ind,alf,alternative = "greater", var.equal = T)

#