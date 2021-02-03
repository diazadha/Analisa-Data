
table(iris$Species)
#ringkasan data
summary(iris)
#periksa frekuensi dari spesies
table(iris$Species)

#meng-rescale variabel sehingga punya range sama

seplength_n<-(iris$Sepal.Length-min(iris$Sepal.Length))