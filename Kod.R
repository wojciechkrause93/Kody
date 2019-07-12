############bibliteki
#install.packages(c("readxl","dplyr","magrittr","tidyverse","moments","clusterSim))
library(readxl);library(dplyr);library(magrittr);library(tidyverse);library(moments);library(clusterSim)
############ Wczytanie bazy
baza <- read_xlsx("RstandaryzacjaDane.xlsx",sheet = "Arkusz1")
############ Przerobienie Bazy
wojewodztwa <- baza[1]
baza <- baza[-1]
BazaSprawdzajaca <- baza
############ Standaryzacja
{baza.normalizacja <- data.frame(baza)
n1.Baza <- data.Normalization(baza.normalizacja, type="n1")
n2.Baza <- data.Normalization(baza.normalizacja, type="n2")
n3.Baza <- data.Normalization(baza.normalizacja, type="n3")
n3a.Baza <- data.Normalization(baza.normalizacja, type="n3a")
n4.Baza <- data.Normalization(baza.normalizacja, type="n4")
n5.Baza <- data.Normalization(baza.normalizacja, type="n5")
n6.Baza <- data.Normalization(baza.normalizacja, type="n6")
n7.Baza <- data.Normalization(baza.normalizacja, type="n7")
n8.Baza <- data.Normalization(baza.normalizacja, type="n8")
n9.Baza <- data.Normalization(baza.normalizacja, type="n9")
n10.Baza <- data.Normalization(baza.normalizacja, type="n10")
n11.Baza <- data.Normalization(baza.normalizacja, type="n11")
n12.Baza <- data.Normalization(baza.normalizacja, type="n12")
n12a.Baza <- data.Normalization(baza.normalizacja, type="n12a")
n13.Baza <- data.Normalization(baza.normalizacja, type="n13")
}
############ statystyki opisowe - prawdziwa baza
Statystyki.Bazy <- function(Baza.Do.Analizy)
{
  BazaSprawdzajaca <- Baza.Do.Analizy
Korelacja <- cor(BazaSprawdzajaca)
BazaSprawdzajaca <- data.frame(sort(BazaSprawdzajaca$X1), sort(BazaSprawdzajaca$X2), sort(BazaSprawdzajaca$X3),
                               sort(BazaSprawdzajaca$X4), sort(BazaSprawdzajaca$X5),
                              sort(BazaSprawdzajaca$X6), sort(BazaSprawdzajaca$X7),
                              sort(BazaSprawdzajaca$X8), sort(BazaSprawdzajaca$X9), sort(BazaSprawdzajaca$X10),
                              sort(BazaSprawdzajaca$X11), sort(BazaSprawdzajaca$X12))
X<- c("X1","X2","x3","X4","X5","X6","X7","X8","X9","X10","X11","X12")
colnames(BazaSprawdzajaca)<-X
srednia <- numeric(12);wariancja <- numeric(12); odchylenie <- numeric(12);
kurtoza <- numeric(12);skosnosc <- numeric(12);mediana <- numeric(12);odchylenie.medianowe <- numeric(12)
Rozstep <- numeric(12)
n <- length(BazaSprawdzajaca)
i <- 1
for(i in 1:12)
{
  srednia[i] <- mean(BazaSprawdzajaca[,i])
  wariancja[i] <- var(BazaSprawdzajaca[,i])
  odchylenie[i] <- sd(BazaSprawdzajaca[,i])
  kurtoza[i] <- kurtosis(BazaSprawdzajaca[,i])
  skosnosc[i] <- skewness(BazaSprawdzajaca[,i])
  mediana[i] <- median(BazaSprawdzajaca[,i])
  Rozstep[i] <- max(BazaSprawdzajaca[,i])-min(BazaSprawdzajaca[,i])
  odchylenie.medianowe[i] <- c(mad(BazaSprawdzajaca[,i]))
}
mediana <- t(as.matrix(mediana))

wynik <- list(srednia = srednia, 
               Wariancja = wariancja,
               Odchylenie = odchylenie,
               Kurtoza = kurtoza,
               Skosnosc = skosnosc,
               Mediana = mediana,
               Rozstep = Rozstep,
               Odch.medianowe = odchylenie.medianowe,
               Korelacja = Korelacja)
return(wynik)
}
