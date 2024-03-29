##### Biblioteki #####
library(ggplot2)
library(igraph)
library(dplyr)
library(randomNames)
library(stringr)
#### Przygotowanie bazy ####
set.seed(231047)
k = 200
p = 140
x = randomNames(k)
y = sample(1:p, replace = T, size = k)
z = sample(1:p, replace = T, size = k)
macierz = matrix(c(x, y, z), ncol = 3)
df = as.data.frame(macierz)
colnames(df) = c("Nazwa","X", "Y")
for(i in 2:3)
{
  df[[i]] = as.numeric(as.character(df[[i]]))
}
df[[1]] = as.character(df[[1]])
Clean_String <- function(string){
  temp = string
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- stringr::str_split(temp, " ")[[1]]
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}
imie  = c()
for(i in 1:nrow(df))
{
df.imie = (Clean_String(df$Nazwa[i]))
imie[i] = df.imie[length(df.imie)]
}
np = k - length(unique(imie))
rownames.imie = c(unique(imie), paste(imie[1:np],"n"))
rownames(df) = rownames.imie
df = df[,-1]
#df$Nazwa = imie
##### Budowanie Sieci ####
Graf.DT = graph.data.frame(df, directed = T)
Graf.DF = graph.data.frame(df, directed = F)
Graf.DT = simplify(Graf.DT, remove.multiple = T, remove.loops = T)
Graf.DF = simplify(Graf.DF, remove.multiple = T, remove.loops = T)
layout1 = layout.fruchterman.reingold(Graf.DT)
layout2 = layout.fruchterman.reingold(Graf.DF)
plot(Graf.DT)
plot(Graf.DF)
V(Graf.DT)$color <- "grey"
E(Graf.DT)$color <- "grey"
graph.density(Graf.DF)
### Wykres wzgl�dem kraw�dzi
par(mfrow=c(1,2))
plot(Graf.DT, vertex.color = heat.colors(52),
     vertex.size = degree(Graf.DT)*1.5,
     edge.arrow.size = 0.2,
     vertex.label.cex = 0.9,
     layout = layout.kamada.kawai)
plot(Graf.DF, vertex.color = heat.colors(52),
     vertex.size = degree(Graf.DF)*1.5,
     edge.arrow.size = 0.3,
     vertex.label.cex = 0.9,
     layout = layout.kamada.kawai)
par(mfrow=c(1,1))
plot(Graf.DT, vertex.color = heat.colors(52),
     vertex.size = degree(Graf.DT)*1.8,
     edge.arrow.size = 0.2,
     vertex.label.cex = 0.9,
     layout = layout.kamada.kawai)
plot(Graf.DT,
     vertex.color =rainbow(52),
     vertex.size = degree(Graf.DT)*1.8,
     edge.arrow.size = 0.3, vertex.label.cex = 0.9,
     layout = layout.graphopt)
mean(degree(Graf.DT, mode = 'in'))
mean(degree(Graf.DT, mode = 'out'))
average.path.length(Graf.DT)
diameter(Graf.DT)
edge_density(Graf.DT, loops = F)
graph.density(Graf.DT, loop=F)
Graf.DT.Petle = graph.data.frame(df, directed = T)
Graf.DT.Petle = simplify(Graf.DT.Petle, remove.multiple = T, remove.loops = F)
Macierz = matrix(c(mean(degree(Graf.DT, mode = 'in')),
                   average.path.length(Graf.DT),
                   diameter(Graf.DT),
                   edge_density(Graf.DT, loops = F),
                   graph.density(Graf.DT, loop=F),transitivity(Graf.DT),
                   mean(degree(Graf.DT.Petle, mode = 'in')),
                   average.path.length(Graf.DT.Petle),
                   diameter(Graf.DT.Petle),
                   edge_density(Graf.DT.Petle, loops = F),
                   graph.density(Graf.DT.Petle, loop=F),
                   transitivity(Graf.DT.Petle)), ncol = 2)
colnames(Macierz) =c("Sie� bez p�tli", "Sie� z p�tlami")
rownames(Macierz) = c("�rednia liczba kraw�dzi",
                      "�rednia d�ugo�� �cie�ki",
                      "Najd�u�sza �cie�ka",
                      "G�sto�� grafu na podstawie kraw�dzi",
                      "G�sto�� grafu na podstawie kraw�dzi - druga funkcja",
                      "Prawdopodobie�stwo �e s�siednie wierzcho�ki s� po��czone")
Macierz
#### Budowanie wykres�w na podstawie blisko�ci

plot(Graf.DT, vertex.color = heat.colors(52),
     vertex.size = (4200*closeness(Graf.DT, mode = 'all', weights = NA)),
     edge.arrow.size = 0.2,
     vertex.label.cex = 0.9,
     layout = layout.kamada.kawai)
closeness(Graf.DF, mode = "all")

Graf.DT.Density <- degree.distribution(Graf.DT)
graf.dt.density.df = matrix(c(Graf.DT.Density, 1:length(Graf.DT.Density)), ncol = 2)
graf.dt.density.df = as.data.frame(graf.dt.density.df)
hist(Graf.DT.Density, col = "orange")

ggplot(data = graf.dt.density.df, aes(x = V2, y = V1)) +
  geom_histogram(stat = "identity", fill = heat.colors(9), col = "red") + 
  xlab(" ") + 
  ylab("Liczebno��") + 
  ggtitle("Histogram G�sto�ci wierzcho�k�w")

hs = hub_score(Graf.DT)$vector
as = authority.score(Graf.DT)$vector
set.seed(231047)
plot(Graf.DT, vertex.color = rainbow(52),vertex.size = hs*15, main = "Hubs",
     edge.arrow.size = .1,layout = layout.kamada.kawai) ### 
set.seed(231047)
plot(Graf.DT, vertex.color = rainbow(52),vertex.size =  as*20, main = "Autohority",
     edge.arrow.size = .1,layout = layout.kamada.kawai) ### Do kogo ludzie pisz�
