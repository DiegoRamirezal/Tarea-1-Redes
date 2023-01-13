# Tarea tweets

require(here)
library(igraph)
library(readr)
library(readxl)
library(tidyverse)
library(triangle)
library(MASS) # to access Animals data sets
library(scales)
library(ggplot2)
library(foreign)
library(summarytools)
#Base de datos

dbenlaces <- read_csv("red_completa_twitter_catrillanca.csv")
dbnodos <- read_csv("Nodos.csv")
nodos<-data.frame(unique(dbnodos$`1070`))




#plot de la red

red <- graph_from_data_frame(d = dbenlaces , vertices =nodos$unique.dbnodos..1070.., directed = TRUE)



componentes <- clusters(red)
componente_max<- which.max(componentes$csize)


#componente principal
red1 <- induced.subgraph(red, which(componentes$membership ==componente_max)) 
plot(red1, vertex.size = 2, vertex.color = "tomato",vertex.frame.color = NA,
     vertex.label.cex = .7,vertex.label = NA,edge.curved = 0,
     edge.arrow.size = .0003,edge.size = .0003, edge.width = .007,
     layout = layout_with_fr(red1) )

#componentes secuandarias
red2 <- induced.subgraph(red, which(componentes$membership !=componente_max)) 
plot(red2, vertex.size = 2, vertex.color = "tomato",vertex.frame.color = NA,
     vertex.label.cex = .7,vertex.label = NA,edge.curved = 0,
     edge.arrow.size = .0003,edge.size = .0003, edge.width = .007,
     layout = layout_with_fr(red2) )


E(red1) # cantidad de enlaces componente principal
V(red1) # cantidad de nodos componente principal

table(sapply(decompose(red),  FUN = vcount)) # proporciona componentes de la red

table(sapply(decompose(red), diameter)) # diametro para cada componente de la red



centr_degree(red)
mean_distance(red)
density <- graph.density(red)
reciprocity(red1)

####### identificar los grados por nodo (degree, in, out)

tab.p11 <- table(grado$`degree(red)`)

tab.p12 <- table(in1$degree.red..mode....in..)

tab.p13 <- table(out1$degree.red..mode....out..)


#####Histogramas

h1<-hist(degree(red),
         breaks =500,ylim=c(0,400),xlim=c(1,500),
         main="",xlab="Grado", ylab="Frecuencia"         )

h2<-hist(degree(red, mode="in"),breaks =700,ylim=c(0,2500),xlim=c(0,500),
     main="",xlab="Grado de entrada", ylab="Frecuencia")

h3<-hist(degree(red, mode="out"),breaks =200,ylim=c(0,2000),xlim=c(0,150),
         main="",xlab="Grado de salida", ylab="Frecuencia")


##### grafico log 

d.red <- as.data.frame(degree(red))
f.d.red<-as.data.frame(table(d.red$`degree(red)`))
f.d.red$Var1<-as.integer(f.d.red$Var1)

log<-ggplot(f.d.red, aes(x =Var1, y =Freq)) + geom_point() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
    labs(x='log(Grado)',y = "log(Frecuencia)")+
  theme_bw()


#### grado de entrada versus salida

indeg<-as.vector(degree(red, mode="in"))
outdeg<-as.vector(degree(red, mode="out"))

grados<-data.frame(indeg,outdeg)

invsout<-ggplot(grados, aes(x =outdeg, y =indeg)) + geom_point() +
   labs(x='Grado de salida',y = "Grado de entrada")+
  theme_bw()



ggsave(invsout, filename = "invsout.jpeg",
       dpi = 600, width = 7, height = 5)




#########senso triadas

triad.census(red)

plot(triad.census(red))
