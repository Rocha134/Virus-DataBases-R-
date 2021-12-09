library("seqinr")
getwd()
setwd("~/FASTA")
#Creación de una funcion que calcula el porcentaje de cada nucleotido en la secuencia
base.percentage <-function(dna){
a <- 0
t <- 0
g <- 0
c <- 0
for(i in 1:length(dna)){
if(dna[i] == "a"){
a <- a + 1
} else if(dna[i] =="t"){
t <- t + 1
} else if(dna[i] == "c"){
c <- c + 1
} else if(dna[i] == "g"){
g <- g + 1
}
}
print("Porcentaje de Adenina: ")
print((a/length(dna))*100)
print("Porcentaje de Timina: ")
print((t/length(dna))*100)
print("Porcentaje de Citosina: ")
print((c/length(dna))*100)
print("Porcentaje de Guanina: ")
print((g/length(dna))*100)
}
#Creación de función de la cadena complementaria
complement <-function(dna){
cdna <- c()
for(i in 1:length(dna)){
if(dna[i] == "a"){
cdna[i] <- "t"
} else if(dna[i] == "t"){
cdna[i] <- "a"
} else if(dna[i] == "c"){
cdna[i] <- "g"
} else if(dna[i] == "g"){
cdna[i] <- "c"
}
}
return(cdna)
}sarsCovid <- read.fasta("SARS-COVID.fasta")
Denguevirus <- read.fasta("Dengue-virus-1.fasta")
Mersvirus <- read.fasta("Mers.fasta")
WuhanHu1 <- read.fasta("Wuhan-HU-1.fasta")
Zikavirus <- read.fasta("Zika-Virus.fasta")
#Obtener la longitud
length(sarsCovid[[1]])
length(Denguevirus[[1]])
length(Mersvirus[[1]])
length(WuhanHu1[[1]])
length(Zikavirus[[1]])
#Obtener el contenido de nucleótidos
count(seq = sarsCovid[[1]], 1)
count(seq = Denguevirus[[1]], 1)
count(seq = Mersvirus[[1]], 1)
count(seq = WuhanHu1[[1]], 1)
count(seq = Zikavirus[[1]], 1)
#Recuperación de genoma como vector
vector_sarsCovid <- sarsCovid[[1]]
vector_Denguevirus <- Denguevirus[[1]]
vector_Mersvirus <- Mersvirus[[1]]
vector_WuhanHu1 <- WuhanHu1[[1]]
vector_Zikavirus <- Zikavirus[[1]]
#Cálculo de porcentaje de nucleótidos
length(vector_sarsCovid)
base.percentage(vector_sarsCovid)
length(vector_Denguevirus)
base.percentage(vector_Denguevirus)
length(vector_Mersvirus)
base.percentage(vector_Mersvirus)
length(vector_WuhanHu1)
base.percentage(vector_WuhanHu1)
length(vector_Zikavirus)
base.percentage(vector_Zikavirus)
#Calcular GC
GC(vector_sarsCovid)
GC(vector_Denguevirus)
GC(vector_Mersvirus)
GC(vector_WuhanHu1)
GC(vector_Zikavirus)
#Obtener la secuencia complementaria del genoma viralcomplement(vector_sarsCovid)
complement(vector_Denguevirus)
complement(vector_Mersvirus)
complement(vector_WuhanHu1)
complement(vector_Zikavirus)
#Tablas
table_basecontent<-data.frame(adenina=1:5,citocina=1.5,guanina=1.5,timina=1.5)
table_basecontent
rownames(table_basecontent)<-c("SarsCovid","Zika","WuhanHu1","Mers","Dengue")
table_basecontent
table_basecontent[1,]<-6
table_basecontent
table_basecontent[1,]<-6
table_basecontent[1,]<-count(sarsCovid[[1]],1)
table_basecontent[2,]<-count(Zikavirus[[1]],1)
table_basecontent[3,]<-count(WuhanHu1[[1]],1)
table_basecontent[4,]<-count(Mersvirus[[1]],1)
table_basecontent[5,]<-count(Denguevirus[[1]],1)
table_basecontent
