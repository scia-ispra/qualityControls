#Lunedi 21 settembre 2015
#Questo programma verifica le serie di precipitazione e temperatura (serie giornaliere). Restituisce
#sia i dati a cui è stato applicato il flag, sia i dati originali con le colonne dei flag (una colonna per ciascun tipo
#di controllo).
rm(list=objects())
library("compiler")
library("stringr")
library("purrr")
library("chron") #contiene leap.year
library("stringr")
library("readr")
library("magrittr")
library("dplyr")
source("creaDirectory.R",local=FALSE)
source("elabora.R",local=FALSE)

options(warn=2,error=browser)
#####################################################################
###Inizio programma Guido
#####################################################################


#crea directory di output
creaDirOut(lista.directory=LISTA.DIRECTORY,cancella=TRUE)

####
#parametri da elaborare: se non se ne vuole elaborare uno, mettere la variabile a FALSE
#se un parametro è TRUE e non viene trovato il corrispettivo file allora la variabile
#viene posta a FALSE
parametri.da.elaborare<-c(PREC=TRUE,TMAX=TRUE,TMIN=TRUE)

#lettura del file con i codici delle stazioni
lista.codici<-"codici.txt"

read_delim(lista.codici,delim=";",col_names=FALSE) %>% extract2(1) ->codici

#lista dei file di input
file.parametri<-c("precipitazione.csv","tmax.csv","tmin.csv")

print(system.time({
#se uno dei parametri è FALSE viene comunque creata una colonna di NA nella rispettiva posizione
#quarta colonna per la prec, quinta per tmax, sesta per tmin
elabora(codici=codici,param=parametri.da.elaborare,ffile=file.parametri,dir.risultati)
}))
#####################




