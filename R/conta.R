rm(list=objects())
library("dplyr")
library("magrittr")

parametri<-c("precipitazione","tmax","tmin")

conteggi<-c("precipitazione"=0,"tmax"=0,"tmin"=0)

lapply(1:3,FUN=function(indice){

	parametri[indice]->nome.parametro

	tbl_df(read.csv(file=paste0(nome.parametro,".csv"),sep=",",head=TRUE,check.names=FALSE,stringsAsFactors=FALSE))->dati
	ncol(dati)->colonne

	lapply(4:colonne,FUN=function(colPos){

		which(!is.na(dati[,colPos]))->index
		length(index)

	})->numeri #fine lapply	

	sum(unlist(numeri))->>conteggi[indice]


})#fine lapply

print(conteggi)


