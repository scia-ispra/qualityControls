#crea directory necessarie per memorizzare i risultati
dir.risultati<-"./extraqc/"

dir.grafici<-c("grafici_serie")
dir.temperatura<-c("tmaxmin","tmax_equal0_tmin","tmax_equal_tmin","tmax_tmin_zscore","tmax_tmin_dtr")
dir.varie<-c("impossible_values","jumps","serie_uguali","flatline","gap_checks","percentile95")
dir.output<-c("data","data_con_flag","statistiche")

LISTA.DIRECTORY<<-c(dir.grafici,dir.temperatura,dir.varie,dir.output)

#se cancella ==TRUE cancella le directory esistenti
creaDirOut<-function(lista.directory,cancella=TRUE){

	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("lista.directory"))

	crea<-function(dir.base){

		lapply(lista.directory,FUN=function(nome){
		
			nome<-paste0(dir.base,nome)
        		if(!file.exists(nome)){dir.create(nome)}
		
			message(sprintf("CREO DIRECTORY %s:\n",nome))
			invisible(NULL)

		}) #fine lapply
	}#fine crea

 	if(cancella & file.exists(dir.risultati)){ suppressWarnings(unlink(dir.risultati,recursive=TRUE)) }

	dir.create(dir.risultati)
	crea(dir.base=dir.risultati)
  

}#fine funzione

#questa funzione evrifica i parametri. Se esiste il parametro "parametri.mai.nulli"
#conterrà la lista dei parametri che non dovranno essere mai nulli. Se "parametri.mai.nulli" non esiste
#allora verranno controllati tutti i parametri della funzione
messaggio.errore<-function(argomento,parametri.mai.nulli=NULL){

	argomento[[1]]->nome.funzione
	
	if(is.null(parametri.mai.nulli)){

		#qui ottengo la lista di tutti i parametri (grazie a formals)
		names(formals(as.character(nome.funzione)))->tutti.i.parametri

		for(iii in 1:length(tutti.i.parametri)){

			tutti.i.parametri[iii]->nome.parametro
			#se il parametro ha un valore preimpostato (default) anche se non compare 
			#in argomento non dovrà generare un errore
			if(!is.null(formals(as.character(nome.funzione)))) next
			if(!(nome.parametro %in% names(argomento))) stop(sprintf(paste0("Errore funzione '",nome.funzione,"' parametro '%s' mancante"),nome.parametro))

		}

	}else{ #fine if

		for(iii in 1:length(parametri.mai.nulli)){

			parametri.mai.nulli[iii]->nome.parametro
			if(!(nome.parametro %in% names(argomento))) stop(sprintf(paste0("Errore funzione '",nome.funzione,"' parametro '%s' mancante"),nome.parametro))

		}

	}

}#fine messaggio.errore

################àoutput generico messaggio
messaggi<-function(testo){
	message("##############################################")	
	message(testo)
	message("##############################################")	
}

