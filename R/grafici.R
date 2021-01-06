#crea grafico delle serie giornaliere

#questa funzione disegna la serie e le crocette rosse con i dati mancanti
plotta.serie<-function(serie,parametro,type,LAG){

	if(missing(type) || (type!="yearly" && type!="monthly" && type!="daily")) stop()

	#ci serve sapere quante colonne ha serie. L'ultima colonna è quella con i dati da plottare	
	COLONNA.DATI<-ncol(serie)
	

	#parametro per ts dopo
	if(type=="monthly"){

		FREQUENCY<-12

	} else if(type=="daily"){ #yearly

		FREQUENCY<-365.25

	} else{

		FREQUENCY<-1

	}#fine su frequency

	#etichetta per asse y serie
	if(parametro=="prcp"){
		etichetta<-"mm"
	} else if(parametro=="tmax" || parametro=="tmin"){
		etichetta<-"°C"
	}

	serie %>% extract2("year")->anni
	annoI<-range(anni,na.rm=TRUE)[1]
	annoF<-range(anni,na.rm=TRUE)[2]

	if(missing(LAG)){

		LAG<-annoF-annoI+1

	}#su missing LAG


	#quanti grafici per pagina? Al massimo 4 per pagina
	(annoF-annoI+1)/LAG->rapporto

	if(rapporto<1){
		par(mfrow=c(1,1))
	}else{

		if(rapporto<=4){

			par(mfrow=c(rapporto,1))
	
		}else{

			par(mfrow=c(4,1))

		}

	}#su rapporto


	for(yy in seq(annoI,annoF,by=LAG)){

		which(anni>=yy & anni<=(yy+LAG-1))->index.anni
		#serie[index.anni,c(4)]->subserie
		#Utilizzando la psozione della colonna dei dati da plottare evitiamo di dover specificare il nome del parametro in serie
		#Quando le variabili vengono aggregate, la var aggregata viene chiamata "var", altrimenti nelle serie giornaliere compare
		#come prec o temp...
		serie[index.anni,] %>% extract2(COLONNA.DATI)->subserie	
		anni[index.anni]->sub.anni	

		if(all(is.na(subserie))) next

		#trasforma la subserie in oggetto ts, più diretto da graficare
		ts(subserie,frequency=FREQUENCY,start=yy)->subserie.ts

		plot(subserie.ts,ylab=etichetta)
		#quali dati mancanti?
		which(!is.na(subserie))->index.not.na
		which(is.na(subserie))->index.na

		#per aggregazione annuale e mensile nn compariranno i pallini rossi corrispondenti ai dati mancanti
		#al momento dell'aggregazione infatti per sum e mean si è utilizzato na.rm=TRUE. Bisognerebbe scrivere le funzioni medie 
		#e somma che restituiscano NA quando i dati disponibili sono troppo pochi	

		if(type=="yearly"){

			anni.mesi.giorni<-sub.anni
			#pallini blu per ogni anno
			points(anni.mesi.giorni[index.not.na],subserie[index.not.na],pch=21,bg="blue")

		}else if(type=="monthly"){

			rep(seq(0,11),LAG)->mesi
			anni.mesi.giorni<-sub.anni+mesi/12
			#pallini blu per ogni mese
			points(anni.mesi.giorni[index.not.na],subserie[index.not.na],pch=21,bg="blue")

		}else{

			giorni<-(0:364.25)
			#questa parte del codice va rivista ma nn è fondamentale adesso
			anni.mesi.giorni<-suppressWarnings(sub.anni+giorni/364.25)
			#croci rosse per dati mancanti
			points(anni.mesi.giorni[index.na],rep(0,length(index.na)),col="red",pch=3)

		}#su if



	}#fine ciclo for


}#fine plotta



#fun dovrà essere uguale a mean per la temperatura
# e sum per la preicpitazione
grafico.serie<-function(dati,annoI,annoF,output,aggrega="daily"){

	#manca qualcuno dei parametri non mi fermo (no stop) e vado avanti
	if(missing(output) | missing(dati) | all(is.na(dati$year)) | all(is.na(dati$month)) | all(is.na(dati$day)) | all(is.na(dati[,c(4)]))) return()

	parametro<-names(dati)[4]

	#se non trovo i nomi delle variabili mi fermo
        which(names(dati) %in% c("year","month","day"))->index
	stopifnot(length(index)==3)
	
	#se uno dei due parametri manca o se mancano entrambi,  grafico la serie dall'inizo alla fine
	if(missing(annoI) | missing(annoF)){
		messaggi("GRAFICO SERIE: parametri annoI e/o annoF non specificato/i")
		annoI<-years
		annoF<-yeare
	}

	#se ho passato i parametri faccio subset della serie
	dati %<>% filter(year>=annoI & year<=annoF)

	#apertura pdf
	pdf(file=paste(output,parametro,"_serie.pdf",sep="_"))

	if(aggrega!="daily"){

		if(parametro!="prcp"){
			#fun<-"mean"
			fun<-mean
		}else{
			#fun<-"sum"
			fun<-sum
		}#if su parametro

		#stringa.per.aggregare<-paste0(fun,"(",parametro,",na.rm=TRUE)")

		if(aggrega=="monthly"){
			#ddply(dati,c("year","month"),here(summarize),var=eval(parse(text=get("stringa.per.aggregare"))))->out
			dati %>% group_by(year,month) %>% summarise_(var=interp(~fun(par,na.rm=TRUE),par=as.name(parametro)))->out

			#aggrego e ottengo una serie nulla
			if(all(is.na(out$var))){
				dev.off()
				return()	
			}

			plotta.serie(serie=out,parametro,type="monthly",LAG=10)				
		
		}else{

			#ddply(dati,c("year"),here(summarize),var=eval(parse(text=get("stringa.per.aggregare"))))->out
			dati %>% group_by(year) %>% summarise_(var=interp(~fun(par,na.rm=TRUE),par=as.name(parametro)))->out

			#aggrego e ottengo una serie nulla
			if(all(is.na(out$var))){
				dev.off()
				return()	
			}

			plotta.serie(serie=out,parametro,type="yearly")					

		}#se non è monthly o yearly il programma non aggrega senza dire nulla


	}else{ #aggrega daily

		        plotta.serie(serie=dati,parametro,type="daily",LAG=5)		

	}#fine su aggrega
       
	dev.off() #fine pdf

}#fine grafico.serie
