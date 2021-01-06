source("funzioniControllo.R",local=FALSE)
source("grafici.R",local=FALSE)

###########################################################################################33
allqc<-function(idstaz,output){


	messaggio.errore(argomento=match.call())
	messaggi(sprintf("Running EXTRAqc, station %s",idstaz))

	#questi sono i nomi delle variabili utilizzati da allqc
	names(datos)<-c("year","month","day","pc","tx","tn")

#questo controllo verifica la presenza di valori ripetuti, sia nella precipitazione che nella temperatura
#Per la precipitazione viene fatto un apposito controllo sulla persistenza degli 0.

#Questo è il primo controllo da fare. Per esempio: se trova due anni di zeri li invalida. Il controllo 
#che trova invece serie ripetute (mensili o annuali) non dovrebbe trovare le serie affette da persistenza
	flatline_all(fileout=paste0(output,"flatline/",idstaz))

	#serie_uguali:
	# - serie annuali uguali: annulla la seconda e lascia invariata (non NA) la prima. I flag vengono posti a uno
	#   sia per la prima che per la seconda serie
	#serie mensili uguali: se si riferiscono a due mesi diversi (indipendentemente dall'anno), entrambe le serie sono invalidate
	# altrimenti se si riferiscono allo stesso mese, viene invalidata solo la prima serie 
	 serie_uguali(fileout=paste0(output,"serie_uguali/",idstaz))

####################
#Altri valori ripetuti
####################

# will list when tmax == tmin. Output goes to series.name_tmax_euqal_tmin.txt
#QUesto controllo deve venir prima di tmax_equal0_tmin. Infatti: se ho un blocco
#di valori tmax==tmin e in mezzo a questo blocco capita tmax=tmin=0,allora lo invalido
#e dopo vado a cercare tmax=tmin=0 che non cadono in bloccchi di valori continui.

#Se prima facessi il controllo tmax=tmin=0 e invalido, romperei una serie di valori continui uguali
#che quindi non troverei. Ad esempio:
#5.5 5.5
#6.6 6.6
#7.7 7.7
#0 0
#8.8 8.8 
#Se faccio prima il controllo tmax_equal0_tmin invaliderei gli 0. Nel controllo successivo (ipotizzando che LEN, il numero di
#valori ripetuti, sia uguale a 4) il fatto di avere NA NA tra 7.7 e 8.8 potrebbe impedirmi di rilevare il fatto che ho una sequenza 
#di tmax e tmin uguali. 

	tmax_equal_tmin(fileout=paste0("./extraqc/tmax_equal_tmin/",idstaz))


####################
#Valori nulli mal codificati come 0
####################

	#tmax==tmin==0. Questo controllo va dopo tmax==tmin
	tmax_equal0_tmin(fileout=paste0("./extraqc/tmax_equal0_tmin/",idstaz))


####################
#Valori impossibili
####################


	impossible_values(fileout=paste0("./extraqc/impossible_values/",idstaz))


####################
#Valori outliers
####################

	tmax_tmin_zscore(fileout=paste0("./extraqc/tmax_tmin_zscore/",idstaz))

	gap_checks(fileout=paste0("./extraqc/gap_checks/",idstaz))
	#prec95 va dopo i controlli su tmax e tmin perchè il calcolo del multiplo da moltiplicare 
	#al 95 percentile dipende dalla tmedia (media di tmax e tmin)	
	prec_95percentile(fileout=paste0("./extraqc/percentile95/",idstaz))


###################
#INTERNAL CONSISTENCY OF THE SERIES
###################

	jumps_tx_tn(fileout=paste0("./extraqc/jumps/",idstaz))


#######################
#Valori fisicamente impossibili
#######################

	#tmax < tmin+5. Output goes to series.name_tmaxmin.txt
	#I controlli che annullano due valori (tmax e tmin) 
	tmaxmin(fileout=paste0("./extraqc/tmaxmin/",idstaz))

	#lagged test	
	tmax_tmin_dtr(fileout=paste0("./extraqc/tmax_tmin_dtr/",idstaz))

	messaggi("Extra Quality Control Routines finished!!!")

}#fine allqc


###############################################################################################################
#funzione elabora: elabora stazione per stazione, mette insieme i dati dei tre parametri
#crea i grafici delle serie e poi chiama allqc che si occupa dei controlli veri e propri

#ACCETTA.SERIE.MANCANTI: se TRUE non viene controllato lista.serie. Questo significa che una stazione può
#apparire nel file codici.txt ma poi effettivamente non avere dati in nessun parametro (prcp, tmax, tmin)

#se ACCETTA.SERIE.MANCANTI==FALSE le serie elencate in codici.txt debbono per forza avere almeno un parametro
#(serie di dati)
elabora<-function(codici=NULL,param=NULL,ffile=NULL,dir.risultati=NULL,ACCETTA.SERIE.MANCANTI=TRUE){

	messaggio.errore(argomento=match.call())

	#numero di parametri
	length(param)->len.param

	for(ii in 1:len.param){

		if(!file.exists(ffile[ii])) param[ii]<-FALSE
	
		paste0("ELABORAZIONE PARAMETRO ",names(param)[ii]," : ",param[ii],"\n")->messaggio 
		messaggi(messaggio)


	}#fine ciclo for

	#questa variabile serve per scrivere una e una volta sola l'intestazione del file di statistiche
	INTESTAZIONE.PRCP<-TRUE
	INTESTAZIONE.TMAX<-TRUE			
	INTESTAZIONE.TMIN<-TRUE		

	numero.colonne<-c(242,242,242)				

	purrr::walk(sort(as.vector(codici)),.f=function(id.staz){

		print(id.staz)

		lapply(1:len.param,FUN=function(indice){

			if(param[indice]){

				paste(rep("d",numero.colonne[indice]),collapse="")->STRINGA.DATI
				paste0("iii",STRINGA.DATI)->STRINGA.TIPO	
				tryCatch({
		        		read_delim(ffile[indice],delim=";",col_names=TRUE,col_types=cols(yy=col_integer(),mm=col_integer(),dd=col_integer(),.default=col_double()))
				},warning=function(w){
					print(sprintf("read_delim: %s ha fallito uso read.csv",names(param)[indice]))
					read.csv(file=ffile[indice],sep=",",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
				})->dati

				names(dati)->nomi.dati

				which(nomi.dati %in% id.staz)->colonna

				if(length(colonna)){
		
					#return(dati[,c(1,2,3,colonna)])

					 return(dati %>% select(1,2,3,colonna))

				}else{
					return(NULL)
				}


			}

		})->lista.dati

		

		length(lista.dati[!sapply(lista.dati, is.null)])->lista.len
		#serie elencata in codici.txt senza dati effettvi: due casi..mi blocco oppure soprassiedo
		if(lista.len==0){
			if(ACCETTA.SERIE.MANCANTI==FALSE){      
		        	stop(sprintf("STAZIONE NON TROVATA: %s",id.staz))
			}else{
				return() #stazione successiva
			}	

		}#lista.len==0

		#lista con tre sottoliste: una per prec una per tmax e una per tmin
		#Queste tre sottoliste contenono i dati di una stazione specifica. Se un parametro
		#non va elaborato (oaram[ii] è FALSE) la sottolista corrispondente sarà vuota
		names(lista.dati)<-tolower(names(param))
	
		#se siamo qui almeno uno dei tre parametri per la stazione id.staz è disponibile
		#La gestione che segue per unire i dati (mediante merge) permette di gestire i casi 
		#in cui un parametro non inizia nello stesso anno degli altri parametri
		#Se invece si assumesse che tutti e tre i parametri partono dallo stesso giorno mese e anno
		#si potrebbe riscrivere il codice in forma più semplice

		if(is.null(lista.dati$prec)){

			if(is.null(lista.dati$tmax)){

				#dd<-lista.dati$tmin[,c(1,2,3)]
				dd<-lista.dati$tmin %>% select(yy,mm,dd)
				dd %<>% mutate(prec=NA,tmax=NA)
				#qui non c è bisogno di un full join
				dd$tmin<- lista.dati$tmin %>% extract2(4)
		
			}else{

				#temp<-lista.dati$tmax[,c(1,2,3)]
				temp<-lista.dati$tmax %>% select(yy,mm,dd)
				#temp$prec<-NA
				temp %<>% mutate(prec=NA)
				temp$tmax<-lista.dati$tmax %>% extract2(4) 

				if(is.null(lista.dati$tmin)){
					#temp$tmin<-NA
					#temp->dd
					dd<-temp %>% mutate(tmin=prec)
				}else{
					#merge(temp,lista.dati$tmin,by=c("yy","mm","dd"),all.x=TRUE,all.y=TRUE,sort=FALSE)->dd
					dd<-temp %>% full_join(lista.dati$tmin,by=c("yy","mm","dd")) %>% arrange(yy,mm,dd)
					#temp$tmin<-lista.dati$tmin %>% extract2(4) 
					#temp->dd
				}

			}


		}else{

			if(is.null(lista.dati$tmax)){

				temp<-lista.dati$prec
				#temp$tmax<-NA
				temp %<>% mutate(tmax=NA)

				if(is.null(lista.dati$tmin)){
	
					#temp$tmin<-NA
					#temp->dd
					dd <- temp %>% mutate(tmin=NA)
				}else{

					#merge(temp,lista.dati$tmin,by=c("yy","mm","dd"),all.x=TRUE,all.y=TRUE,sort=FALSE)->dd
					dd<- temp %>% full_join(lista.dati$tmin,by=c("yy","mm","dd")) %>% arrange(yy,mm,dd)	
					#temp$tmin<-lista.dati$tmin %>% extract2(4)
					#temp->dd 
				}


			}else{

				#merge(lista.dati$prec,lista.dati$tmax,by=c("yy","mm","dd"),all.x=TRUE,all.y=TRUE,sort=FALSE)->temp
				temp<-lista.dati$prec
				#temp$tmax<-lista.dati$tmax %>% extract2(4)
				temp %<>% full_join(lista.dati$tmax,by=c("yy","mm","dd")) %>% arrange(yy,mm,dd)	

				if(is.null(lista.dati$tmin)){
	
					#temp$tmin<-NA
					#temp->dd
					dd <- temp %>% mutate(tmin=NA)
				}else{

					#merge(temp,lista.dati$tmin,by=c("yy","mm","dd"),all.x=TRUE,all.y=TRUE,sort=FALSE)->dd
					dd<- temp %>% full_join(lista.dati$tmin,by=c("yy","mm","dd")) %>% arrange(yy,mm,dd)	
					#temp$tmin<- lista.dati$tmin %>% extract2(4)
					#temp->dd
				}


			}#if su tmax

		}#fine if	

	    	
	     if(ncol(dd)!=6) stop("Errore dd: numero di colonne diverso da 6")
	     #fondamentale rinominare qui le variabili	
	     names(dd)<-c("year","month","day","prcp","tmax","tmin")
	     #possiamo essere sicuri che merge mantenga l'ordine dei giorni mesi e anni? meglio di no	

	     #questo non serve più grazie agli arrange di cui sopra	  
	     #order(dd$year,dd$month,dd$day)->index
     
	     #lavoriamo su una variabile globale in modo di non dover passare
	     #il dataframe alle funzioni

	     if(exists("datos")) rm(datos,envir=.GlobalEnv)
	     if(exists("datos.senza.na")) rm(datos.senza.na,envir=.GlobalEnv)	

	     #assegnamo lo stesso data.frame a due variabili: uno che serve solo a tracciare i flag e che non viene invalidato
	     #l'altro che non contiene flag e i cui dati vengono di volta in volta invalidati		

	     #datos viene di volta in volta invalidato, questo garantisce che i controlli a cascata lavorino
	     #su dati via via privi di errori						
	     #assign("datos",dd[index,],envir=.GlobalEnv)
	     assign("datos",dd,envir=.GlobalEnv)
	     #questo è il data.frame che contiene i flag dei vari controlli e i dati originali senza invalidarli
	     #in questo modo alla fine del programma avremo un file con i dati originali nn modificati e
	     #i flag che ci dicono quali controlli sono risultati positivi o negativi	
#	     assign("datos.senza.na",dd[index,],envir=.GlobalEnv)
	     assign("datos.senza.na",dd,envir=.GlobalEnv)
	     #dd non ci serve più	
	     rm(dd)	

	     #years dovrebbe essere year start mentre yeare dovrebbe essere year end
	     #guido	
	     range(datos$year)->yrange	   		
	     years<-yrange[1] #anno inizio	
	     yeare<-yrange[2] #anno fine

	     assign("years",years,envir=.GlobalEnv)
	     assign("yeare",yeare,envir=.GlobalEnv)

	     #4 5 6 le posizioni di prcp tmax e tmin		
	     lapply(4:6,FUN=function(ii){
			#dir.grafici[1]: serie
			filena<-paste0(dir.risultati,"grafici_serie","/",id.staz)
			grafico.serie(dati=(datos %>% select(1,2,3,ii)),aggrega="daily",output=filena)	
			invisible()
     	     })#fine lapply	

	    allqc(idstaz=id.staz,output=dir.risultati)

	    #scrittura dei file di output: dati senza flag e invalidati
	    filena<-paste0(dir.risultati,"data","/",id.staz,".txt")		
     	    write_csv(x=datos,file=filena,append=F,col_names=TRUE)

	    #scrittura dei file di output: dati originali (non invalidati) e flag
	    filena<-paste0(dir.risultati,"data_con_flag","/",id.staz,".txt")		
     	    write_csv(x=datos.senza.na,file=filena,append=F,col_names=TRUE)

	    #scritture delle statistiche: quanti dati? quanti annullati?
	    #Per ogni riga scriviamo idstaz numero di dati flaggati su numero di dati (statistiche calcolate solo su dati non NA)

	    #ATTENZIONE. PER UNA BUONA RIUSCITA DEL FILE STATISTICHE I FLAG NON DEVONO INIZIARE CON LE PAROLE tmax, tmin e prcp	

	    lapply(4:6,FUN=function(colonna){
	   	 names(datos.senza.na)->nomi.datos
		
		 if(colonna==4) grep(".prcp",nomi.datos)->posizione	
		 if(colonna==5) grep(".tmax",nomi.datos)->posizione #la presenza del "." nel pattern serve per escludere la colonna dati "tmax" "tmin" "prcp"			 
		 if(colonna==6) grep(".tmin",nomi.datos)->posizione

		 #potrebbe succedere che faccio solo controlli di temperatura: quindi se colonna==0 "posizione" è 0				
		 if(!length(posizione)) return()
		 #datos.senza.na[,c(colonna,posizione)]->temporanei

		 datos.senza.na %>% select(all_of(c(colonna,posizione)))->temporanei	
		 which(is.na(temporanei[,c(1)]))->index.na
		 #eliminiamo gli NA (questo ci serve perchè le serie partono tutte da uno stesso anno, ovvero dall'anno della più lunga.
		 #Una serie potrebbe iniziare nel 1962 ma nel file comparirà come NA dal 1961. Non possiamo contare i flag che corrispondono
		 #anche a questa parte fittizia della serie. SOluzione: contiamo i flag solo in corrispondenza di dati non NA). 
		 if(length(index.na)==nrow(temporanei)) return()	
	 
		 if(length(index.na)) temporanei[-index.na,]->temporanei
	

		 lapply(2:ncol(temporanei),FUN=function(iCol){

		 	 sum(temporanei[temporanei[[c(iCol)]]!=0, ][[iCol]])

		 })->somma#fine lapply	

		 
		 names(somma)<-names(temporanei)[2:ncol(temporanei)]
		 
		 as_tibble(somma)->somma	

		 somma %<>% mutate(length=nrow(temporanei),idstaz=id.staz)


		 if(colonna==4){
	    	 	filena<-paste0(dir.risultati,"statistiche/prcp.txt")
			write_csv(as.vector(somma),file=filena,append=TRUE,col_names=INTESTAZIONE.PRCP)	
			INTESTAZIONE.PRCP<<-FALSE	
		 }else if(colonna==5){
	    	 	filena<-paste0(dir.risultati,"statistiche/tmax.txt")
			write_csv(as.vector(somma),file=filena,append=TRUE,col_names=INTESTAZIONE.TMAX)	
			INTESTAZIONE.TMAX<<-FALSE
		 }else if(colonna==6){
	    	 	filena<-paste0(dir.risultati,"statistiche/tmin.txt")
			write_csv(as.vector(somma),file=filena,append=TRUE,col_names=INTESTAZIONE.TMIN)
			INTESTAZIONE.TMIN<<-FALSE		
		 }	

		#l'intestazione va scritta solo all'inizio..ma va scritta per sapere a che flag si riferiscono le colonne


	    }) #fine lapply	
	    
	    
})#fine furrr/purrr
	
	#},mc.cores=2)#fine mclapply

}#fine funzione elabora
