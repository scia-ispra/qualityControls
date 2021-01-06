###########################
#FLAG DA ASSEGNARE AI DATI
###########################

FLAG.VALIDO<<-0
FLAG.NON.VALIDO<<-1
#FLAG DUBBIO PER Z-SCORE E PER GAP CHEK NEL CASO DELLA PRECIPITAZIONE. Non invalidando subito questi dati si può vedere
#cosa risulta dai controlli "spatial checks".
FLAG.DUBBIO<<-2

#si vogliono invalidare i dati rilevati da gapchecks e zscore? (ad esempio a un secondo controllo ..)
INVALIDA<<-FALSE

if(INVALIDA){
	FLAG.DUBBIO<<-FLAG.NON.VALIDO
}

####################################################################################################################
################# funzione per la ricerca di blocchi di valori ripetuti
####################################################################################################################

#12 dicembre 2013
#funzioni per cercare blocchi di dati ripetuti
#
#
#Diverse implementazioni della stessa funzione (in.blocco). Quella più efficiente è
#in.blocco2 (in.blocco3 è costruita su cicli foor ed è la più lenta)-

#Alla funzione va passato un vettore x che rappresenta una serie (ad esempio, potrebbe essere una serie di precipitazione
#o di temperatura associata a un dataframe in cui compaiono anni mesi e giorni). La funzione restituisce un vettore di
#indici relativi alla posizione dei valori ripetuti nella serie. Per funzionare, la serie deve essere completa (quindi
#giorni mancanti vanno riempiti). Gli NA vengono ignorati (come specificato dal NOAA: infatti queste funzioni sosno state #sviluppate sulla base del documento NOAA che descrive i controlli sui dati giornalieri utilizzati per il dataset
#globale GHCN. Queste funzioni sono state scritte per il controllo dei dati giornalieri per il nostro database).

#drop=T. Gli zeri vengono ignorati (come gli NA). drop=T va utilizzato per le serie di precipitazione (come specificato dal
#NOAA, per le serie di precipitazione vanno ricercati valori ripetuti che differiscono da 0).

#nmax= dimensione minima di ciascun blocco (Il NOAA specifica almeno 20 valori).

#La funzione longPPrec è simile a in.blocco2, ma serve a verificare la presenza di lunghi (valore di default #nmax=180giorni cioeè 6mesi) blocchi di zeri (0). Questa funzione è pensata per le serie di precipitazione (in.blocco2 per le serie di prec
#va utilizzata con drop=T, per ignorare la ripetizione degli 0. Lunghe serie di 0, precipitazione nulla, sono possibili.
#Sono invece da invalidare serie troppo lunghe, tipo 6 mesi e più).



#in.blocco2 è la versione più performante.
#
#Il parametro drop (TRUE o FALSE) permette di distinguere il caso
#della precipitazione che richiede di trascurare la persistenza degli 0

in.blocco2<-function(x,drop=F,nmax=3){
  
  INVQUI<-732 #due anni: questo valore definisce la dimensione iniziale
              #del evttore che serve a contenere gli indici che identificano i valori 
              #ripetuti
 
  if(!length(x)) return() #serie vuota
  
  #se drop è TRUE (serve per la precipitazione) la persistenza dei valori
  #pari a 0 viene ignorata

   if(drop){
     x[x==0]<-NA
   }
  
  
  which(is.na(x))->index.na
  length(index.na)->len.na
  if(len.na)
    x[-index.na]->x
  
  if(!length(x)) return()
  
  
  diff(x)->diffPrima
  which(diffPrima==0)->index0
  
  diff(index0)->diffSeconda
  which(diffSeconda!=1)->index1
  c(0,index1,length(index0))->index2
  diff(index2)->diffTerza
  which((diffTerza+1)>=nmax)->trovati
  
  if(!length(trovati)) return() #nessun duplicato

  index0[index2[trovati+1]]+1 ->estremi2
  index0[index2[trovati]+1] ->estremi1
  
  length(estremi1)->len.estremi #gli estremi di ciascun blocco di valori ripetuti
  #estremi1 è l'indice che identifica linizio di un blocco
  #estremi2 è l'indice che identifica la fine di ciascun blocco
  
  invalidaQui<-numeric(length=(INVQUI)) #indici degli elementi (non NA) da invalidare
  
  pos1<-1
  i<-1
  while(1){
    step<-(estremi2[i]-estremi1[i])
    #grow object
    invalidaQui[pos1:(pos1+step)]<-seq(estremi1[i],estremi2[i])   
    pos1<-(pos1+step+1)
    
    if(pos1>round(INVQUI/1.5)){#grow object
      invalidaQui<-c(invalidaQui,numeric(length(INVQUI)))          
    }#grow object
    
    i<-i+1
    if(i>len.estremi) break
  }
  
  invalidaQui[1:(pos1-1)]->invalidaQui
  
  if(length(index.na)){#index.na
    for(j in 1:len.na){
      
      which(invalidaQui>=index.na[j])->index
      
      if(!length(index)) break
      invalidaQui[index]<-invalidaQui[index]+1
    }#ciclo su j
  }#if su index.na

  invalidaQui

  
}#fine funzione

####################################################################################################################
####################################################################################################################
#trova lunghe serie di zeri (di default 6 mesi)
#implementazione identica a in.blocchi2
longPPrec<-function(x,nmax=180){
  
  INVQUI<-732
  
  if(!length(x) || length(x)< nmax ) return(NULL)
    
  which(is.na(x))->index.na
  length(index.na)->len.na
  if(len.na)
    x[-index.na]->x
  
  if(!length(x)) return()
  
  which(x==0)->index0
  diff(index0)->diffSeconda
  which(diffSeconda!=1)->index1
  c(0,index1,length(index0))->index2
  diff(index2)->diffTerza
  which((diffTerza+1)>=nmax)->trovati
  
  if(!length(trovati)) return() #nessun duplicato
  
  index0[index2[trovati]+1] ->estremi1
  index0[index2[trovati+1]] ->estremi2
  
  length(estremi1)->len.estremi #gli estremi di ciascun blocco di valori ripetuti
  #estremi1 è l'indice che identifica linizio di un blocco
  #estremi2 è l'indice che identifica la fine di ciascun blocco
  
  invalidaQui<-numeric(length=(INVQUI)) #indici degli elementi (non NA) da invalidare
  
  pos1<-1
  i<-1
  while(1){
    step<-(estremi2[i]-estremi1[i])
    #grow object
    invalidaQui[pos1:(pos1+step)]<-seq(estremi1[i],estremi2[i])   
    pos1<-(pos1+step+1)
    
    if(pos1>round(INVQUI/1.5)){#grow object
      invalidaQui<-c(invalidaQui,numeric(length(INVQUI)))          
    }#grow object
    
    i<-i+1
    if(i>len.estremi) break
  }
  
  invalidaQui[1:(pos1-1)]->invalidaQui
  
  if(length(index.na)){#index.na
    for(j in 1:len.na){
      
      which(invalidaQui>=index.na[j])->index
      
      if(!length(index)) break
      invalidaQui[index]<-invalidaQui[index]+1
    }#ciclo su j
  }#if su index.na
  
  invalidaQui

}#fine funzione


messaggi<-function(testo){
	message("##############################################")	
	message(testo)
	message("##############################################")	
}


####################################################################################################################
####################################################################################################################
flatline_all<-function(fileout,nmax.pers=180,nmax.prec=20,nmax.temp=20) {

		#colonne per i flag
		datos.senza.na$flatline.prcp0<<-FLAG.VALIDO #persistenza
		datos.senza.na$flatline.prcp<<-FLAG.VALIDO
		datos.senza.na$flatline.tmax<<-FLAG.VALIDO
		datos.senza.na$flatline.tmin<<-FLAG.VALIDO

		messaggi(testo="FLATLINE CONTROL: verifica presenza di serie di valori ripetuti")

		messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("fileout"))

		if(is.null(nmax.pers)){
			nmax.pers<-180
			messaggi(sprintf("Funzione flatline_all: nmax.pers is missing, nmax.pers default is %s days",nmax.pers))
		}


		if(is.null(nmax.prec)){		
			nmax.prec<-20
			messaggi(sprintf("Funzione flatline_all: nmax.pers is missing, nmax.pers default is %s days",nmax.prec))
		}

		if(is.null(nmax.temp)){		
			nmax.temp<-10
			messaggi(sprintf("Funzione flatline_all: nmax.pers is missing, nmax.temp default is %s days",nmax.temp))
		}


		for(ii in 4:6){

			if(all(is.na(datos[,c(ii)] ) ) ) next

			#nel caso della precipitazione: persistenza degli 0
			if(ii==4){

				filena<-paste0(fileout,"_persistenza.txt")
				#longPPrec(x=datos[,c(ii)],nmax=nmax.pers)->index			
				datos %>% extract2(ii) %>% longPPrec(nmax=nmax.pers)->index

				if(length(index)){
					#questo scrive nella directory specifica per il controllo, non scrive
					#tutta la serie, solo la parte problematica
					write_csv(datos[index,],file=filena,col_names=FALSE,append=TRUE) 
					#data.frame che serve per tracciare i flag ma senza invalidare i dati
					datos.senza.na[index,]$flatline.prcp0<<-FLAG.NON.VALIDO
					datos[index,]$prcp<<-NA	
				}#fine if			

			}

			#valori ripetuti
			if(ii==4){
				filena<-paste0(fileout,"_flatlinePrec.txt")
				nmax<-nmax.prec
				DROP<-TRUE
			}else if(ii==5){
				filena<-paste0(fileout,"_flatlinetx.txt")
				nmax<-nmax.temp
				DROP<-FALSE
			}else if(ii==6){
				filena<-paste0(fileout,"_flatlinetn.txt")
				nmax<-nmax.temp
				DROP<-FALSE
			}

			#in.blocco2(x=datos[,c(ii)],nmax=nmax,drop=DROP)->index
			datos %>% extract2(ii) %>% in.blocco2(nmax=nmax,drop=DROP)->index

			if(length(index)){ 
				write_csv(datos[index,],file=filena,col_names=FALSE,append=TRUE)
				if(ii==4){ 
					datos.senza.na[index,]$flatline.prcp<<-FLAG.NON.VALIDO
				}else if(ii==5){
					datos.senza.na[index,]$flatline.tmax<<-FLAG.NON.VALIDO
				}else if(ii==6){
					datos.senza.na[index,]$flatline.tmin<<-FLAG.NON.VALIDO
				}else{
					stop("something gone wrong")
				}
 
				datos[index,c(ii)]<<-NA	
			}
			
		}#fine ciclo for


}#fine flatline_all

####################################################################################################################
####################################################################################################################

tmaxmin <-function(fileout) {

	#colonne per i flag
	datos.senza.na <<- datos.senza.na %>% mutate(lower0.tmaxmin=FLAG.VALIDO)

	messaggio.errore(argomento=match.call())

	messaggi(testo="VALORI IMPOSSIBILI: verifica che tmax non sia < di tmin")

	if ((all(is.na(datos$tmax))) || (all(is.na(datos$tmin))) ) return()
	filena<-paste(fileout,'_tmaxmin.txt',sep='')
	which((datos$tmin-datos$tmax)>= 5)->index  #la max è minore della min e almeno distano 5 gradi
	
	if(length(index)){
		
		#creaGrafico
		lapply(1:length(index),FUN=function(iii){
		  
		  index[iii]->indice
		  #verifichiamo che tra indice e l'elementro precedente in "index"
		  #passino almeno 30 valori: questo serve per evitare troppi grafici quando i valori individuati
		  #sono tutti uno accanto all'altro
		  if(iii>1){
		    
		    if((indice-index[iii-1])<=15) return() #nn rifacciamo il frafico
		    
		  }#fine su if
		  
		  casuale<-round(runif(n=1,1,1000),0)
		  etichetta<-"°C"
		  
		  if(indice<=15){
		    seq(1,indice+15)->sequenza
		  }else if((nrow(datos)-indice)<=15){
		    seq(indice-15,nrow(datos))->sequenza
		  }else{
		    seq(indice-15,indice+15)->sequenza
		  }
		    
		  #estraiamo i dati per definire il range dei valori su asse y
		  datos %>% slice(sequenza) %>% extract2("tmax")->valtmax
		  datos %>% slice(sequenza) %>% extract2("tmin")->valtmin
		  
		  which((valtmin-valtmax)>=5)->posizione

		  paste0(fileout,'_tmaxmin.')->nome.grafico
		  min(c(valtmin,valtmax),na.rm=TRUE)->minimo
		  max(c(valtmin,valtmax),na.rm=TRUE)->massimo
		  pdf(paste0(nome.grafico,casuale,".pdf"))
		  plot(c(valtmax),ylab=etichetta,main="Tmin >= Tmax+5",type="n",ylim=c(minimo,massimo))
		  abline(v=posizione,lty=2)
		  points(1:length(sequenza),valtmax,bg="red",type="l")
		  points(1:length(sequenza),valtmin,bg="blue",type="l")
		  points(1:length(sequenza),valtmax,bg="red",pch=21)
		  points(1:length(sequenza),valtmin,bg="blue",pch=21)
		  dev.off()
		  
		  
		})#fine lapply su indice
	  
	  write_csv(datos[index,],file=paste0(fileout,"_tmaxmin.txt"),col_names=FALSE,append=TRUE)
	  #data.frame che serve per tracciare i flag ma senza invalidare i dati
	  datos.senza.na[index,]$lower0.tmaxmin<<-FLAG.NON.VALIDO
	  
	  datos[index,]$tmax<<-NA	
	  datos[index,]$tmin<<-NA	

	}#fine if

}#fine tmaxmin


####################################################################################################################
########################### se tmax==tmin==0 annulla o tmin==tmax=-17.8 ovvero 0°C in Faraneith

tmax_equal0_tmin <- function(fileout) {

	#colonne per i flag
	datos.senza.na$equal0.tmaxmin<<-FLAG.VALIDO

	messaggi(testo="VALORI NULLI MAL CODIFICATI: tmax==tmin==0")

	messaggio.errore(argomento=match.call())

	if ((all(is.na(datos$tmax))) || (all(is.na(datos$tmin))) ) return()

	which((datos$tmax==0 & datos$tmin==0) | (datos$tmax==-17.8 & datos$tmin==-17.8))->index
	#qui non mi interessa nessuna continuità dei risultati, il valore è dubbio ogni volta
	#che tmax==tmin==0. 0 potrebbe essere utilizzato erroneamente come indicativo di dato mancante.
	if(length(index)){

	  #creaGrafico
	  lapply(1:length(index),FUN=function(iii){
	    
	    index[iii]->indice
	    #verifichiamo che tra indice e l'elementro precedente in "index"
	    #passino almeno 30 valori: questo serve per evitare troppi grafici quando i valori individuati
	    #sono tutti uno accanto all'altro
	    if(iii>1){
	      
	      if((indice-index[iii-1])<=15) return() #nn rifacciamo il frafico
	      
	    }#fine su if
	    
	    casuale<-round(runif(n=1,1,1000),0)
	    etichetta<-"°C"
	    
	    if(indice<=15){
	      seq(1,indice+15)->sequenza
	    }else if((nrow(datos)-indice)<=15){
	      seq(indice-15,nrow(datos))->sequenza
	    }else{
	      seq(indice-15,indice+15)->sequenza
	    }
	    
	    #estraiamo i dati per definire il range dei valori su asse y
	    datos %>% slice(sequenza) %>% extract2("tmax")->valtmax
	    datos %>% slice(sequenza) %>% extract2("tmin")->valtmin
	    
	    which((valtmin==0 & valtmax==0) | (valtmin==-17.8 & valtmax==-17.8))->posizione

	    paste0(fileout,'_tmax_equal0_tmin.')->nome.grafico
	    min(c(valtmin,valtmax),na.rm=TRUE)->minimo
	    max(c(valtmin,valtmax),na.rm=TRUE)->massimo
	    pdf(paste0(nome.grafico,casuale,".pdf"))
	    plot(c(valtmax),ylab=etichetta,main="Tmax & Tmax == 0",type="n",ylim=c(minimo,massimo))
	    abline(v=posizione,lty=2)
	    points(1:length(sequenza),valtmax,bg="red",type="l")
	    points(1:length(sequenza),valtmin,bg="blue",type="l")
	    points(1:length(sequenza),valtmax,bg="red",pch=21)
	    points(1:length(sequenza),valtmin,bg="blue",pch=21)
	    dev.off()
	    
	    
	  })#fine lapply su indice	  
	  
	  
		write_csv(datos[index,],file=paste0(fileout,"_tmax_equal0_tmin.txt"),col_names=FALSE,append=TRUE)
		#data.frame che serve per tracciare i flag ma senza invalidare i dati
		datos.senza.na[index,]$equal0.tmaxmin<<-FLAG.NON.VALIDO
		datos[index,]$tmax<<-NA	
		datos[index,]$tmin<<-NA	
	}

}#fine tmax_equal0_tmin

####################################################################################################################
##############################################controllo NOAA: giorni in cui tmax ==tmin per almeno dieci giorni

#LEN: numero di valori ripetuti
tmax_equal_tmin <- function(fileout,LEN=10) {

	#colonne per i flag
	datos.senza.na$equal.tmaxmin<<-FLAG.VALIDO

	messaggi(testo="VALORI RIPETUTI: tmax==tmin")

	if((LEN==1) || is.null(LEN)){
		warnings("tmax_equal_tmin non funziona con LEN=1")
		LEN<-4
	}


	if ((all(is.na(datos$tmax))) || (all(is.na(datos$tmin))) ) return()

	round((datos$tmax-datos$tmin),1)->differenza

	#prima di tutto troviamo le serie di 0 ripetuti (ovvero blocchi di datio in cui tmax==tmin)
	longPPrec(differenza,nmax=LEN)->indice
	
	#non trovo nulla? me ne vado
	if(!length(indice) || (length(indice)<LEN)) return()

	#indice mi da la posizione degli 0. Devo verificare che "indice" si riferisca a blocchi di dati continui (non
	#necessariamente uguali tra un giorno e il successivo). Ad esempio:
	#3.1 3.1
	#2.3 2.3
	#2.3 2.3
	#4.4 4.4
	#........... tmax meno tmin mi daranno 0. Devo essere sicuro che questi 0 non si riferiscano a valori sparpagliati
	#nella serie ma a blocchi di valori contigui. UTILIZZO IL PARAMETRO LEN PER FOCALIZZARMI SOLO SU BLOCCHI DI 0
	#(in altre parole un giorno singolo in cui tmax==tmin non lo invalido)

	in.blocco2(diff(indice),nmax=(LEN))->indice2 #a questo punto fare in.blocco su tmax o tmin è lo stesso
	
	#coorezione del 2 novembre 2015. Se a in.blocco2 passiamo "diff(indice)" ci perdiamo.
	#nel risultato finale l'ultimo giorno della serie. Per correggere il risultato
	#cerchiamo dove indice2 ha un diff !=1 e aggiungiamo un valore (l'ultimo indice+1).
	
	#in.blocco2 e longPPrec di per se non hanno problemi. IL problema si presenta utilizzando in.blocco2 con
	#diff
	which(diff(indice2)!=1)->index.fix
	#se non ci sono valori !=1 aggiungi un valore alla fine di indice2. Il valore aggiunto è l'ultimo valore
	#di indice2 +1

	if(length(index.fix)){
	  #index.fix va incrementtato prima di 1 poi di due poi di tre perchè via via che inserisco elementi in indice2
	  #la posizione dei valori individuati si sposta verso destra
	  for(ii in 1:length(index.fix)) { append(x=indice2,values=(indice2[ii+index.fix[ii]-1]+1),after=(ii+index.fix[ii]-1))->indice2 }
	}#fine if su index.fix
	
	#quest'ultimo elemento va comunque inserito (ad esempio nel caso in cui non trovo nessun valore !=1 devo cmq
	#aggiungere/aggiustare l'ultimo valore)
	indice2[length(indice2)+1]<-indice2[length(indice2)]+1
	#fine correzione del 2 novembre 2015. Se a in.blocco2 passiamo "diff(indice)" ci perdiamo.

	#in.blocco2 trova i blocchi di 1 (vale a dire che gli indici si riferiscono a 0 vicini). Il rischio è che trovi
	#anche blocchi di 2 di 3 di 4 etc etc .....ovvero 0 che invece di distare "1" (contigui) distano "2 giorni"..o "3 giorni" etc etc

	#Però con LEN abbastanza grande il risultato dovrebbe restituirmi i risultati che cerco ovvero eventuali blocchi di 1
	#che corrispondono a blocchi di 0, ovvero differenze di tmax e tmin ==0 contigue. Quanto è probabile trovare un blocco di 2 contigui??
	#Un blocco di 3 contigui? LEN abbastanza grande dovrebbe preservarci da errori
	if(!is.null(indice2)){
	  
		write_csv(datos[indice,][indice2,],file=paste0(fileout,"_tmax_equal_tmin.txt"),col_names=FALSE,append=TRUE)
		#colonne per i flag
		datos.senza.na[indice,][indice2,]$equal.tmaxmin<<-FLAG.NON.VALIDO
		#invalidat tutti meno un dato che rimane integro
		datos[indice,][indice2,]$tmax<<-NA
		datos[indice,][indice2,]$tmin<<-NA
	}

}#fine tmax_euql_tmin


################gap checks:
#questa funzione prende i dati di un determinato mese, li ordina dal più piccolo al più grande e calcola la differenza (gap) tra
#dati consecutivi, quindi cerca i gap che superano un determinato threhsold (gap.temp e gap.prcp). Il passo successivo è identificare
#il valore che corrisponde a quel gap. Tutti i valori di quel mese >= di quel valore sono annullati (per la temperatura questo viene fatto sia
#sui valori positivi che negativi). In pratica il controllo cerca i valori che sono nettamente separati da tutti gli altri

gap_checks<- function(fileout,gap.temp=10,gap.prcp=300) {

	datos.senza.na$gapcheck.tmax<<-FLAG.VALIDO
	datos.senza.na$gapcheck.tmin<<-FLAG.VALIDO
	datos.senza.na$gapcheck.prcp<<-FLAG.VALIDO

	messaggi(testo="RICERCA DI OUTLIERS: gap check")
	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("fileout"))

	if(is.null(gap.temp)) gap.temp<-10
	if(is.null(gap.prcp)) gap.prcp<-300

	verifica_gap<-function(x,gap,parametro){

		messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("x","gap","parametro"))
		paste0(fileout,"_gapchecks_",parametro,".txt")->filena #nome file di output

		if(all(is.na(x %>% extract2(4)))) return() #tutto NA
		
		for(mm in 1:12){

		  trovaAnomali<-function(xx,index.mesi){
		    
		    xx %>% slice(index.mesi) %>% extract2(4) %>% order ->index.order
		    xx %>% slice(index.mesi) %>% slice(index.order) %>% extract2(4) ->serie
		    
		    diff(serie)->serie.gaps
		    which(serie.gaps>gap)->index.gaps
		    
		    if(!length(index.gaps)) return(NULL)
		    
		    median(serie,na.rm=TRUE)->mediana
		    #identifichiamo mediante la mediana quali valori anomali cadono alla sinistra e quali alla destra
		    #della serie "serie"

		    which(serie[index.gaps]<mediana)->indici.left		    
		    which(serie[index.gaps+1]>mediana)->indici.right
		    
		    if(length(indici.left)){
		      
		      valore.left<-serie[index.gaps][length(indici.left)]

          which(xx[index.mesi,4]<=valore.left)->soluzioneLeft		      

		    }else{
		     
		      soluzioneLeft<-NULL
		       
		    }#su indici.left
		    
		    if(length(indici.right)){
		      
		      valore.right<-serie[index.gaps+1][indici.right[1]]
		      which(xx[index.mesi,4]>=valore.right)->soluzioneRight		 		      

		    }else{
		      
		      soluzioneRight<-NULL		      
		      
		    }#su indici.right
		    
		    return(c(soluzioneLeft,soluzioneRight))
		    
		  }#trovaAnomali
		  
		  indiceFinalePlus<-NULL
		  indiceFinaleMinus<-NULL
		  
		  which(x$month==mm & x[,c(4)]>=0)->indexPlus

		  #questo controllo è stato cambiato: la stazione cassano d'adda in lombardia
		  #dura meno di un anno e per la tmax non ha valori nel mese di ottobre
		  #if(!length(index.mm)){browser(); stop("errore?")} #ci devono essere neccariamente valori >=0 indipendentemente dal parametro
		  if(length(indexPlus)>=90) trovaAnomali(xx=x,index.mesi=indexPlus)->indiceFinalePlus

		  which(x$month==mm & x[,c(4)]< 0)->indexMinus
		  if(length(indexMinus)>=90) trovaAnomali(xx=x,index.mesi=indexMinus)->indiceFinaleMinus
		  
		  if(is.null(indiceFinalePlus) & is.null(indiceFinaleMinus)) next #passa al mese successivo

			#grafico dei valori trovati
			grafico<-function(nome,indice,index.mm,colonna){
		
					casuale<-round(runif(n=1,1,1000),0)
					etichetta<-"°C"
					if(colonna==4) etichetta<-"mm"

					pdf(paste0(nome,casuale,".pdf"))
						#plot(datos[index.mm,c(colonna)],ylab=etichetta,main="GAP CHECK OUTLIERS")
						#points(indice.finale,datos[index.mm,][indice.finale,][,c(colonna)],bg="red",pch=21)
						plot((datos[index.mm,] %>% extract2(colonna)),ylab=etichetta,main="GAP CHECK OUTLIERS")
						points(indice,(datos[index.mm,][indice,]%>% extract2(colonna)),bg="red",pch=21)
					dev.off()

			}#fine grafico
			##############################fine grafico

		  scriviRisultato<-function(index,indexFinale){
		    
		      write_csv(datos[index,][indexFinale,],file=filena,col_names=FALSE,append=TRUE)
		      
		      if(parametro=="prcp"){
		        
		        grafico(nome=filena,indice=indexFinale,index.mm=index,colonna=4)
		        datos.senza.na[index,][indexFinale,]$gapcheck.prcp<<-FLAG.DUBBIO
		        datos[index,][indexFinale,]$prcp<<-NA								
		        
		      }else if(parametro=="tmax"){
		        
		        grafico(nome=filena,indice=indexFinale,index.mm=index,colonna=5)
		        datos.senza.na[index,][indexFinale,]$gapcheck.tmax<<-FLAG.DUBBIO
		        datos[index,][indexFinale,]$tmax<<-NA
		        
		      }else if(parametro=="tmin"){
		        
		        grafico(nome=filena,indice=indexFinale,index.mm=index,colonna=6)
		        datos.senza.na[index,][indexFinale,]$gapcheck.tmin<<-FLAG.DUBBIO
		        datos[index,][indexFinale,]$tmin<<-NA
		        
		      }else{#fine if su parametro
		        
		        stop("PARAMETRO NON RICONOSCIUTO")						
		        
		      }#su parametro

  	  }#fine funzione scriviRisultato
		  
		  if(!is.null(indiceFinalePlus)) scriviRisultato(index=indexPlus,indexFinale=indiceFinalePlus)
		  if(!is.null(indiceFinaleMinus)) scriviRisultato(index=indexMinus,indexFinale=indiceFinaleMinus)

		}#ciclo su mesi		

	}#fine verifica_gap

	datos %>% select(1,2,3,4) %>% verifica_gap(gap=gap.prcp,parametro="prcp")
	datos %>% select(1,2,3,5) %>% verifica_gap(gap=gap.temp,parametro="tmax")
	datos %>% select(1,2,3,6) %>% verifica_gap(gap=gap.temp,parametro="tmin")

}#gap_checks


####################################################################################################################
##############################################controllo NOAA
#NUMERO.DATI: numero minimo di valori non NA necessario per il calcolo della media e della std
#ZSTD: threshold per zscore
#BIWEIGHT: utilizzare la media semplice o la media robusta (noaa utilizza media robusta)

tmax_tmin_zscore <- function(fileout,NUMERO.DATI=100,ZSTD=6,winSize=7,BIWEIGHT=TRUE) {

	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("fileout"))

	datos.senza.na$zscore.tmax<<-FLAG.VALIDO
	datos.senza.na$zscore.tmin<<-FLAG.VALIDO

	messaggi(testo="RICERCA DI OUTLIERS: zscore")

	if ((all(is.na(datos$tmax))) && (all(is.na(datos$tmin))) ) return()

	if(is.null(NUMERO.DATI)) NUMERO.DATI<-100
	if(is.null(ZSTD)) ZSTD<-6
	if(is.null(BIWEIGHT)) BIWEIGHT<-TRUE
	if(is.null(winSize)) winSize<-7 # 7+7..15 giorni

	nrow(datos)->numero.dati
	contatorezScore<-1	#contatore per numerare i file pdf
	for(mese in 1:12){	

		for(giorno in 1:31){

				which(datos$month==mese & datos$day==giorno)->indice

				if(!length(indice)) next #30 febbraio per esempio

				indice-winSize->indice.left
				indice+winSize->indice.right

				length(indice.right)->len

				while(indice.left[1]<=0){

					indice.left[1]<-indice.left[1]+1

				}#fine while


				while(indice.right[len]>numero.dati){

					indice.right[len]<-indice.right[len]-1

				}#fine while				


				vettore.indici<-""

				for(ii in 1:len){

					paste0(indice.left[ii],":",indice.right[ii])->vettore.indici[ii]

				}#ciclo su for
	
				#la forma più facile per estrarre i dati sulla base degli indici in indici.left e indici.right
				#è costruire un file di testo come quello in str.indici
				paste0("c(",paste(vettore.indici,collapse=","),")")->str.indici

				my_zscore<-function(x,indici.per.stat,indici,filena,parametro){

					if(is.null(x) || is.null(indici.per.stat) || is.null (indici) || 
						is.null(filena) || is.null(parametro)) stop("ERRORE PARAMETRO FUNZIONE my_zscore")


					paste0(filena,'_',parametro,'_zscore.txt')->filena

					x[eval(parse(text=indici.per.stat))]->valori

					which(is.na(valori))->index

					if(length(index)) valori[-index]->valori
					
					#se sono meno di 100 i valori non NA  salta controllo
					if(length(valori)<NUMERO.DATI) return()

					#valori ora contiene tutti i valori nella serie che corrispondono a un determinato giorno e mese
					#e tutti i giorni che rientrano in una finestra di dimensione winSize

					#sulla base di questi dati calcolo la media e la std con cui poi calcolare lo z-score
					median(valori)->mediana

					pesi<-function(x,c=7.5,mediana){

						mad(x)->my.mad

						u<-(x-mediana)/(c*my.mad)

						which(abs(u)>=1)->index.u
						if(length(index.u)) u[index.u]<-0

						return(round(u,2))

					}#fine pesi

					#biweight mean: vedi Lanzante 1996, resistant tobust and non parametric techniques..

					biw.mean<-function(x,pesi,mediana){

						biw<-mediana+(( sum((x-mediana)*( 1-(pesi)^2 )^2) )/( sum((1-(pesi)^2)^2) ))
						return(round(biw,2))	
							
					}#fine biweighted mean

					biw.std<-function(x,pesi,mediana){

						len<-length(x)

						numeratore<-sqrt(len*sum(((x-mediana)^2)*(1-pesi^2)^4 ))
						denominatore<-abs(sum((1-(pesi^2))*(1-5*(pesi^2))))					

						biw<-(numeratore/denominatore)
						
						return(round(biw,2))
				
					}#fine biweighted std

					####
					if(BIWEIGHT){
						pesi(x=valori,mediana=mediana)->uuu
						biw.mean(x=valori,pesi=uuu,mediana=mediana)->media
						biw.std(x=valori,pesi=uuu,mediana=mediana)->std.dev
					}else{
					#i valori oltre 6 volte lo z-score vanno eliminati: MEDIA E STD SEMPLICI
						mean(valori)->media
						sqrt(var(valori))->std.dev
					}

					x[indice]->dati.da.verificare
					abs((dati.da.verificare-media)/std.dev)->vett.zscore
					
					which(vett.zscore > ZSTD)->index.zscore

					if(!length(index.zscore)) return()

					#se arriviamo qui, significa che abbiamo trovato dato che super il threshold	
					#in termini di zscore

					###########Grafico zscore		

					  #creaGrafico
					    
					    etichetta<-"°C"
					    
					    #estraiamo i dati per definire il range dei valori su asse y
					    datos %>% slice(indici)->temporaneo
				 	    temporaneo %>% extract2(parametro)->valori
					    valori[!is.na(valori)]->valori

					    #valore anomalo
					    temporaneo %>% slice(index.zscore) ->temporaneo.zscore
					    temporaneo.zscore %>% extract2(parametro)->outlier #valore
					    paste(temporaneo.zscore %>% select(2,3) %>% slice(1),collapse="-") ->mytime #anno e mese 						
	
					    which(valori %in% outlier)->posizione	
					    unlist(str_split(filena,'\\.'))[2]->nome.grafico

					    pdf(paste0("./",nome.grafico,contatorezScore,".pdf"))
						    plot(valori,ylab=etichetta,main=paste0("Z-score: ",mytime))		
						    points(posizione,outlier,pch=21,bg="red")
					    dev.off()
					    
					    contatorezScore<<-contatorezScore+1 			

					############fine grafico zscore	

					write_csv(datos[indici,][index.zscore,],file=filena,col_names=FALSE,append=TRUE)
					#INVALIDAZIONE DEI DATI E DEI FLAG
					if(parametro=="tmax"){
						datos.senza.na[indici,][index.zscore,]$zscore.tmax<<-FLAG.DUBBIO
						datos[indici,][index.zscore,]$tmax<<-NA
					}else if(parametro=="tmin"){
						datos.senza.na[indici,][index.zscore,]$zscore.tmin<<-FLAG.DUBBIO
						datos[indici,][index.zscore,]$tmin<<-NA
					}else{
						stop("FUNZIONE my_zscore: parametro non riconosciuto")
					}


				}#fine my_zscore

				datos %>% select(tmax) %>% extract2(1) %>% my_zscore(indici.per.stat=str.indici,indici=indice,filena=fileout,parametro="tmax")
				datos %>% select(tmin) %>% extract2(1) %>% my_zscore(indici.per.stat=str.indici,indici=indice,filena=fileout,parametro="tmin")
		
		}#fine su giorno

	}#fine su mese

}#fine tmax_tmin_zscore

#####################################################
####################################################################################################################
##############################################controllo NOAA
prec_95percentile<- function(fileout,NUMERO.DATI=20,winSize=14) {

	messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("fileout"))

	datos.senza.na$perc95.prcp<<-FLAG.VALIDO

	messaggi(testo="RICERCA DI OUTLIERS: valori oltre il 95 percentile")

	if (all(is.na(datos$prcp))) return()

	if(is.null(NUMERO.DATI)) NUMERO.DATI<-20
	if(is.null(winSize)) winSize<-14 # 7+7..15 giorni

	nrow(datos)->numero.dati
	contatorePerc95<-1 #contatore per file pdf	
	for(mese in 1:12){	

		for(giorno in 1:31){

				which(datos$month==mese & datos$day==giorno)->indice

				if(!length(indice)) next #30 febbraio per esempio

				#definiamo qui i multipli da usare per il calcolo della soglia. Tale multiplo dipende dalla temperatura media
				(datos[indice,]$tmax+datos[indice,]$tmin)/2->tmedia
				multiplo<-rep(9,length(tmedia))
				which(tmedia<=0)->index.media
				if(length(index.media)) multiplo[index.media]<-5

				indice-winSize->indice.left
				indice+winSize->indice.right

				length(indice.right)->len

				while(indice.left[1]<=0){

					indice.left[1]<-indice.left[1]+1

				}#fine while


				while(indice.right[len]>numero.dati){

					indice.right[len]<-indice.right[len]-1

				}#fine while				


				vettore.indici<-""

				for(ii in 1:len){

					paste0(indice.left[ii],":",indice.right[ii])->vettore.indici[ii]

				}#ciclo su for
	
				#la forma più facile per estrarre i dati sulla base degli indici in indici.left e indici.right
				#è costruire un file di testo come quello in str.indici
				paste0("c(",paste(vettore.indici,collapse=","),")")->str.indici

				my_perc95<-function(x,indici.per.stat,indici,filena){

					if(is.null(x) || is.null(indici.per.stat) || is.null (indici) || 
						is.null(filena)) stop("ERRORE PARAMETRO FUNZIONE my_perc95")


					x[eval(parse(text=indici.per.stat))]->valori

					which(is.na(valori) | (valori<0.1))->index

					if(length(index)) valori[-index]->valori
					
					#se sono meno di 100 i valori non NA  salta controllo
					if(length(valori)<NUMERO.DATI) return()

					#valori ora contiene tutti i valori nella serie che corrispondono a un determinato giorno e mese
					#e tutti i giorni che rientrano in una finestra di dimensione winSize

					#sulla base di questi dati calcolo il 95 percentile
					quantile(valori,prob=0.95)->pvalori95
					pvalori95*multiplo->soglia
					x[indice]->dati.da.verificare	

					which(dati.da.verificare>soglia)->index.soglia

					soglia<-pvalori95*multiplo
					if(!length(index.soglia)) return()
					#se arriviamo qui, significa che abbiamo trovato dato che supera soglia
					write_csv(datos[indici,][index.soglia,],file=filena,col_names=FALSE,append=TRUE)
					#INVALIDAZIONE DEI DATI E DEI FLAG

					#grafico
					pdf(paste0(filena,".",contatorePerc95,".pdf"))
					
						hist(valori,breaks=seq(0,round(max(dati.da.verificare[index.soglia]),0)+300,2.5),col="red",main="Valori oltre 5/9 volte il 95° percentile")
					dev.off()
					contatorePerc95<<-contatorePerc95+1
					#fine grafico

					datos.senza.na[indici,][index.soglia,]$perc95.prcp<<-FLAG.DUBBIO
					datos[indici,][index.soglia,]$prcp<<-NA

				}#fine my_perc95


				my_perc95(datos$prcp,indici.per.stat=str.indici,indici=indice,
				filena=paste(fileout,'_perc95_prcp.txt',sep=''))
		

		}#fine su giorno

	}#fine su mese

}#fine prec_95percentile

####################################################################################################################
###########################CONTROLLO NOAA SU DTR
tmax_tmin_dtr <- function(fileout,THRESHOLD=35) {

	datos.senza.na$dtr.tmax<<-FLAG.VALIDO
	datos.senza.na$dtr.tmin<<-FLAG.VALIDO
	
	messaggi(testo="CONSISTENCY CHEKS: diurnal temperature range test")

	if ((all(is.na(datos$tmax))) || (all(is.na(datos$tmin))) ) return()

	nrow(datos)->numero.dati
	contatoreDTR<-1
	verifica<-function(serie1,serie2,fun,num.dati,parametro){

		#va da 2 a num.dati-1 perchè per ogni dato faccio il confronto con quello precedente e successivo. Il primo
		#e l'lultimo valore non hanno precedente/successivo
		for(ii in 2:(num.dati-1) ){
		
			indice.left<-ii-1
			indice<-ii
			indice.right<-ii+1
			
			serie2[c(indice.left,indice,indice.right)]->sub.serie
			which(is.na(sub.serie))->index.na
			if(length(index.na)) next #devono essere tutti e tre diversi da NA	

			serie1[ii]->valore1
			if(is.na(valore1)) next

			#funzione da applicare alla minima (max) o alla massima (min)
			paste0(fun,"(sub.serie)")->stringa

			eval(parse(text=stringa))->valore2

			if(abs(valore1-valore2) >= THRESHOLD){

				###############################
				#grafico dtr
				  etichetta<-"°C"
					  
				  if(indice<=15){
				    seq(1,indice+15)->sequenza
				  }else if((nrow(datos)-indice)<=15){
				    seq(indice-15,nrow(datos))->sequenza
				  }else{
				    seq(indice-15,indice+15)->sequenza
				  }
					    
				  #estraiamo i dati per definire il range dei valori su asse y
				  serie1[sequenza]->temporaneo.serie1
				  serie2[sequenza]->temporaneo.serie2
				  min(c(temporaneo.serie1,temporaneo.serie2),na.rm=TRUE)->minimo	
				  max(c(temporaneo.serie1,temporaneo.serie2),na.rm=TRUE)->massimo

				  which(sequenza %in% indice)->posizione
				  ifelse(fun=="max",par<-"tmax",par<-"tmin")
						 
				  pdf(paste0(fileout,'_',par,'_dtr.',contatoreDTR,".pdf"))
				  	plot(sequenza,temporaneo.serie1,ylab=etichetta,main=paste0("Diurnal Temperature Range - ",THRESHOLD,"- check"),type="n",ylim=c(minimo,massimo))
					points(sequenza,temporaneo.serie1,type="l")
					points(sequenza,temporaneo.serie2,type="l")
					points(sequenza,temporaneo.serie1,pch=21,bg="red")
					points(sequenza,temporaneo.serie2,pch=21,bg="blue")
					abline(v=posizione,lty=2)
				  dev.off()
				  contatoreDTR<<-contatoreDTR+1	
				#fine grafico dtr
				###############################

				filena<-paste(fileout,'_',par,'_dtr.txt',sep='')
				write_csv(datos[indice.left,],file=filena,col_names=FALSE,append=TRUE)
				write_csv(datos[indice,],file=filena,col_names=FALSE,append=TRUE)
				write_csv(datos[indice.right,],file=filena,col_names=FALSE,append=TRUE)
				
				
				#i flag variano in base a chi sia serie1 e serie2. Possiamo distinguere i due casi:
				#se fun==min allora sto testando tmax. Se fun==max allora sto testando tmin
				
					if(fun=="min"){
				  		#flagghiamo
				  		datos.senza.na[c(indice.left,indice,indice.right),]$dtr.tmax<<-FLAG.DUBBIO
				  		datos.senza.na[indice,]$dtr.tmin<<-FLAG.DUBBIO
					  	#ogni volta che fallisce il test dobbiamo flaggare sia la tmax che la tmin
						datos[c(indice.left,indice,indice.right),]$tmax<<-NA
						datos[indice,]$tmin<<-NA

					}else{
					  	#flagghiamo
					  	datos.senza.na[c(indice.left,indice,indice.right),]$dtr.tmin<<-FLAG.DUBBIO
					  	datos.senza.na[indice,]$dtr.tmax<<-FLAG.DUBBIO
					  	#ogni volta che fallisce il test dobbiamo flaggare sia la tmax che la tmin
					  	datos[c(indice.left,indice,indice.right),]$tmin<<-NA
					  	datos[indice,]$tmax<<-NA
					}#if su fun
				
				#IMPORTANTE: annullare anche serie2, altrimenti nel ciclo successivo i dati non validi
				#saranno ancora presenti (invalidati su datos ma non su serie con cui si sta lavorando!)
				#questo annullamento non necessita distinzione: serie1 annulla solo un valore
				#serie2 annulla tre valori
				serie1[indice]<-NA
				serie2[c(indice.left,indice,indice.right)]<-NA        
				
				
			}#fine if	

		}#fine ciclo for


	}#fine verifica

	#prendo il massimo della temperatura minima	
	verifica(serie1=datos$tmax,serie2=datos$tmin,fun="max",num.dati=numero.dati)
	#prendo il minimo della temperatura massima	
	verifica(serie1=datos$tmin,serie2=datos$tmax,fun="min",num.dati=numero.dati)


}#fine tmax_tmin_dtr

####################################################################################################################
#################### controllo NOAA: questo controllo verifica che non vi siano anni uguali (serie giornaliere) o mesi uguali (serie giornaliere)
#Idea: aggrega i dati giornalieri a livello annuale e cerca gli anni a cui corrispondono valori medi uguali. Verifica se
#le corrispondenti serie giornaliere sono uguali.

#Aggrega i dati a livello mensile e cerca gli anni a cui corrispondono i valori mensili uguali. Verifica se le corrispettive serie giornaliere (mesi)
#sono uguali
serie_uguali<-function(fileout){

	messaggi(testo="REPEATED SERIES CONTROL: verifica presenza di serie ripetute")

	#colonne per i flag
	datos.senza.na$serieUguali.prcp<<-FLAG.VALIDO
	datos.senza.na$serieUguali.tmax<<-FLAG.VALIDO
	datos.senza.na$serieUguali.tmin<<-FLAG.VALIDO

	#controllo su serie annuali

	#ddply(datos,c("year"),summarize,prcp=round(mean(prcp,na.rm=TRUE),2),tmax=round(mean(tmax,na.rm=TRUE),1),tmin=round(mean(tmin,na.rm=TRUE),1))->out
	datos %>% group_by(year) %>% summarise(prcp=round(mean(prcp,na.rm=TRUE),2),tmax=round(mean(tmax,na.rm=TRUE),1),tmin=round(mean(tmin,na.rm=TRUE),1))->out	

	#out contiene il valore medio anno per anno. Ci sono valori medi uguali? se si, allora passiamo a fare il confronto
	#tra i valori giornalieri

	#colonna si riferisce alla posizione del parametro (pc,tx,tn) in datos e non in out
	uguali<-function(serie,anni=NULL,colonna=NULL,mesi=NULL){

		#queste due variabili nn possono mai essere nulle
		messaggio.errore(argomento=match.call(),parametri.mai.nulli=c("colonna","anni"))
		if(all(is.na(serie))) return()

		order(serie)->index
		#ordiniamo i valori medi e vediamo se ci sono blocchi di almeno due valori annuali uguali
		serie[index]->serie.ordinata

		#cerchiamo i blocchi
		in.blocco2(serie.ordinata,nmax=2)->zzz
		if(length(zzz)<=1) return()

		#out$year[index[zzz]] mi da gli anni che corrispondono ai valori medi uguali
		#A questo punto devo prendere i corrispettivi valori giornalieri e confrontarli
		anni[index][zzz]->anni.sospetti
		#anni.sospetti->anni.sospetti.out #in questo vettore annullo gli anni che non hanno duplicati nei giornalieri
		#più facile sarebbe assumere che gli anni sospetti siano a coppie di due, ma in realtà (come capita)
		#potrebbero capitare anche tre anni sospetti..oer questo "seq"	va con by=1 e ogni volta devo
		#verificare che anni.sospetti[ii] sia == a anni.sospetti[ii+1]

		if(!is.null(mesi)){

			mesi[index][zzz]->mesi.sospetti
			#mesi.sospetti->mesi.sospetti.out 

		}#su mesi		



		#anni.buoni e mesi.buoni contengono gli anni "buoni" da flaggare come invalidi..nome sfortunato	
		mesi.buoni<-c()
		anni.buoni<-c()

		for(ii in seq(1,length(anni.sospetti),by=1)){

			if((ii+1) > length(anni.sospetti)) break
			if((serie.ordinata[zzz][ii+1]) != (serie.ordinata[zzz][ii])) next
			if(serie.ordinata[zzz][ii]==0) next

			if(is.null(mesi)){

				#subset(datos$year==anni.sospetti[ii])[,colonna]->serie.uno
				#subset(datos,datos$year==anni.sospetti[ii+1])[,colonna]->serie.due

				datos %>% filter(year==anni.sospetti[ii]) %>% extract2(colonna)->serie.uno
				datos %>% filter(year==anni.sospetti[ii+1]) %>% extract2(colonna)->serie.due


			}else{

				#subset(datos,datos$year==anni.sospetti[ii] & datos$month==mesi.sospetti[ii])[,colonna]->serie.uno
				#subset(datos,datos$year==anni.sospetti[ii+1] & datos$month==mesi.sospetti[ii+1])[,colonna]->serie.due

				datos %>% filter(year==anni.sospetti[ii] & month==mesi.sospetti[ii]) %>% extract2(colonna)->serie.uno
				datos %>% filter(year==anni.sospetti[ii+1] & datos$month==mesi.sospetti[ii+1]) %>% extract2(colonna)->serie.due

				#Il controllo sul fatto che serie.uno e serie.due non siano tutte di zeri serve per evitare
				#che due mesi asciutti siano ritenuti "una ripetizione" (questo vale nel caso della precipitazione)
				#per cui potrebbe essere valido che un mese di tutti 0 si osservi più volte nella serie.		
				#Serie di 0 troppo lunghe o 0 in Tmax e Tmin sono cercate in altre parti del programma

				if((colonna==4) & ((length(serie.uno[serie.uno==0])>25) | (length(serie.uno[serie.due==0])>25))) next				
				
				#anni.sospetti.out[c(ii,ii+1)]<-NA
				#mesi.sospetti.out[c(ii,ii+1)]<-NA
				#next		
			

			}#

			length(serie.uno)->len.uno
			length(serie.due)->len.due

			min(len.uno,len.due)->minima.lunghezza
			if(is.na(minima.lunghezza)) stop("errore minima lunghezza")

			#le due serie potrebbero non avere uguali lunghezze. In tal caso
			#dobbiamo vedere la lunghezza della serie più corta

			serie.uno[1:minima.lunghezza]-serie.due[1:minima.lunghezza]->differenza

			#quanti NA ha la serie differenza? se ho più di TOT NA il confronto perde senso (e la presenza di NA)
				#potrebbe portare a risultati errati. La conta di NA va fatta sulla serie differenza. Serie uno e Serie due
				#potrebbero avere un numero di NA accettabile ma la differenza no (ad esempio 10 != NA all'inizio in serie.uno
				#e 10 !=NA alla fine in serie.due..serie differenza tutta NA)
				which(is.na(differenza))->index.na
				if(length(index.na)>=15) next 

				if(all(differenza==0,na.rm=TRUE)){

					anni.buoni<-c(anni.buoni,anni.sospetti[c(ii,ii+1)])				
					if(!is.null(mesi)) mesi.buoni<-c(mesi.buoni,mesi.sospetti[c(ii,ii+1)])	
		
				}
##############


		}#fine ciclo for

		if(is.null(mesi)){
			#return(anni.sospetti.out[!is.na(anni.sospetti.out)])
			return(anni.buoni)
		}else{
			#return(cbind(anni.sospetti.out[!is.na(anni.sospetti.out)],mesi.sospetti.out[!is.na(anni.sospetti.out)]))
			return(cbind(anni.buoni,mesi.buoni))	
		}	 

	}#fine uguali

	
	#serie annuali duplicate	
	#uguali(serie=out$prcp,anni=out$year,colonna=4)->anni.duplicati.pc
	#uguali(serie=out$tmax,anni=out$year,colonna=5)->anni.duplicati.tx
	#uguali(serie=out$tmin,anni=out$year,colonna=6)->anni.duplicati.tn

	out %>% extract2("year")->outYear

	out %>% extract2("prcp") %>% uguali(anni=outYear,colonna=4)->anni.duplicati.pc
	out %>% extract2("tmax") %>% uguali(anni=outYear,colonna=5)->anni.duplicati.tx
	out %>% extract2("tmin") %>% uguali(anni=outYear,colonna=6)->anni.duplicati.tn


	filena<-paste(fileout,'_pc_serie_uguali.txt',sep='')
	if(length(anni.duplicati.pc)){
		#questo scrive nella directory specifica del controllo il pezzo di serie errato
		write_csv(as.data.frame(anni.duplicati.pc),file=filena,col_names=FALSE,append=TRUE)

		#i risultati sono in numero paro? si..allora invalido il secondo e mi tengo la prima serie
		#i risultati sono in numero disparo? qulacosa è andato storto?
		if( length(anni.duplicati.pc)%%2 !=0 ) {stop("numero di anni ripetuti disparo, che fare per invalidare?")	}
		
		for(hhh in seq(2,length(anni.duplicati.pc),by=2)){

			which(datos$year==anni.duplicati.pc[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
	
			datos[index.year,]$prcp<<-NA

		}#fine ciclo for	


		#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
		#ho annullato solo la seconda serie)
		for(hhh in 1:length(anni.duplicati.pc)){

			which(datos.senza.na$year==anni.duplicati.pc[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
			datos.senza.na[index.year,]$serieUguali.prcp<<-FLAG.NON.VALIDO

		}#fine ciclo for			

	}#fine if

	filena<-paste(fileout,'_tx_serie_uguali.txt',sep='')
	if(length(anni.duplicati.tx)){
		#questo scrive nella directory specifica del controllo il pezzo di serie errato
		write_csv(as.data.frame(anni.duplicati.tx),file=filena,col_names=FALSE,append=TRUE)


		#i risultati sono in numero paro? si..allora invalido il secondo e mi tengo la prima serie
		#i risultati sono in numero disparo? qulacosa è andato storto?
		if( length(anni.duplicati.tx)%%2 !=0 ) {stop("numero di anni ripetuti disparo, che fare per invalidare?") }
		
		for(hhh in seq(2,length(anni.duplicati.tx),by=2)){

			which(datos$year==anni.duplicati.tx[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
			datos[index.year,]$tmax<<-NA

		}#fine ciclo for	


		#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
		#ho annullato solo la seconda serie)
		for(hhh in 1:length(anni.duplicati.tx)){

			which(datos.senza.na$year==anni.duplicati.tx[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
			datos.senza.na[index.year,]$serieUguali.tmax<<-FLAG.NON.VALIDO

		}#fine ciclo for			


	}#fine if

	filena<-paste(fileout,'_tn_serie_uguali.txt',sep='')
	if(length(anni.duplicati.tn)){
		#questo scrive nella directory specifica del controllo il pezzo di serie errato
		write_csv(as.data.frame(anni.duplicati.tn),file=filena,col_names=FALSE,append=TRUE)

		#i risultati sono in numero paro? si..allora invalido il secondo e mi tengo la prima serie
		#i risultati sono in numero disparo? qulacosa è andato storto?
		if( length(anni.duplicati.tn)%%2 !=0 ){stop("numero di anni ripetuti disparo, che fare per invalidare?")}	

		for(hhh in seq(2,length(anni.duplicati.tn),by=2)){

			which(datos$year==anni.duplicati.tn[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
			datos[index.year,]$tmin<<-NA

		}#fine ciclo for	


		#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
		#ho annullato solo la seconda serie)
		for(hhh in 1:length(anni.duplicati.tn)){

			which(datos.senza.na$year==anni.duplicati.tn[hhh])->index.year
			stopifnot(index.year!=0)
			#annullo dati in dato
			datos.senza.na[index.year,]$serieUguali.tmin<<-FLAG.NON.VALIDO

		}#fine ciclo for			


	}#fine if	



	#serie mensili duplicate
	#ddply(datos,c("year","month"),summarize,prcp=round(mean(prcp,na.rm=TRUE),2),tx=round(mean(tmax,na.rm=TRUE),1),tmin=round(mean(tmin,na.rm=TRUE),1))->out.month
	datos %>% group_by(year,month) %>% summarise(prcp=round(mean(prcp,na.rm=TRUE),2),tmax=round(mean(tmax,na.rm=TRUE),1),tmin=round(mean(tmin,na.rm=TRUE),1))->out.month

	out.month %>% extract2("year")->omonthYear
	out.month %>% extract2("month")->omonthMonth
	#uguali(serie=out.month$prcp,anni=out.month$year,colonna=4,mesi=out.month$month)->mesi.duplicati.pc
	#uguali(serie=out.month$tmax,anni=out.month$year,colonna=5,mesi=out.month$month)->mesi.duplicati.tx
	#uguali(serie=out.month$tmin,anni=out.month$year,colonna=6,mesi=out.month$month)->mesi.duplicati.tn

	out.month %>% extract2("prcp") %>% uguali(anni=omonthYear,colonna=4,mesi=omonthMonth)->mesi.duplicati.pc
	out.month %>% extract2("tmax") %>% uguali(anni=omonthYear,colonna=5,mesi=omonthMonth)->mesi.duplicati.tx
	out.month %>% extract2("tmin") %>% uguali(anni=omonthYear,colonna=6,mesi=omonthMonth)->mesi.duplicati.tn

	#ciascun mesi.duplicati è un data.frame con la prima colonna con l'anno e la seconda colonna il mese
	#Le prime due righe si riferiscono alla prima coppia di mesi che si ripetono (anno e mese ci dicono dove)
	#Le due righe successive si riferiscono alla seconda serie mensile ripetuta 
	#etc etc

	#Dobbiamo verificare se mesi.duplicati non è NULL

	#Nel caso della precipitazione dobbiamo verificare che le serie mensili uguali non siano due mesi con precipitazione
	#nulla (potrebbe evrificarsi la presenza di due mesi nulli nello stesso anno o in anni distinti)

	#Una volta eliminati i risultati relative alle serie di 0 dobbiamo scrivere i risultati e annullari i valori in datos
	#I flag invece verranno annullati in datos.senza.na (dove invece non si invalidano i valori)

	filena<-paste(fileout,'_pc_serieMensili_uguali.txt',sep='')
	#Per la precipitazione il caso mensile pone un problema:due mesi di 0 potrebbero coesistere nella stessa serie
	#Un mese di zeri (30 valori pari a 0) non viene trovato dai controlli di persistenza (infatti la persostenza
	#degli zeri riguarda periodi molto più lunghi, circa 180 giorni). Per questo è necessario fare una verifica del
	#risultato

	if(!is.null(mesi.duplicati.pc)){
		#verifichiamo che quanto trovato non sia un mese di 0
		indici.finali<-c()
		for(zz in 1:nrow(mesi.duplicati.pc)){

			which(datos$year==mesi.duplicati.pc[zz,1] & datos$month==mesi.duplicati.pc[zz,2])->indice

			stopifnot(length(indice)!=0)
		#è tutta zero la precipitazione corrispondente al risultato?
			which(datos$prcp[indice]!=0)->index0
			#se almeno tre valori sono diversi da 0 nella serie, allora la serie si considera
			#come una serie ripetuta..è un tentativo	
			if(length(index0)>3){indici.finali<-c(indici.finali,zz)}
				
		}#codice for

		#a questo punto possiamo scrivere i risultati	
		if(length(indici.finali)){

			mesi.duplicati.pc[indici.finali,]->mesi.duplicati.pc

			write_csv(as.data.frame(mesi.duplicati.pc),file=filena,col_names=FALSE,append=TRUE)

			##################
			#i risultati dovrebbero venire a coppie di due (a meno di errori nel programma)

			if(nrow(mesi.duplicati.pc)%%2 !=0 ) {stop("numero di anni ripetuti disparo, che fare per invalidare?")}	

			#in datos annulliamo solo la seconda serie mentre la prima la tratteniamo
			#se le due serie mensili si riferiscono allo stesso mese...altrimenti entrambe le serie verranno annullate
			for(hhh in seq(2,nrow(mesi.duplicati.pc),by=2)){

				which(datos$year==mesi.duplicati.pc[hhh,1] & datos$month==mesi.duplicati.pc[hhh,2])->index.year.month
				stopifnot(index.year.month!=0)
				#annullo dati in dato
				datos[index.year.month,]$prcp<<-NA

				#mesi diversi: annulliamo anche la prima serie
				if(mesi.duplicati.pc[hhh,2]!=mesi.duplicati.pc[hhh-1,2]){

					message(sprintf("SERIE MENSILI ENTRAMBE ANNULLATE, vedere file %s",filena))

					which(datos$year==mesi.duplicati.pc[hhh-1,1] & datos$month==mesi.duplicati.pc[hhh-1,2])->index.year.month
					stopifnot(index.year.month!=0)
					#annullo dati in dato
					datos[index.year.month,]$prcp<<-NA
	
				}#if 


			}#fine ciclo for	


			#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
			#ho annullato solo la seconda serie)
			for(hhh in 1:nrow(mesi.duplicati.pc)){
				which(datos.senza.na$year==mesi.duplicati.pc[hhh,1] & datos$month==mesi.duplicati.pc[hhh,2])->index.year.month
				stopifnot(index.year.month!=0)
				#annullo dati in dato
				datos.senza.na[index.year.month,]$serieUguali.prcp<<-FLAG.NON.VALIDO
			}#fine ciclo for			


			##############

		}#if su length indici.finali


	}#
	

	filena<-paste(fileout,'_tx_serieMensili_uguali.txt',sep='')
	if(!is.null(mesi.duplicati.tx)){
		
		write_csv(as.data.frame(mesi.duplicati.tx),file=filena,col_names=FALSE,append=TRUE)
			print(mesi.duplicati.pc)
		if(nrow(mesi.duplicati.tx)%%2 !=0 ) {stop("numero di anni ripetuti disparo, che fare per invalidare?")}	
	
		for(hhh in seq(2,nrow(mesi.duplicati.tx),by=2)){
			#in datos cerchiamo gli anni e mesi che corrispondono a quelli trovati come "serie duplicate"
			which(datos$year==mesi.duplicati.tx[hhh,1] & datos$month==mesi.duplicati.tx[hhh,2])->index.year.month
			stopifnot(index.year.month!=0)
			#annullo dati in dato
			datos[index.year.month,]$tmax<<-NA


				#mesi diversi: annulliamo anche la prima serie
				if(mesi.duplicati.tx[hhh,2]!=mesi.duplicati.tx[hhh-1,2]){

					message(sprintf("SERIE MENSILI ENTRAMBE ANNULLATE, vedere file %s",filena))

					which(datos$year==mesi.duplicati.tx[hhh-1,1] & datos$month==mesi.duplicati.tx[hhh-1,2])->index.year.month
					stopifnot(index.year.month!=0)
					#annullo dati in dato
					datos[index.year.month,]$tmax<<-NA
	
				}#if 

		}#fine ciclo for	


			#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
			#ho annullato solo la seconda serie)
		for(hhh in 1:nrow(mesi.duplicati.tx)){
			which(datos.senza.na$year==mesi.duplicati.tx[hhh,1] & datos$month==mesi.duplicati.tx[hhh,2])->index.year.month
			stopifnot(index.year.month!=0)
			#annullo dati in dato
			datos.senza.na[index.year.month,]$serieUguali.tmax<<-FLAG.NON.VALIDO
		}#fine ciclo for			



	}#if su mesi.duplicati.tx

	filena<-paste(fileout,'_tn_serieMensili_uguali.txt',sep='')
	if(!is.null(mesi.duplicati.tn)){
	
		write_csv(as.data.frame(mesi.duplicati.tn),file=filena,col_names=FALSE,append=TRUE)
			print(mesi.duplicati.pc)
		if(nrow(mesi.duplicati.tn)%%2 !=0 ) {stop("numero di anni ripetuti disparo, che fare per invalidare?")}	

		for(hhh in seq(2,nrow(mesi.duplicati.tn),by=2)){
			#in datos cerchiamo gli anni e mesi che corrispondono a quelli trovati come "serie duplicate"
			which(datos$year==mesi.duplicati.tn[hhh,1] & datos$month==mesi.duplicati.tn[hhh,2])->index.year.month
			stopifnot(index.year.month!=0)
			#annullo dati in dato
			datos[index.year.month,]$tmin<<-NA


				#mesi diversi: annulliamo anche la prima serie
				if(mesi.duplicati.tn[hhh,2]!=mesi.duplicati.tn[hhh-1,2]){

					message(sprintf("SERIE MENSILI ENTRAMBE ANNULLATE, vedere file %s",filena))

					which(datos$year==mesi.duplicati.tn[hhh-1,1] & datos$month==mesi.duplicati.tn[hhh-1,2])->index.year.month
					stopifnot(index.year.month!=0)
					#annullo dati in dato
					datos[index.year.month,]$tmin<<-NA
	
				}#if 

		}#fine ciclo for	


			#ora invalido i flag: il flag lo assegno sia alla prima che alla seconda serie ripetuta (ma in datos
			#ho annullato solo la seconda serie)
		for(hhh in 1:nrow(mesi.duplicati.tn)){
			which(datos.senza.na$year==mesi.duplicati.tn[hhh,1] & datos$month==mesi.duplicati.tn[hhh,2])->index.year.month
			stopifnot(index.year.month!=0)
			#annullo dati in dato
			datos.senza.na[index.year.month,]$serieUguali.tmin<<-FLAG.NON.VALIDO
		}#fine ciclo for



	}#mesi.duplicati.tn	



}#fine serie_uguali


####################################################################################################################
####################################################################################################################
impossible_values<-function(fileout) {

	messaggi(testo="IMPOSSIBLE VALUES: ricerca di valori fisicamente impossibili")

	#colonne per i flag
	datos.senza.na$imp.prcp<<-FLAG.VALIDO
	datos.senza.na$imp.tmax<<-FLAG.VALIDO
	datos.senza.na$imp.tmin<<-FLAG.VALIDO
	

	if(!all(is.na(datos$tmax))){
		which(datos$tmax > 50 | datos$tmax < -30)->index.tmax
		if(length(index.tmax)){
			write_csv(datos[index.tmax,],file=paste0(fileout,"_tmax.txt"),col_names=FALSE,append=TRUE)
			datos.senza.na[index.tmax,]$imp.tmax<<-FLAG.NON.VALIDO
			datos[index.tmax,]$tmax<<-NA
		}
	}


	if(!all(is.na(datos$tmin))){
		which(datos$tmin > 40 | datos$tmin < -40)->index.tmin
		if(length(index.tmin)){
			write_csv(datos[index.tmin,],file=paste0(fileout,"_tmin.txt"),col_names=FALSE,append=TRUE)
			datos.senza.na[index.tmin,]$imp.tmin<<-FLAG.NON.VALIDO
			datos[index.tmin,]$tmin<<-NA
		}
	}


	if(!all(is.na(datos$prcp))){
		which(datos$prcp >= 800 | datos$prcp < 0)->index.prcp
		if(length(index.prcp)){
			write_csv(datos[index.prcp,],file=paste0(fileout,"_prcp.txt"),col_names=FALSE,append=TRUE)
			datos.senza.na[index.prcp,]$imp.prcp<<-FLAG.NON.VALIDO
			datos[index.prcp,]$prcp<<-NA
		}
	}



}#fine impossible_values



####################################################################################################################
####################################################################################################################
######################### jumps_revisited

jumps_tx_tn<-function(fileout,JUMP=18) {

	messaggi(testo="INTERNAL CONSISTENCY: ricerca dei salti nella serie")

	#colonne per i flag
	datos.senza.na$jump.tmax<<-FLAG.VALIDO
	datos.senza.na$jump.tmin<<-FLAG.VALIDO


	if(all(is.na(datos$tmax)) && all(is.na(datos$tmin))) return()

	contatoreJumps<-1
	trova_jump<-function(serie,parametro){

		differenza <-abs(diff(serie))
		which(differenza>=JUMP)->index.jump
		if(!length(index.jump)) return()
		diff(index.jump)->index.jump.jump
		which(index.jump.jump==1)->index.finale
		if(!length(index.finale)) return()
    		SCRITTO<-TRUE
		for(ii in 1:length(index.finale)){
		  if(ii>1) if((index.finale[ii]-1)==(index.finale[ii-1]) & SCRITTO){ SCRITTO<-FALSE; next} #gestisce i casi di gap contigui
     		  SCRITTO<-TRUE
		  index.jump[index.finale[ii]]->uno
			uno+1->due
			due+1->tre

			########################
			#qui grafico per jumps

				  (index.jump[index.finale[ii]]+1)->indice
				  etichetta<-"°C"
					  
				  if(indice<=15){
				    seq(1,indice+15)->sequenza
				  }else if((nrow(datos)-indice)<=15){
				    seq(indice-15,nrow(datos))->sequenza
				  }else{
				    seq(indice-15,indice+15)->sequenza
				  }
					    
				  #estraiamo i dati per definire il range dei valori su asse y
				  datos %>% slice(sequenza) ->temporaneo
				  temporaneo %>% extract2(parametro)->valori
				  which(sequenza %in% indice)->posizione
			          paste(temporaneo %>% select(1,2,3) %>% slice(1),collapse="-")->mytime
				  paste0(fileout,"_",parametro,'_jumps.')->nome.grafico

				  pdf(paste0(nome.grafico,contatoreJumps,".pdf"))
				  	plot(c(valori),ylab=etichetta,main=paste0("Jumps >=",JUMP,"°C ",mytime),type="l")
					points(posizione,valori[posizione],pch=21,bg="red")
				  dev.off()
				  contatoreJumps<<-contatoreJumps+1
					  			  

			#fine grafico per jumps
			########################

			if(parametro=="tmax"){
				filena<-paste(fileout,'_tmax_jumps.txt',sep='')
				write_csv(datos[c(uno,due,tre),],file=filena,col_names=FALSE,append=TRUE)
				datos.senza.na[due,]$jump.tmax<<-FLAG.DUBBIO
				datos[due,]$tmax<<-NA		
			}else if(parametro=="tmin"){
				filena<-paste(fileout,'_tmin_jumps.txt',sep='')
				write_csv(datos[c(uno,due,tre),],file=filena,col_names=FALSE,append=TRUE)
				datos.senza.na[due,]$jump.tmin<<-FLAG.DUBBIO
				datos[due,]$tmin<<-NA			
			}else{
			
				stop("ERRORE PARAMETRO in jumps")
				
			}


		}#fine ciclo for


	}#fine trova_jump

	trova_jump(serie=datos$tmax,parametro="tmax")	
	trova_jump(serie=datos$tmin,parametro="tmin")

}#fine jump_tx_tn








