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


################# funzione per la ricerca di blocchi di valori ripetuti
prova<-c(3,2,NA,4,1,2,NA,2,NA,2,2,2,1,3,rep(NA,6),3,3)
temp<-c(3,2,4,1,2,2,2,2,2,1,3,3,3,0,0,1,0,2,2,2,3,3,3,3,1,2,2)
temp2<-seq(1,100)
temp3<-c(1,2,1,1,1,1,1,1,1,2,2,1,1,1,2,2,1,1,1,1,1)
temp4<-c(3,2,NA,4,1,2,NA,2,NA,2,2,2,1,3,rep(NA,0),3,3,0,0,0,0,0,0,NA,0,0,1,NA,0,0,0,1,0,0,2,3,0,0,0,0,1,0,0,1,1,2,3,0,1,2,3,0,0,1,1,1,2,0,1,1,0,NA,NA,0,0,0,NA)

#x vettore con i dati
#value: valore di cui si cerca la presenza in blocchi
#n: dimensione minima dei blocchi

in.blocco<-function(x,nmax=3){
  
  INVQUI<-732 #due anni
  
  ##nmax<-20 #almeno 20 valori ripetuti
  invalidaQui<-numeric(length=INVQUI) #indici degli elementi (non NA) da invalidare
  
  #if(!is.numeric(x)) stop("aspetto dati numerici")
  length(x)->len
  if(!len) return() #nessun dato da analizzare...

  which(is.na(x))->index.na
  length(index.na)->len.na
  
  if(len.na)
    x[-index.na]->x
  
  length(x)->len
  if(!len){
    return() #nessun dato da analizzare...tutta la serie non valida
  } 

  #calcoliamo la differenza prima e cerchiamo gli zeri  
  diff(x)->diffPrima
  which(diffPrima==0)->index.zero

  length(index.zero)->len.zero
  if(!len.zero) 
    return() #non ci sono valori ripetuti: possibile?

  #index value contiene la posizione dove si trovano i valori "value" all'interno di x
  #
  #calcolo la differenza prima: diff(index.value): laddove ottengo valore uguale a 1
  #significa che sono in presenza di valori contigui

  diff(index.zero)->diff.zero
#  length(diff.zero)->len.zero
  
  which(diff.zero==1)->index.uno 
  #se ho valore uno, significa che ho valori ripetuti adiacenti

  length(index.uno)->len.uno
  if(!len.uno) 
    return() #nessun valore adiacente

  conta<-0
  pos1<-1
  gg<-1
  valore1<-NA
  valore2<-NA
  
  while(1){

    valore1<-index.zero[gg]
    hh<-gg+1

    if(hh<=len.zero){
  
    valore2<-index.zero[hh]
    valore2-valore1->step

    if(step==1){

      if(!conta)
        index.gg<-valore1
        
        #se è la prima volta che trovo due valori consecutivi
        #mi tengo gg
      
      conta<-conta+1

    }#valore2-valore1==1
      
} #h > len.zero
    
    if((step!=1) || (hh>len.zero)){   

      if((conta+2)>= nmax){ 
        #devo sommare due perchè sto considerando un vettore che è due volte la differenza
        #tra gli elementi del vettore di partenza
        
        index.hh<-index.zero[hh-1]
        seq(index.gg,index.hh)->indici
        length(indici)->len.indici
        #grow object  
        invalidaQui[pos1:(pos1+len.indici-1)]<-indici
        pos1<-(pos1+len.indici)
        invalidaQui[pos1]<-invalidaQui[pos1-1]+1
        pos1<-pos1+1
        
        if(pos1>round(INVQUI/1.5)){#grow object
          invalidaQui<-c(invalidaQui,numeric(length(INVQUI)))          
        }#grow object
    
        #se conta vale 3, allora ho trovato 4 (conta+1) valori "value"
        #consecutivi: questo significa che ho trovato un blocco  
        
      }
      
      if(hh>len.zero) break
      conta<-0 #ora resettiamo conta a 0
    }#if valore2!=valore1

    gg<-gg+1
    
  }#ciclo gg su while

  if(pos1==1) return()
  
  invalidaQui[1:(pos1-1)]->invalidaQui

  for(j in 1:len.na){
    which(invalidaQui>=index.na[j])->index
 
    if(!length(index)) break
    invalidaQui[index]<-invalidaQui[index]+1
  }#ciclo su j

  invalidaQui
  
}#fine in.blocco 

########################à
#questa è la versione che gira più lentamente, perchè scritta come ciclo for

in.blocco3<-function(x,nmax=3){
  
  INVQUI<-732 #due anni
  
  length(x)->len
  if(!length(x))
    return()
  
  which(is.na(x))->index.na
  length(index.na)->len.na
  if(len.na)
    x[-index.na]->x
  length(x)->len #importante risettare la lunghezza
  invalidaQui<-numeric(length=(INVQUI)) #indici degli elementi (non NA) da invalidare
  
  pos1<-1
  conta<-0
  j<-2

  
  while(1){

    if(j>len){
      if(conta>=nmax){
        estremo2<- len
        step<-(estremo2-estremo1+1)
        #grow object
        invalidaQui[pos1:(pos1+step-1)]<-seq(estremo1,estremo2)
        pos1<-pos1+step
      }  
      break      #interrompi ciclo while
    }#if j> len
    
    valore1<-x[j]
    valore2<-x[j-1]
  
    if(valore1==valore2){
      
        if(!conta){
          conta<-conta+2
          estremo1<- j-1
        }else{
          conta<-conta+1        
        }

    }else{#valore2==valore1
    
        if(conta>=nmax){
          estremo2<- j-1
          if(j==len)
            estremo2<-j
        
          step<-(estremo2-estremo1+1)
          #grow object
          invalidaQui[pos1:(pos1+step-1)]<-seq(estremo1,estremo2)
          pos1<-pos1+step
          if(pos1>round(INVQUI/1.5)){#grow object
            invalidaQui<-c(invalidaQui,numeric(length(INVQUI)))          
          }#grow object
        }
      
        conta<-0        

    }#if su valore2==valore1  
    
      j<-j+1    
    
    }#fine ciclo while
  
    invalidaQui[1:(pos1-1)]->invalidaQui
  
    if(length(index.na)){#index.na
      for(j in 1:len.na){
      
        which(invalidaQui>=index.na[j])->index
      
        if(!length(index)) break
        invalidaQui[index]<-invalidaQui[index]+1
      }#ciclo su j
    }#if su index.na
  
    invalidaQui
  
}#fine in.blocco3

###################################
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


#################long persistence per la precipitazione:
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
