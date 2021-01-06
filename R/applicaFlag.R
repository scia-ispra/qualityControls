rm(list=objects())
library("tidyverse")
library("purrr")
library("lazyeval")
library("stringr")
options(error=recover)

dir.create("data_con_flag")

invalidaFlag<-function(param){
  
  paste0("corroboration.",param)->corName
  paste0("regression.",param)->regName
  
  function(x){
    
    mutate(x,.dots=setNames(list(interp(~(ifelse(y!=0 | z!=0,NA,x)),
                                         .values=list(x=as.name(param),y=as.name(corName),z=as.name(regName)))),param))
  }
  
}#fine invalida flag

invalidaTmax<-invalidaFlag(param="tmax")
invalidaTmin<-invalidaFlag(param="tmin")

applicaFlag<-function(nomeFile)
{
  
  tryCatch({
    suppressWarnings(read_delim(nomeFile,delim=",",col_names=TRUE,col_types ="iiiddddddd"))
  },error=function(e){
    stop(sprintf("Errore lettura file %s",nomeFile))
  })->dati
  

  dati %>% 
    invalidaTmax %>%
      invalidaTmin %>%
        select(-dplyr::contains(".tmax"),-dplyr::contains(".tmin")) %>%
          write_delim(.,path=str_replace(nomeFile,"^n",""),delim=",",col_names=TRUE)
  
  system(sprintf("mv %s ./data_con_flag",nomeFile))
  
    
}#applicaFlag  

list.files(pattern="^n.+txt")->ffile
stopifnot(length(ffile)!=0)
map(ffile,applicaFlag)