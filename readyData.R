library(tidyverse)
library(plumber)
library(future)

options(future.globals.maxSize= 891289600)

joint_probs <- readRDS("data/joint/joint_probs.rds")
joint_cvd_pop <- read_csv("data/pops/simPop2.cvd.cvd.500.14nov20.csv")
joint_mi_pop <- read_csv("data/pops/simPop2.mi.mi.500.12nov20.csv")

cvd = readRDS(file = 'data/cond/cond_prob_cvd.rds')
mi = readRDS(file = 'data/cond/cond_prob_mi.rds')

cond_probs <- list(
  cvd = cvd,
  mi = mi
)

weighted_risk_cvd <- read_csv("data/joint/weighted_risk_cvd.csv")
weighted_risk_mi <- read_csv("data/joint/weighted_risk_mi.csv")

# weighted_risk_cvd %>% filter( year_cat = "(1989,1994]") %>% arrange(event_1,event_2,event_3,event_4 ) %>% View

initials <- function  ( terms ){

  paste0(unlist(lapply ( terms, function (x){
    substring(x, 1, 1)
  })), collapse = "")

}

filterWeightedMSMData <- function( source, year_cat_in ){

  if ( source == "mi" ){
    source_data = weighted_risk_cvd
  } else {
    source_data = weighted_risk_mi
  }
  
  
  source_data <- source_data %>% filter(year_cat == year_cat_in) %>% 
      mutate( wr = round(wr*100,3) ) %>% 
      mutate( probability = wr ) %>% 
      mutate( event_0  = source ) %>% 
      mutate( event_id = "fourth_evnt") %>%
      mutate( event_id = ifelse(is.na(event_4), "thrd_evnt",event_id)) %>%
      mutate( event_id = ifelse(is.na(event_3), "scnd_evnt",event_id)) %>% 
      mutate( event_id = ifelse(is.na(event_2), "frst_evnt",event_id))
  
    source_data <- source_data %>% select( colnames(source_data) %>% sort() )
    
  
    source_data <- source_data %>% rowwise %>% mutate( eventSequence = paste0( c(event_0, event_1, event_2, event_3, event_4), collapse = ",")) %>% mutate ( eventSequence = str_replace_all(eventSequence,",NA", "")) 
  
    first_ev <- source_data %>% filter( event_id == "frst_evnt") %>% mutate( source = event_0, target=paste0("(",initials(c(event_0)),") ",event_1))
    second_ev <- source_data %>% filter( event_id == "scnd_evnt") %>% mutate( source = paste0("(",initials(c(event_0)),") ",event_1), target= paste0("(",initials(c(event_0,event_1)),") ",event_2))
    third_ev  <- source_data %>% filter( event_id == "thrd_evnt") %>% mutate( source = paste0("(",initials(c(event_0,event_1)),") ",event_2), target=paste0("(",initials(c(event_0,event_1,event_2)),") ",event_3))
    fourth_ev <- source_data %>% filter( event_id == "fourth_evnt") %>% mutate( source = paste0("(",initials(c(event_0,event_1,event_2)),") ",event_3), target=paste0("(",initials(c(event_0,event_1,event_2,event_3)),") ",event_4)) #paste0("(4) ",event_4))
    
  all_transitions <- first_ev %>% rbind(second_ev) %>% 
    rbind(third_ev) %>% rbind(fourth_ev) 
  
  filtered <- source_data %>% 
    select(eventSequence,event_0:event_4,wr) %>% 
    arrange(desc(event_1),desc(event_2),desc(event_3),desc(event_4)) %>% 
    rowwise %>% mutate( ss = str_split(eventSequence, ",") )
  
  root <- unlist(lapply(filtered$ss, function(x){ 
      ss <- x[!is.na(x)]
      unlist(paste0(x[1:(length(x)-1)], collapse = ",")) 
    }))
  
  values <- unlist(lapply(root, function(x) {
    sum((filtered %>% filter( str_detect(eventSequence,x) ))$wr)
  }))
  
  transitions <- (tibble ( eventSequence = root, eventSequence2 = root, values = values) %>% distinct) %>% separate(eventSequence2, c( "event_0", "event_1", "event_2", "event_3",  "event_4"), ",")
  
  transitions <- transitions %>% mutate( year_cat = year_cat_in, wr = values, probability = values, event_id="aggr")
  
  transitions <- transitions %>% mutate( event_id = "fourth_evnt") %>%
    mutate( event_id = ifelse(is.na(event_4), "thrd_evnt",event_id)) %>%
    mutate( event_id = ifelse(is.na(event_3), "scnd_evnt",event_id)) %>% 
    mutate( event_id = ifelse(is.na(event_2), "frst_evnt",event_id)) %>% rowwise()
  
  first_ev <- transitions %>% filter( event_id == "frst_evnt") %>% mutate( source = event_0, target=paste0("(",initials(c(event_0)),") ",event_1)) %>% ungroup()
  second_ev <- transitions %>% filter( event_id == "scnd_evnt") %>% mutate( source = paste0("(",initials(c(event_0)),") ",event_1), target= paste0("(",initials(c(event_0,event_1)),") ",event_2)) %>% ungroup()
  third_ev  <- transitions %>% filter( event_id == "thrd_evnt") %>% mutate( source = paste0("(",initials(c(event_0,event_1)),") ",event_2), target=paste0("(",initials(c(event_0,event_1,event_2)),") ",event_3)) %>% ungroup()
  fourth_ev <- transitions %>% filter( event_id == "fourth_evnt") %>% mutate( source = paste0("(",initials(c(event_0,event_1,event_2)),") ",event_3), target=paste0("(",initials(c(event_0,event_1,event_2,event_3)),") ",event_4)) %>% ungroup()
  
  aggr_transitions <- first_ev %>% rbind(second_ev) %>% 
    rbind(third_ev) %>% rbind(fourth_ev)  

  aggr_transitions <- aggr_transitions %>% select(all_transitions %>% colnames)

  all_transitions <- all_transitions %>% rbind (aggr_transitions) %>% ungroup
  
  all_transitions <- all_transitions %>% 
    arrange(desc(event_0),desc(event_1),desc(event_2),desc(event_3),desc(event_4)) %>% 
    mutate( target = ifelse(is.na(event_1), source , target), source = ifelse(is.na(event_1), NA , source), event_id = ifelse(is.na(event_1), "root_evnt" , event_id)  ) %>% 
    distinct 
}

filterMSMData <- function( source, sex_in, simd, hf_in, cerebro_or_mi, year_cat_in, age_in ){
  
  if ( source == "mi" ){
    source_data = cond_probs$mi
    source_pop = joint_mi_pop
    ids <- (source_pop %>% filter( sex == sex_in ) %>% 
              filter(simd_2009 == simd) %>% 
              filter(cerebrovasc == cerebro_or_mi) %>% 
              filter(hf == hf_in) %>% 
              filter(year_cat == year_cat_in))
  } else {
    source_data = cond_probs$cvd
    source_pop = joint_cvd_pop
    ids <- (source_pop %>% filter( sex == sex_in ) %>% 
              filter(simd_2009 == simd) %>% 
              filter(mi == cerebro_or_mi) %>% 
              filter(hf == hf_in) %>% 
              filter(year_cat == year_cat_in))
  }

  sel_id <- (ids %>% mutate( dff = abs(age-age_in) ) %>% arrange(dff))$id[1]

  pdata <- source_data %>% ungroup %>%
    filter(pid == sel_id) %>% mutate( event_0 = source)  #%>% filter(event_1 == "bleeding")
  
  pdata <- pdata %>% mutate( event_1  = as.character(event_1)) %>% 
    mutate( event_2  = as.character(event_2)) %>% 
    mutate( event_3  = as.character(event_3)) %>% 
    mutate( event_4  = as.character(event_4))
  
  pdata <- pdata %>% mutate( event_1 = as.character(ifelse( is.na(event_1),"",event_1 ) )) %>% 
    mutate( event_2 = as.character(ifelse( is.na(event_2),"",event_2 ) )) %>% 
    mutate( event_3 = as.character(ifelse( is.na(event_3),"",event_3 ) )) %>% 
    mutate( event_4 = as.character(ifelse( is.na(event_4),"",event_4 ) ))
  
  pdata <- pdata %>% mutate( path = paste0(event_0,"_",event_1,"_",event_2,"_",event_3,"_",event_4))
  
  pdata <- pdata %>% mutate( path = str_replace(path,"_*$", "") )
 
  first_ev <- pdata %>% filter( event_id == "frst_evnt") %>% mutate( source = event_0, target=paste0(event_0,"_",event_1))
  second_ev <- pdata %>% filter( event_id == "scnd_evnt") %>% mutate( source = paste0(event_0,"_",event_1), target=paste0(event_0,"_",event_1,"_",event_2))
  third_ev  <- pdata %>% filter( event_id == "thrd_evnt") %>% mutate( source = paste0(event_0,"_",event_1,"_",event_2), target=paste0(event_0,"_",event_1,"_",event_2,"_",event_3))
  fourth_ev <- pdata %>% filter( event_id == "fourth_evnt") %>% mutate( source = paste0(event_0,"_",event_1,"_",event_2,"_",event_3), target=paste0(event_0,"_",event_1,"_",event_2,"_",event_3,"_",event_4))

  all_transitions <- first_ev %>% rbind(second_ev) %>% rbind(third_ev) %>% rbind(fourth_ev) %>% mutate( source = str_replace(source,"_*$", ""),  target = str_replace(target,"_*$", "") ) 
  all_transitions <- all_transitions %>% filter ( cond_prob != 1)
  
  labels <- c(all_transitions$source, all_transitions$target) %>% unique()
  all_transitions_n <- all_transitions %>% rowwise() %>% mutate( source_n = which(labels == source)[1], target_n = which(labels == target)[1]  ) 
 
  labels <- tibble(target=labels) %>% left_join(all_transitions_n %>% select(target,cond_prob)) %>% 
              mutate( ss = str_split(target,"_") ) %>% rowwise %>% mutate( labels = paste0(ss[length(ss)], ifelse(is.na(cond_prob) , "" ,paste0(" (", round(cond_prob*100,2),"%)"))) )
  
  
  all_transitions_n <- all_transitions_n %>% select( all_transitions_n %>% colnames() %>% sort() )
  
  data <- list( 
    data = all_transitions_n, 
    labels = labels$labels
    )
  # browser()
  return(data)  
}

# filterMSMData("mi", 1, 3, 1, 0, "(1989,1994]", 50)  $data %>% View


