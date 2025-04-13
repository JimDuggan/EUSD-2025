library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(crayon)

get_data <- function(fname,ens=1){
  read_csv(fname) %>%
    pivot_longer(-Days,names_to="RunVar",values_to = "Value") %>%
    mutate(Ensemble=ens) %>%
    mutate(RunVar=str_remove_all(RunVar,"\"")) %>%
    mutate(RunVar=str_remove_all(RunVar,"=")) %>%
    separate_wider_delim(RunVar,":",names=c("Run","Variable")) %>%
    mutate(Variable=str_trim(Variable)) %>%
    mutate(Variable=str_remove(Variable,"Regular Care.")) %>%
    mutate(Run=as.integer(str_extract(Run,"\\d+"))) %>%
    mutate(RunID=Run+max(Run)*(ens-1)) %>%
    select(Ensemble,Days,RunID,Run,everything()) 
}

# f <- get_data("data/01_SENS_Data_CSV.csv")

process_all_data <- function(save=FALSE){
  f_list <- paste0("data/",list.files(path="data/",
                       pattern="*.csv"))
  
  sim_d <- map2(1:length(f_list),f_list,~{
    cat(red("Processing ",.y,"\n"))
    get_data(.y,ens=.x)
  })
  
  sim_l <- dplyr::bind_rows(sim_d)
  if(save == TRUE)
    saveRDS(sim_l,"data/SimSave.RDS")
  sim_l
}

sim_l <- process_all_data(save=TRUE)


sim_w <- sim_l %>%
  pivot_wider(names_from = Variable,
              values_from = Value) %>%
  mutate(`Activate B1 Overtime`=as.logical(`Activate B1 Overtime`),
         `Activate R1 Burnout`=as.logical(`Activate R1 Burnout`))

ggplot(sim_w,aes(x=Days,y=`Regular Care Backlog`,group=RunID,colour=RunID))+geom_line()


