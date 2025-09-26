pacman::p_load(tidyverse, foreach, doParallel, modEvA, sf)

#Validaton

#Calculate deviance
t1 <- read.csv('Data/Processed/structures/Model/structures_complete.csv') 
t1 <- filter(t1, year>1939)
train <- t1 %>% 
  filter(!is.na(struct_model)) %>% 
  group_by(region, id) %>% 
  summarize(dev_train_pois = Dsquared(obs = struct_model, pred = model_out, family = 'poisson',  dismo.version=TRUE, adjust = TRUE, npar = 1),
            dev_train_g = Dsquared(obs = struct_model, pred = model_out,  dismo.version=TRUE, adjust = TRUE, npar = 1))
valid <- t1 %>% 
  filter(!is.na(struct_valid)) %>% 
  group_by(region, id) %>% 
  summarize(dev_valid_pois = Dsquared(obs = struct_valid, pred = model_out, family = 'poisson',  dismo.version=TRUE, adjust = TRUE, npar = 1),
            dev_valid_g = Dsquared(obs = struct_valid, pred = model_out,  dismo.version=TRUE, adjust = TRUE, npar = 1))

combo <- full_join(train, valid)

#Get model used
get_mod <- function(model_sum){
  if(grepl('no_private_land', model_sum)){
    return()
  }else{
    ms <- read.csv(model_sum)
    a <- data.frame(id = gsub('.csv', '', basename(model_sum)),
                    model = ifelse(ms$aic_nb<ms$aic_poiss, 'Negative binomial', 'Poisson'))
    return(a)
  }
}

files <- list.files('Data/Processed/structures/Model/summaries',
                    full.names = T)
a <- foreach(i=1:length(files), .combine = rbind) %do%
  get_mod(files[[i]])

combo <- left_join(combo, a)    

write.csv(combo, 'Data/Processed/structures/Model/deviance_validation.csv', row.names = F)

      
             
