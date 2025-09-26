# Load required packages
pacman::p_load(tidyverse, raster, sf, mgcv, foreach, doParallel, pryr, modEvA)

# Sample years for model fitting
yrs_fit <- data.frame(year = c(1980, 2000, 1910, 1995, 1935, 1970, 1965, 1955, 1930, 2015, 1985, 1920, 1975, 2020, 1990, 1940, 1900, 1905))

# Get list of processed structure files
type_str <- list.files('Data/Processed',
                       recursive = F,
                       pattern = '_ts',
                       full.names = F)
type_str <- type_str[!grepl('densification', type_str)][6]

fit_gam <- function(type_str){
  dfs <- list.files( paste0('Data/Processed/', type_str),
                     recursive = TRUE,
                     full.names = TRUE)
  if(!dir.exists(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3))))){
    dir.create(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3))))
  }
  if(!dir.exists(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model'))){
    dir.create(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model'))
  }
  if(!dir.exists(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model/predictions'))){
    dir.create(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model/predictions'))
  }
  if(!dir.exists(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model/summaries'))){
    dir.create(paste0('Data/Processed/', substr(type_str, 1, (nchar(type_str)-3)), '/Model/summaries'))
  }
  return(dfs)
}

fit_summary <- function(dfs, yrs_fit) {
  type_str <- gsub('_ts', '', unique(str_split_fixed(dfs, '/', 5)[1,3]))
  df <- read.csv(dfs) 
  
  # Extract eco-region identifiers
  eco_l4_id <- df$eco_l4_id[1]
  eco_l1 <- df$eco_l1[1]
  
  # Filter for private land only
  df <- df %>% 
    filter(dlu %in% 'Private')
  
  if (nrow(df) < 1) {
    
    # Save a marker file for missing private land data
    write.csv('No private land', paste0('Data/Processed/', type_str, 
                                        '/Model/summaries/', eco_l1, '_', eco_l4_id, '_no_private_land.csv'), row.names = FALSE)
  } else {
    if (any(df$year == 2020 & df$structures == 0)) {
      # Predict 0 structures for all years
      df_prediction <- data.frame(year = 1900:2060, fit_poiss = 0, se_poiss = 0, fit_nb = 0, se_nb = 0)
      df_validation <- anti_join(df, yrs_fit)
      df_model <- inner_join(df, yrs_fit)
      pred_df <- full_join(df_prediction, df_model) %>%
        full_join(df_validation, by = 'year')
      pred_df <- pred_df[,c(-6, -10, -12, -13)]
      
      names(pred_df)[c(6, 9)] <- c('struct_model', 'struct_valid')
      # Save outputs indicating zero structures
      file_suffix <- if (type_str == 'structures') 3 else 2
      write.csv(pred_df, paste0('Data/Processed/', type_str, '/Model/predictions/',  
                                      str_split_fixed(basename(dfs), '_', file_suffix)[1, file_suffix]), row.names = FALSE)
      }else{ 
        df_prediction <- data.frame(year = 1900:2060)
        df_model <- inner_join(df, yrs_fit)
        nb_int <- gam(structures ~ s(year, bs = 'cr', k = 12), family = nb(link = log), data = df_model, method = "REML")
        nb_int_pred <- predict(nb_int, se.fit = TRUE, newdata = df_prediction, type = 'response')
        nb_int_pred_df <- data.frame(year = df_prediction$year, fit_nb = nb_int_pred$fit, se_nb = nb_int_pred$se.fit)
        df_validation <- anti_join(df, yrs_fit) %>% left_join(nb_int_pred_df)
        deviance_explained <- Dsquared(obs = df_validation$structures, pred = df_validation$fit, family = 'gaussian', dismo.version=TRUE)
        nb_int_summary <- summary(nb_int)$s.table %>%
          data.frame() %>%
          mutate(aic_nb = nb_int$aic,
             bic_nb = BIC(nb_int),
             dev_expl_model_nb = summary(nb_int)$dev.expl,
             dev_expl_valid_nb = deviance_explained,
             cor_val_nb = cor(df_validation$structures, df_validation$fit))
        
        poiss_int <- gam(structures ~ s(year, bs = 'cr', k = 12), family = poisson, data = df_model)
        poiss_int_pred <- predict(poiss_int, se.fit = TRUE, newdata = df_prediction, type = 'response')
        poiss_int_pred_df <- data.frame(year = df_prediction$year, fit_poiss = poiss_int_pred$fit, se_poiss = poiss_int_pred$se.fit)
        df_validation_poiss <- anti_join(df, yrs_fit) %>% left_join(poiss_int_pred_df)
        deviance_explained_poiss <- Dsquared(obs = df_validation_poiss$structures, pred = df_validation_poiss$fit, 
                                         family = 'poisson', dismo.version=TRUE)
        poiss_summary <- summary(poiss_int)$s.table %>%
          data.frame() %>%
          mutate(aic_poiss = poiss_int$aic,
             bic_poiss = BIC(poiss_int),
             dev_expl_model_poiss = summary(poiss_int)$dev.expl,
             dev_expl_valid_poiss = deviance_explained_poiss,
             cor_valid_poiss = cor(df_validation_poiss$structures, df_validation_poiss$fit))
        summary_df <- cbind(nb_int_summary, poiss_summary)
        pred_df <- full_join(poiss_int_pred_df, nb_int_pred_df) %>%
          full_join(df_model) %>%
          full_join(df_validation, by = 'year')
        names(pred_df)[c(7, 11)] <- c('struct_model', 'struct_valid')
    
    # Save outputs
    file_suffix <- if (type_str == 'structures') 3 else 2
    write.csv(pred_df[,c(1:5, 7:9, 11)], paste0('Data/Processed/', type_str, '/Model/predictions/',  
                                                str_split_fixed(basename(dfs), '_', file_suffix)[1, file_suffix]), row.names = FALSE)
    
    write.csv(summary_df, paste0('Data/Processed/', type_str, '/Model/summaries/',  
                                 str_split_fixed(basename(dfs), '_', file_suffix)[1, file_suffix]), row.names = FALSE)
      }
  }
}


   
    
# Run the model fitting and summary function
df <- foreach(i = 1:length(type_str), .combine = rbind) %do% {
  fit_gam(type_str = type_str[[i]]) %>% as.data.frame()
}

foreach(i = 1:nrow(df)) %do% {
  fit_summary(dfs = df[i, 1], yrs_fit)
}


# Check missing files
a <- list.files('Data/Processed/structures/Model/summaries') %>% as.data.frame()
b <- list.files('Data/Processed/structures_ts') %>% as.data.frame()

