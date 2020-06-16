# function calculates all relevant models
run_models <- function(y, pers, data, model){
  
  # subset data
  data = data %>% 
    dplyr::rename(y = all_of(y),
                  pers = all_of(pers))
  
  if(model == 'lm'){
    
    # predict slopes from personality
    lm_base <- lm(y ~ pers, 
                  data = data)
    
    # predict slopes from personality with controls
    lm_soc <- lm(y ~ pers + 
                   age + male + conservative,
                 data = data)
    
    # predict slopes from personality with controls
    lm_econ <- lm(y ~ pers + 
                    academics + medinc + manufact,
                  data = data)
    
    # predict slopes from personality with controls
    lm_pan <- lm(y ~ pers + 
                   airport_dist + tourism + healthcare + popdens,
                 data = data)
    
    if(!str_detect(y, 'socdist')){
      
      # predict slopes from personality with controls
      lm_all <- lm(y ~ pers + 
                     age + male + conservative +
                     academics + medinc + manufact +
                     airport_dist + tourism + healthcare + popdens,
                   data = data)
    }
    
    if(str_detect(y, 'socdist')){
      
      # predict slopes from personality with controls
      lm_all <- lm(y ~ pers + 
                     age + male + conservative +
                     academics + medinc + manufact +
                     airport_dist + tourism + healthcare + popdens + onset_prev + slope_prev,
                   data = data)
    }
    
    # create list with results
    results <- list('lm_base' = lm_base, 
                    "lm_soc" = lm_soc,
                    "lm_econ" = lm_econ,
                    "lm_pan" = lm_pan,
                    "lm_all" = lm_all)
    
  }else if (model == 'cox'){
    
    # predict slopes from personality
    cox_base <- coxph(Surv(y, event) ~ pers, 
                      data = data)
    
    # predict slopes from personality with controls
    cox_soc <- coxph(Surv(y, event) ~ pers + 
                       age + male + conservative,
                     data = data)
    
    # predict slopes from personality with controls
    cox_econ <- coxph(Surv(y, event) ~ pers + 
                        academics + medinc + manufact,
                      data = data)
    
    # predict slopes from personality with controls
    cox_pan <- coxph(Surv(y, event) ~ pers + 
                       airport_dist + tourism + healthcare + popdens,
                     data = data)
    
    if(!str_detect(y, 'socdist')){
      
      # predict slopes from personality with controls
      cox_all <- coxph(Surv(y, event) ~ pers + 
                         age + male + conservative +
                         academics + medinc + manufact +
                         airport_dist + tourism + healthcare + popdens,
                       data = data)
    }
    
    if(str_detect(y, 'socdist')){
      
      # predict slopes from personality with controls
      cox_all <- coxph(Surv(y, event) ~ pers + 
                         age + male + conservative +
                         academics + medinc + manufact +
                         airport_dist + tourism + healthcare + popdens + onset_prev + slope_prev,
                       data = data)
    }
    
    # create list with results
    results <- list('cox_base' = cox_base, 
                    "cox_soc" = cox_soc,
                    "cox_econ" = cox_econ,
                    "cox_pan" = cox_pan,
                    "cox_all" = cox_all)
  }
}


# functions fits models for all traits
run_analyses <- function(y, data, model){
  
  pers <- c('pers_o', 'pers_c', 'pers_e', 'pers_a', 'pers_n')
  
  list_results <- list()
  
  for(i in pers){
    results_temp <- run_models(y = y, pers = i,  data = data, model = model)
    
    list_results[[i]] <- results_temp
  }
  return(list_results)
}


# define extractor function
extractor <- function(x){
  if(ncol(x)==5){
    subset <- x[,c(1,3,5)]
  }else{
    subset <- x[,c(1,2,4)]
  }
}

renamer <- function(x){
  names(x) <- c('coef', 'se', 'p')
  x <- x %>% rownames_to_column(var = "predictor")
  
  return(x)
}

corstars <- function(x){
  x <- x %>% 
    mutate(stars = ifelse(p < 0.05, '*', '')) %>%
    mutate(stars = ifelse(p < 0.01, '**', stars)) %>%
    mutate(stars = ifelse(p < 0.001, '***', stars))
  
  return(x)
}

dataframer <- function(x){
  
  results_names <- list('base', 'soc', 'econ', 'prev', 'all')
  
  x <- x %>% map(summary) %>%
    map(function(x) x$coefficients) %>%
    map(as.data.frame) %>%
    map(extractor) %>%
    map(as.data.frame) %>%
    map(renamer) %>%
    map(~ filter(., predictor == 'pers'))
  
  names(x) <- results_names
  
  x <- x %>% map(corstars)
  
  return(x)
}

rownamer <- function(x){
  rownames(x) <- c('base', 'soc', 'econ', 'prev', 'all')
  x
}

extract_p <- function(x){
  
  x %>% map(bind_rows) %>% 
    map(rownamer) %>% 
    map(~ .[c('coef', 'se', 'p', 'stars')]) %>% 
    map(rownames_to_column, 'model')
}

model_combiner <- function(x){
  
  trait <- names(x) %>% rep(each=5)
  
  cbind(trait, bind_rows(x))
}

extract_all <- function(x){
  
  x %>% map(dataframer) %>% 
    extract_p() %>% 
    model_combiner()
}
