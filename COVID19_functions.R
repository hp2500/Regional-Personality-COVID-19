# calculates all relevant models
run_models <- function(y, pers, data, model){
  
  # subset data
  data = data %>% 
    dplyr::rename(y = all_of(y),
                  pers = all_of(pers))
  
  if(model == 'lm'){
    
    # no controls
    lm_base <- lm(y ~ pers, 
                  data = data)
    
    # socdem
    lm_soc <- lm(y ~ pers + 
                   age + male + conservative,
                 data = data)
    
    # econ
    lm_econ <- lm(y ~ pers + 
                    academics + medinc + manufact,
                  data = data)
    
    # pandemic
    lm_pan <- lm(y ~ pers + 
                   airport_dist + tourism + healthcare + popdens,
                 data = data)
    
    if(!str_detect(y, 'socdist')){
      
      # all
      lm_all <- lm(y ~ pers + 
                     age + male + conservative +
                     academics + medinc + manufact +
                     airport_dist + tourism + healthcare + popdens,
                   data = data)
    }
    
    if(str_detect(y, 'socdist')){
      
      # all
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
    
    # none
    cox_base <- coxph(Surv(y, event) ~ pers, 
                      data = data)
    
    # soc
    cox_soc <- coxph(Surv(y, event) ~ pers + 
                       age + male + conservative,
                     data = data)
    
    # econ
    cox_econ <- coxph(Surv(y, event) ~ pers + 
                        academics + medinc + manufact,
                      data = data)
    
    # pan
    cox_pan <- coxph(Surv(y, event) ~ pers + 
                       airport_dist + tourism + healthcare + popdens,
                     data = data)
    
    if(!str_detect(y, 'socdist')){
      
      cox_all <- coxph(Surv(y, event) ~ pers + 
                         age + male + conservative +
                         academics + medinc + manufact +
                         airport_dist + tourism + healthcare + popdens,
                       data = data)
    }
    
    if(str_detect(y, 'socdist')){
      
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

# fits models for all traits
run_analyses <- function(y, data, model){
  
  pers <- c('pers_o', 'pers_c', 'pers_e', 'pers_a', 'pers_n')
  
  list_results <- list()
  
  for(i in pers){
    results_temp <- run_models(y = y, pers = i,  data = data, model = model)
    
    list_results[[i]] <- results_temp
  }
  return(list_results)
}

# extracts results columns
extractor <- function(x){
  if(ncol(x)==9){
    subset <- x[,c(1,2,3,5,8,9)]
  }else{
    subset <- x[,c(1,2,4)]
  }
}

# names results columns
renamer <- function(x){
  if(ncol(x)>=4){
    names(x) <- c('coef','hr', 'se', 'p', 'lb95', 'ub95')
  }else{
    names(x) <- c('coef', 'se', 'p')
  }
  
  x <- x %>% rownames_to_column(var = "predictor")
  
  return(x)
}

# adds stars to p-values
corstars <- function(x){
  x <- x %>% 
    mutate(stars = ifelse(p < 0.05, '*', '')) %>%
    mutate(stars = ifelse(p < 0.01, '**', stars)) %>%
    mutate(stars = ifelse(p < 0.001, '***', stars))
  
  return(x)
}

# extracts coefficients and confint
coef_confint <- function(x){
  cbind(x$coefficients, x$conf.int)
}


# turns model output into dataframe 
dataframer <- function(x){
  
  results_names <- list('base', 'soc', 'econ', 'prev', 'all')
  
  x <- x %>% map(summary) %>%
    map(coef_confint) %>%
    map(as.data.frame) %>%
    map(extractor) %>%
    map(as.data.frame) %>%
    map(renamer) %>%
    map(~ filter(., predictor == 'pers'))
  
  names(x) <- results_names
  
  x <- x %>% map(corstars)
  
  return(x)
}

# integrates model configurations as row names
rownamer <- function(x){
  rownames(x) <- c('base', 'soc', 'econ', 'prev', 'all')
  x
}


# extracts relevant information from model output
extract_p <- function(x){
  
  names_temp <- x[[1]]$base %>% names()
  
  if('hr' %in% names_temp){
    x %>% map(bind_rows) %>% 
      map(rownamer) %>%
      map(~ .[c('coef', 'hr', 'se', 'p', 'lb95', 'ub95', 'stars')]) %>% 
      map(rownames_to_column, 'model')
  }else{
    x %>% map(bind_rows) %>% 
      map(rownamer) %>%
      map(~ .[c('coef', 'se', 'p', 'stars')]) %>% 
      map(rownames_to_column, 'model')  
    
    }
}

# combines different list elements into single data frame
model_combiner <- function(x){
  
  trait <- names(x) %>% rep(each=5)
  
  cbind(trait, bind_rows(x))
}

# generate final data frame with all results
extract_all <- function(x){
  
  x %>% map(dataframer) %>% 
    extract_p() %>% 
    model_combiner()
}

run_mlm <- function(y, pers_reg, pers_ind, lvl2, data){
  
  # subset data
  data = data %>% 
    dplyr::rename(y = all_of(y),
                  pers_reg = all_of(pers_reg),
                  pers_ind = all_of(pers_ind),
                  lvl2 = all_of(lvl2))
  
  # no controls
  mlm_base <- lmer(y ~ pers_reg + pers_ind + days + rate_day + 
                     (pers_ind|lvl2), 
                   data = data)
  
  # socdem
  mlm_soc <- lmer(y ~ pers_reg + pers_ind + days + rate_day +
                    age.x + gender + conservative + age.y + male +
                    (pers_ind|lvl2), 
                  data = data)
  
  # econ
  mlm_econ <- lmer(y ~ pers_reg + pers_ind + days + rate_day +
                     educ + income  + manufact +  academics + medinc +
                     (pers_ind|lvl2), 
                   data = data)
  
  # pandemic
  mlm_pan <- lmer(y ~ pers_reg + pers_ind + days + rate_day +
                    hhmember + airport_dist + popdens + tourism + healthcare + 
                    (pers_ind|lvl2), data = data)
  
  # all
  mlm_all <- lmer(y ~ pers_reg + pers_ind + days + rate_day + 
                    age.x + gender + conservative + age.y + male +
                    educ + income  + manufact +  academics + medinc +
                    hhmember + airport_dist + popdens + tourism + healthcare + 
                    (pers_ind|lvl2), data = data)
  
  # create list with results
  results <- list('mlm_base' = mlm_base, 
                  "mlm_soc" = mlm_soc,
                  "mlm_econ" = mlm_econ,
                  "mlm_pan" = mlm_pan,
                  "mlm_all" = mlm_all)
}

mlm_aggregator <- function(model_list){

  model_df <- model_list %>%
    map(summary) %>%
    map(coefficients) %>%
    map(as.data.frame) %>%
    map(rownames_to_column, 'pred') %>% 
    bind_rows()
  
  df_dims <- model_list %>%
    map(summary) %>%
    map(coefficients) %>%
    map(as.data.frame) %>%
    map(nrow) %>%
    unlist() %>%
    as.vector()
  
  model_conf <- rep(c('base', 'soc', 'econ', 'pan', 'all'), df_dims)
  
  cbind(model_conf, model_df)
  
}


