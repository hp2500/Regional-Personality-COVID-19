
get_results <- function(x){

  x2 <- x %>% summary() %>% 
    coef() %>% 
    as.data.frame() %>% 
    rownames_to_column() 
  
  if(ncol(x2)>5){
    x3 <- x2 %>% select(1,2,3,4,6)
    names(x3) <- c('x', 'coef', 'hr', 'se', 'p')
  }else{
    x3 <- x2 %>% select(1,2,3,5)
    names(x3) <- c('x', 'coef', 'se', 'p')
    
  }
  
  return(x3)
}

joiner <- function(x){
  
  x[[1]] %>% full_join(x[[2]], by='x') %>%
    full_join(x[[3]], by='x') %>%
    full_join(x[[4]], by='x') %>%
    full_join(x[[5]], by='x') 
}

make_table <- function(x){
  
  x2 <- x %>% 
    map(get_results) %>%
    joiner() %>% 
    column_to_rownames("x") %>%
    round(3)
  
  if(ncol(x2)>15){
    
    nms <- c('base_coef', 'base_hr', 'base_se', 'base_p',
             'soc_coef', 'soc_hr', 'soc_se', 'soc_p',
             'econ_coef', 'econ_hr', 'econ_se', 'econ_p',
             'pan_coef', 'pan_hr', 'pan_se', 'pan_p',
             'all_coef', 'all_hr', 'all_se', 'all_p')

  }else{
  
    nms <- c('base_coef', 'base_se', 'base_p',
      'soc_coef', 'soc_se', 'soc_p',
      'econ_coef', 'econ_se', 'econ_p',
      'pan_coef', 'pan_se', 'pan_p',
      'all_coef', 'all_se', 'all_p')
  }
  
  names(x2) <- nms
  
  x2
}


coef_summarizer_proto <- function(x, pred){
  x %>% as.data.frame() %>%
    rownames_to_column() %>% 
    filter(rowname == pred) %>% 
    select(contains('all'))
}


coef_summarizer <- function(x, pred){
  x %>% map(make_table) %>% 
    map(as.data.frame) %>%
    map(coef_summarizer_proto(pred)) %>% 
    bind_rows() %>% 
    summary()
}
