library(NCmisc)
must.use.package('dplyr')
must.use.package('stringr')

StandardizeMoney <- function(value){
  
  convertedValue <- sapply(value, USE.NAMES = F, function(value){
    
    convertedValue <- str_remove(value, '^\\D*')
    
    if (convertedValue == '0') {
      convertedValue <- 0
    } else{
      convertedValue <- ifelse(
        str_detect(convertedValue, 'K$'), 
        str_remove(convertedValue, 'K$') %>%
          as.numeric(),
        str_remove(convertedValue, 'M$') %>%
          as.numeric() * 1000
      )
    }
    
    return(convertedValue)
  })
  
  return(convertedValue)
}

