library(tidyverse)

my_data<-read_csv("ImputationData.csv")
#df %>% mutate_if(is.character, list(~na_if(.,"")))
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
for (i in 2:length(my_data)){
  if(is.character(unlist(my_data[i])) == TRUE){
    my_data[i]<-replace_na(unlist(my_data[i]), Mode(unlist(my_data[i]), na.rm = TRUE))
  } else {
    my_data[i]<-replace_na(unlist(my_data[i]), mean(unlist(my_data[i]), na.rm = TRUE))
  }
}
