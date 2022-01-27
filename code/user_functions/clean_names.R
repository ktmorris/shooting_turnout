clean_names <- function(data){
  return(gsub("[.]", "_", make.unique(make.names(tolower(colnames(data))))))
}
