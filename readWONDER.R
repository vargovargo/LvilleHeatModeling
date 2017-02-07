readWONDER <- function(file){
  
  foo<-readLines(file)
  foo<-foo[-1]
  
  if(length(grep("---",foo))>0){
    foo<-foo[-c(grep("---",foo):length(foo))]
  }
  n <- as.numeric(length(foo))
    
  wonder <- read.table(file, sep="\t", as.is = T, nrows = n, skip=1)
  header <- read.table(file, sep="\t", as.is = T, nrows = 1)
  colnames(wonder) <- unlist(header)
  
  wonder$Deaths <- ifelse(wonder$Population== "Suppressed" | wonder$Population== "Missing" , 0, wonder$Deaths)
  wonder$POPclean <- ifelse(wonder$Population== "Suppressed", sample(1:9), as.integer(as.character(wonder$Population))) 
  wonder$POPclean <- ifelse(wonder$Population== "Missing", sample(1:9), as.integer(as.character(wonder$POPclean))) 
  
  return(wonder)
  
}
