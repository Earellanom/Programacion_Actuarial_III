setwd("C:/Users/Erick/Desktop/Programacion Actuarial III/Programacion_Actuarial_III/specdata")
directorio <- getwd()
S <- "sulfate"
N <- "nitrate"
mediacontaminante <- function(directorio=directorio,contaminante = S,id=1:332) {
    con <- c()
    for (i in id){
        if (nchar(i) == 1){
            fuente <- "00" 
        }
        if (nchar(i)==2){
            fuente <- "0"
        }
        if (nchar(i)==3){
            fuente <- ""
        }
                   
         fuente <- paste(fuente,i,".csv", sep = "")
         t <- read.csv(fuente)
         n <- nrow(t)
         if (contaminante == S){
             x <- t[1:n,2] 
             }else{
                 x <- t[1:n,3]
             }
        con <- c(con,x)
        
    }
    m <- cbind(con)
    v<- mean(m[,1],na.rm = T)
    v
}
  
mediacontaminante()