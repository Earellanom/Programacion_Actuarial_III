setwd("C:/Users/Erick/Desktop/Programacion Actuarial III/Programacion_Actuarial_III/specdata")
directorio <- getwd() 

completos <- function(directorio = directorio, id=1:332){  
    v <- c()   
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
        
        x <- read.csv(fuente)
        r <- nrow(x)
        com <- 0
        for(j in 1:r){
            #Asumimos que son competos cuando tienen datos numericos tanto en 
            #"sulfate" como en "nitrate"
            y1=is.na(x[j,2])
            y2=is.na(x[j,3])
            if (y1==T | y2==T){
                    com <- com 
            }else{
                com <- com + 1
            }
        }
    v <- c(v,com)
    }
    
    id<-c(id)
    nobs<-c(v)
    k <- data.frame(id,nobs)
    k
}

 completos()
