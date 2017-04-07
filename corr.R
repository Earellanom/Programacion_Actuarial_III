setwd("C:/Users/Erick/Desktop/Programacion Actuarial III/Programacion_Actuarial_III/specdata")
directorio <- getwd()

corr <- function(directorio=directorio,horizonte=0){
    v1 <- c() 
    v2 <- c() 
    v3 <- c() 
    v4 <- c() 
    for (i in 1:332){
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
        if (com > horizonte){
            for(k in 1:r){
                y1=is.na(x[k,2])
                y2=is.na(x[k,3])
                if (y1==F & y2==F){
                    v1 <- c(v1, x[k,2])
                    v2 <- c(v2, x[k,3])
                }
                as.numeric(v1)
                as.numeric(v2)
                c <- cor(as.numeric(v1), as.numeric(v2))
            }
            v3 <- c(v3, c)
            v4 <- c(v4, i)
        }
       names(v3) <- v4
    }
    v3
}
corr(directorio,500)
