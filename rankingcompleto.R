rankall <- function(resultado,num = "mejor"){
    
    if(file.exists("outcome-of-care-measures.csv") == F){
        if (file.exists("Calidad de Hospitales - data")==T){
            setwd("./Calidad de Hospitales - data")
        }else{
            stop("No existe la base de datos en el directorio." )
            break 
        }
    }
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state <- names( split(outcome$State,outcome$State))
    
    if((resultado != "ataque") && (resultado != "falla") && (resultado 
                                                             != "neumonia")){
        stop("Resultado invalido.")
        break
    }
    
    if((num != "mejor") && (num != "peor") && (as.numeric(num) != 
                                               as.integer(num))){
        stop("num invalido.")
        break
    }
    
    
    if (resultado == "ataque"){
        
        tabla <- cbind(outcome[,2],outcome[,7],outcome[,11])
        
    }else{
        
        if (resultado == "falla"){
            
            tabla <- cbind(outcome[,2],outcome[,7],outcome[,17])
        }else{
            
            tabla <- cbind(outcome[,2],outcome[,7],outcome[,23])
        }
    }
    
    completo <- data.frame()
    
    for (estado in state){
        filtro1 <- cbind((tabla[(tabla[,2] == estado),1]),(tabla[(tabla[,2] == 
                                                                      estado),3]))
        
        filtro2 <- cbind(filtro1[(filtro1[,2] != "Not Available"),1],
                         as.numeric(filtro1[(filtro1[,2] != "Not Available"),2]))
        filtro3 <- sort(as.numeric(filtro2[,2]))
        
        if(num == "mejor"){
            num <- 1
        }
        
        if(num == "peor"){
            num <- length(filtro3)
        }
        
        filtro4 <- cbind(filtro2[(as.numeric(filtro2[,2]) <= (filtro3[num])  ),1],
                         filtro2[(as.numeric(filtro2[,2]) <= (filtro3[num])  ),2])
        x <- length(cbind(filtro4[(as.numeric(filtro4[,2]) < (filtro3[num])  ),1]))
        filtro5 <- cbind(filtro4[(as.numeric(filtro4[,2]) == (filtro3[num])  ),1])
        data.frame(filtro5)
        hospital <- sort(filtro5)[num-x]
        completo <- rbind(completo,cbind(hospital,estado))
    }
    row.names(completo) <- completo[,2]
    completo
}
tail(rankall("falla"),10)
