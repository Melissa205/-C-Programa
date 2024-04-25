# Abril 2024
# Descripción: Realiza el cálculo de ATP totales obtenidos de la Beta oxidación de ácidos grasos saturados de cadenas pares e impares.

# Argumentos
c #número de carbonos del ácido graso

#Código
#creacion de la función
ATP_cal <- function(c) {
  
  if(c<=3 | c>20) stop("La cadena del ácido graso debe tener mínimo 4 carbonos y máximo 20") #validar que el numero de carbonos sea el indicado en la literatura
  
  
  #cadenas pares
  if(c %% 2 == 0){    #validar que la cadena de carbonos de carbonos sea par  
    ciclos <- (c/2)-1 
    acetil <- (c/2)    #cada dos carbonos se hace un ciclo
    ATPN <- ciclos*3   #oxidación del NADH que proporciona 3 ATP
    ATPF <- ciclos*2   #oxidación del FADH2 que proporciona 2 ATP
    ATPAc <- acetil*12 #oxidación del acetil CoA en una vueta del ciclo que proporciona 12 ATP
    ATPT <- ATPN + ATPF + ATPAc  #suma de todos los ATP
    ATPE <- ATPT - 2          #rendimiento energético total menos 2 ATP para la activación
    
    #impresión
    paste0(" En total se producen ",  ATPE , " ATP provenientes de " , ATPN , " NADH, ", 
           ATPF, " FADH2 " ," y ", ATPAc , " de acetilCoA ")
  }else{
    
    ciclos <- (c-3)/2 #cadenas impares
    acetil <- (c/2) #cada dos carbonos se hace un ciclo
    ATPN <- ciclos*3  #oxidación del NADH que proporciona 3 ATP
    ATPF <- ciclos*2  #oxidación del FADH2 que proporciona 2 ATP
    ATPAc <- acetil*12 #oxidación del acetil CoA en una vueta del ciclo que proporciona 12 ATP
    ATPpr <- 12        #el propionil CoA tiene un equivalente a 3 NADH, 3 FADH2 Y 1 GTP
    ATPT <- ATPN + ATPF + ATPAc + ATPpr #suma de todos los ATP
    ATPE <- ATPT - 2 #rendimiento energético total menos 2 ATP para la activación
    
    
    #impresion
    paste0(" En total se producen ",  ATPE , " ATP provenientes de " , ATPN , " NADH, ", 
           ATPF , " FADH2, ", ATPAc , " de acetilCoA ", " y " , ATPpr ,
           " provenientes del propionil CoA")
    
  }
  
}

