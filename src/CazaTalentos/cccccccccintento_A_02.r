#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<- sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_B <- function() {
  gimnasio_init()
  planilla_cazatalentos <- data.table(ids_juegan1 = 1:100, aciertos = 0)  # Planilla
  
  #Ronda 1: 110 tiros
  resultado1  <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, 80)
  planilla_cazatalentos <- planilla_cazatalentos[, aciertos := resultado1]
  
  #Ronda 2-3 - seleccion
  for (ronda in 2:3) {
    #ordeno x aciertos a los mejores 15
    planilla_cazatalentos <- planilla_cazatalentos[order(-aciertos)[1:50]]
    # Tiros adicionales por ronda (60, 60)
    tiros_ronda <- c(30, 30)[ronda - 1] 
    resultados2 <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, tiros_ronda)
    planilla_cazatalentos[, aciertos := aciertos + resultados2]  # Acumular aciertos
  }

  #Ronda 2-3 - seleccion
  for (ronda in 2:3) {
    #ordeno x aciertos a los mejores 15
    planilla_cazatalentos <- planilla_cazatalentos[order(-aciertos)[1:30]]
    # Tiros adicionales por ronda (60, 60)
    tiros_ronda <- c(30, 30)[ronda - 1] 
    resultados3 <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, tiros_ronda)
    planilla_cazatalentos[, aciertos := aciertos + resultados3]  # Acumular aciertos
  }

  #Ronda 3-4 - seleccion
  for (ronda in 2:3) {
    #ordeno x aciertos a los mejores 10
    planilla_cazatalentos <- planilla_cazatalentos[order(-aciertos)[1:20]]
    
    # Tiros adicionales por ronda (60, 60)
    tiros_ronda <- c(30, 30)[ronda - 1] 
    resultados4 <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, tiros_ronda)
    planilla_cazatalentos[, aciertos := aciertos + resultados4]  # Acumular aciertos
  }
  
  #Ronda 4-5 - seleccion
  for (ronda in 2:3) {
    #ordeno x aciertos a los mejores 10
    planilla_cazatalentos <- planilla_cazatalentos[order(-aciertos)[1:10]]
    
    # Tiros adicionales por ronda (60, 60)
    tiros_ronda <- c(30, 30)[ronda - 1] 
    resultados5 <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, tiros_ronda)
    planilla_cazatalentos[, aciertos := aciertos + resultados5]  # Acumular aciertos
  }
  
  #Ronda 6 - seleccion
  planilla_cazatalentos <- planilla_cazatalentos[order(-aciertos)[1:5]]
  resultados6 <- gimnasio_tirar(planilla_cazatalentos$ids_juegan1, 80)
  planilla_cazatalentos[, aciertos := aciertos + resultados6]
  
id_mejor <- planilla_cazatalentos[which.max(aciertos), ids_juegan1]
veredicto <- gimnasio_veredicto(id_mejor)
return(veredicto)
  
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 200003 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total
tasa_eleccion_correcta

#Esta estrategia elije al verdadero_mejor el 99% de las veces
#pero lamentablemente necesita de un total de 36600   tiros libres

