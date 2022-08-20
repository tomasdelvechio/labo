#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion findmax() de Julia hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector
using Random

Random.seed!(102191)

function ftirar(prob, qty)
  return   sum( rand() < prob for i in 1:qty )
end


#defino los jugadores
mejor   = [0.7]
peloton = Vector((501:599) / 1000)
jugadores = append!(peloton, mejor) #intencionalmente el mejor esta al final





for tiros_libres in [ 10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000 ]

  primero_ganador = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadores, tiros_libres)
    mejor = findmax( vaciertos )[2]
    if mejor == 100   primero_ganador += 1   end
  end

  println( tiros_libres,  "\t", primero_ganador/10000 )
end

