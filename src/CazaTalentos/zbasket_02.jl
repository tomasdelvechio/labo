using Random

Random.seed!(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qyt tiros libres

function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end


#defino los jugadores
mejor     = [0.7]
peloton   = Vector((501:599) / 1000)
jugadores = append!( mejor, peloton)

#veo que tiene el vector
jugadores




#hago que los 100 jugadores tiren 10 veces cada uno
res = ftirar.( jugadores, 10 )

primero_ganador = 0

for i = 1:10000  #diez mil experimentos
  vaciertos = ftirar.(jugadores, 10)  #10 tiros libres cada jugador
  mejor = findmax( vaciertos )

  if mejor[2] == 1
      primero_ganador += 1
  end
end

print( primero_ganador )
