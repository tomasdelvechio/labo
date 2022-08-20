using Random

Random.seed!(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#que hace qyt tiros libres

function ftirar(prob, qty)
  return  sum(rand() < prob for i in 1:qty)
end


#defino los jugadores
mejor = [0.7]
peloton = Vector((501:599) / 1000)
jugadores = append!(mejor, peloton)

#veo que tiene el vector
jugadores




for i = 1:10
  vaciertos = ftirar.(jugadores, 10)  #10 tiros libres cada jugador
  mejor = findmax(vaciertos)[2]
  aciertos_torneo = vaciertos[ mejor ]

  aciertos_segunda = ftirar.(jugadores[ mejor ], 10)

  println(aciertos_torneo, "\t", aciertos_segunda)
end

