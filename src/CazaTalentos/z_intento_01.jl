#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion findmax() de Julia hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector
using Base.Iterators: partition
using Random

semillas = 10000:11000

#Random.seed!(102191)


function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end

veces_que_gano_michael = 0
cantidad_tiros_totales = 0 # En cada vuelta dará lo mismo, solo lo necesitamos su valor al final

for semilla in semillas
    global veces_que_gano_michael, cantidad_tiros_totales
    Random.seed!(semilla)

    #defino los jugadores
    mejor   = [0.7]
    peloton = Vector((501:599) / 1000)
    jugadores = append!(peloton, mejor) #intencionalmente el mejor esta al final

    longitud_grupo = 10
    tiros_libres = 10



    ganadores_rondas = Vector()

    cantidad_tiros_totales = 0

    for ronda in 1:20
        #global ganadores_rondas, cantidad_tiros_totales
        ganadores_grupo = Vector()
        grupos = collect(Iterators.partition(shuffle(jugadores), longitud_grupo))
        for grupo in grupos
            vaciertos = ftirar.(grupo, tiros_libres)
            cantidad_tiros_totales += longitud_grupo * tiros_libres
            mejor_del_grupo = findmax( vaciertos )[2]
            jugador_ganador = grupo[mejor_del_grupo]
            ganadores_grupo = append!(ganadores_grupo, jugador_ganador)
        end
        ronda_aciertos = ftirar.(ganadores_grupo, tiros_libres)
        cantidad_tiros_totales += length(ganadores_grupo) * tiros_libres
        mejor_de_ronda = findmax( ronda_aciertos )[2]
        jugador_ganador_ronda = ganadores_grupo[mejor_de_ronda]
        ganadores_rondas = append!(ganadores_rondas, jugador_ganador_ronda)
    end

    #println(ganadores_rondas)

    ronda_final_aciertos = ftirar.(ganadores_rondas, 60)
    cantidad_tiros_totales += length(ganadores_rondas) * 60
    mejor_ronda_final = findmax( ronda_final_aciertos )[2]
    jugador_ganador_final = ganadores_rondas[mejor_ronda_final]

    #println(cantidad_tiros_totales)
    #println(jugador_ganador_final)
    if (jugador_ganador_final == 0.7) veces_que_gano_michael += 1 end
end

println(cantidad_tiros_totales)
println(veces_que_gano_michael / length(semillas))