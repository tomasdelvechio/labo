
using Base.Iterators: partition
using Random

function ftirar(prob, qty)
    return  sum( rand() < prob for i in 1:qty )
end

mejor   = [0.7]
peloton = Vector((501:599) / 1000)
jugadores = append!(peloton, mejor)

triunfos_primer_ronda = zeros(length(jugadores))

# Parametros de la simulación
longitud_grupo_primera_ronda = 5
tiros_primera_ronda = 3
iteraciones_primer_ronda = 10

############################
# 1er Ronda
#
#   Se arman 20 grupos de 5, donde todos compiten contra todos en 
#   10 enfrentamientos. Los dos mejores de cada grupo pasan a 2da ronda
############################

grupos_primer_ronda = collect(Iterators.partition(shuffle(jugadores), longitud_grupo_primera_ronda))

for grupo in grupos_primer_ronda
    global tiros_primera_ronda, iteraciones_primer_ronda
    #for iteracion in 1:iteraciones_primer_ronda
    vaciertos = ftirar.(grupo, tiros_primera_ronda * iteraciones_primer_ronda)
    ganadores = findall(x -> x == findmax(vaciertos)[1], vaciertos)
    for ganador in ganadores
        # ACA hay que agarrar el value del ganador
        # Con eso recuperar el indice en el grupo y ver el valor del jugador
        # Ir al vector de jugadores y conseguir el id del jugador
        # Con ese id sumar uno en triunfos_primer_ronda
    end

    #mejor = findmax( vaciertos )[1]
    
    #end
    println(grupo)
    println(vaciertos)
    println("############################")
    #mejor = findmax( vaciertos )[2]
    #if mejor == 100   primero_ganador += 1   end
end
