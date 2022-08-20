#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion np.argmax() de Python hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector
import  numpy as np

np.random.seed(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#que hace qyt tiros libres
def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob) 


#defino los jugadores
mejor = 0.7
peloton = np.array(range(501, 600)) / 1000
jugadores = np.append(peloton, mejor) #intencionalmente el mejor esta al final

#vectorizo la funcion  ftirar
vec_ftirar = np.vectorize(ftirar)

for tiros_libres in [10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000]:
  primero_ganador = 0
  for i in range(10000):
    vaciertos = vec_ftirar(jugadores, tiros_libres) #10 tiros libres cada jugador
    mejor = np.argmax(vaciertos)
    if mejor == 99:
      primero_ganador += 1
  print(tiros_libres, "\t", primero_ganador/10000)


