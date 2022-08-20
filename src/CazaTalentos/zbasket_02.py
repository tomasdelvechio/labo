import  numpy as np

np.random.seed(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob
#haciendo qyt tiros libres

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)



#defino los jugadores
mejor = 0.7
peloton = np.array(range(501, 600)) / 1000
jugadores = np.append(mejor, peloton)

#veo que tiene el vector
jugadores

#vectorizo la funcion  ftirar
vec_ftirar = np.vectorize(ftirar)

#hago que los 100 jugadores tiren 10 veces cada uno
vec_ftirar(jugadores, 10)

primero_ganador = 0

for i in range(10000): #diez mil experimentos
  vaciertos = vec_ftirar(jugadores, 10) #10 tiros libres cada jugador
  mejor = np.argmax(vaciertos)
  if mejor == 0:
    primero_ganador += 1




print(primero_ganador)
