import numpy as np

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)

gustavo_ganador = 0

for i in range(1, 10001):
    aciertos_michael = ftirar(0.85, 10)
    aciertos_gustavo = ftirar(0.10, 10)

    if (aciertos_gustavo > aciertos_michael):
        gustavo_ganador = gustavo_ganador + 1

print(gustavo_ganador)
