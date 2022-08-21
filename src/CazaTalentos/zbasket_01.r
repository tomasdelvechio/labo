gustavo_ganador <- 0

for (i in 1:10000) {
  aciertos_michael <- sum(runif(10) < 0.85)
  aciertos_gustavo <- sum(runif(10) < 0.10)

  if (aciertos_gustavo > aciertos_michael)
    gustavo_ganador <- gustavo_ganador + 1
}

print(gustavo_ganador)
