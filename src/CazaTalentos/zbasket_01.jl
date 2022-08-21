
gustavo_ganador = 0

function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end

for i = 1:10000
    aciertos_michael = ftirar(0.85, 10)
    aciertos_gustavo = ftirar(0.10, 10)

    if (aciertos_gustavo > aciertos_michael)
        gustavo_ganador = gustavo_ganador + 1
    end
end

println(gustavo_ganador)
