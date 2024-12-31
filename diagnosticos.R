# Smallest Average Latent Class Posterior Probability (SALC-PP). Este valor es el promedio más bajo de las probabilidades posteriores promedio entre todas las clases. En este caso:
#   
#   Los promedios de las probabilidades posteriores por clase son:
#   
#   Clase 1: 0.05342907
# Clase 2: 0.74219890
# Clase 3: 0.20437204
# El valor más bajo (SALC-PP) es 0.05342907, correspondiente a la Clase 1.
# 
# Posibles razones para el SALC-PP bajo:
#   Tamaño reducido de la Clase 1: Si la Clase 1 tiene muy pocos individuos, las probabilidades promedio tienden a ser más bajas.
# Superposición: Puede haber observaciones cercanas a otras clases, lo que disminuye las probabilidades posteriores.

min(colMeans(glca_mejor_ajuste$posterior$ALL))


posterior_probs <- glca_mejor_ajuste$posterior$ALL

# Paso 1: Determinar la clase más probable para cada individuo
clase_asignada <- apply(posterior_probs, 1, which.max)

# Paso 2: Evaluar si la probabilidad de la clase asignada supera un umbral (por defecto 0.8)
umbral <- 0.8
prob_asignada <- apply(posterior_probs, 1, max)




invisible("probar con distintos umbrales")

# Probar con diferentes umbrales
umbrales <- seq(0.5, 0.9, by = 0.1)

# Calcular tasa general de asignación errónea para cada umbral
tasa_error_umbrales <- sapply(umbrales, function(umbral) {
    prob_asignada <- apply(posterior_probs, 1, max)
    mean(prob_asignada < umbral)
})

# Mostrar los resultados
data.frame(Umbral = umbrales, Tasa_Error = tasa_error_umbrales)



invisible("entropía relativa")

#La entropía que calculaste inicialmente ya es la entropía relativa debido a la 
##normalización implícita en la fórmula. Por eso, no hay diferencia entre los dos cálculos en este caso

