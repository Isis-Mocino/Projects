# Tiempos en la F1
## Rendimiento de neumáticos en el GP de Bahrain

Se registró el tiempo que les tomó a 18 pilotos completar las primeras 13 vueltas del GP de Bahrain 2022.
Esto con el propósito de analizar el rendimiento del neumático blando y cómo es que su desgaste afecta en la velocidad de los pilotos.

Para esto, a pesar de que los datos no son independientes, se usó regresión lineal. Se consideraron dos modelos: uno con todos los datos 
y otro donde solamente se tomaron en cuenta a los pilotos más rápidos.

El resultado del **primer modelo** fue que un neumático nuevo tardaría en promedio de 99.2 a 100.2 segundos con un 95% de confianza. 
Además, con la misma confianza, cada vuelta tarda de 0.12 a 0.245 segundos más en ser completada en promedio. 
Con lo cual podemos decir que hacer el análisis con todos los pilotos conlleva a que perdamos precisión.

En cambio el **segundo modelo** asegura con 95% de confianza que un neumático nuevo tarda de 98 a 98.7 segundos. De igual manera, 
cada vuelta aumenta entre 0.099 y 0.178 segundos más en ser completada.

Los datos se encuentran en _Tabla datos.xlsx_ y fueron obtenidos
de F1 TV. El análisis computacional se encuentra en _GPBahrain2.R_, mientras que los resultados se exponen en:

* Reporte: https://drive.google.com/file/d/1E85r0egofPc8qra4q_Hsl6shJfJ710jF/view?usp=share_link
* Presentación: https://drive.google.com/file/d/1Ba3xEMASX4Q8fTNdtlBRQZsGbX_FJVqC/view?usp=share_link
* Poster: https://drive.google.com/file/d/1U4Z4KBBxSZc_LhhCmFuyTq5h82ZX6eb_/view?usp=share_link

------------------------------------------------------------------------------------------------
## Tire performance at the Bahrain GP

The time it took 18 drivers to complete the first 13 laps of the 2022 Bahrain GP was recorded.
This with the purpose of analyzing the performance of the soft tire and how its wear affects the speed of the pilots.

For this, although the data are not independent, linear regression was used. Two models were considered: one with all the data
and another where only the fastest drivers were taken into account.

The result of the **first model** was that a new tire would take an average of 99.2 to 100.2 seconds with 95% confidence.
Also, with the same confidence, each lap takes 0.12 to 0.245 seconds longer to complete on average.
With which we can say that doing the analysis with all the pilots leads to us losing precision.

On the other hand, the **second model** assures with 95% confidence that a new tire takes from 98 to 98.7 seconds. The same way,
each lap increases between 0.099 and 0.178 more seconds to complete.

The data is found in _Table data.xlsx_ and was obtained
from F1 TV. The computational analysis is in _GPBahrain2.R_, while the results are posted in:

* Report: https://drive.google.com/file/d/1E85r0egofPc8qra4q_Hsl6shJfJ710jF/view?usp=share_link _(Spanish)_
* Presentation: https://drive.google.com/file/d/1Ba3xEMASX4Q8fTNdtlBRQZsGbX_FJVqC/view?usp=share_link _(Spanish)_
* Poster: https://drive.google.com/file/d/1U4Z4KBBxSZc_LhhCmFuyTq5h82ZX6eb_/view?usp=share_link _(Spanish)_
