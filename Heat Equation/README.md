# Parallelization of the heat equation in 2D

## Spanish version

Se resolvió la ecuación de calor bidimensional
$$\frac{\partial u}{\partial t} = \alpha \left(\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2}\right),$$
donde $u(x,y,t)$ es la función de temperatura en las coordenadas $(x,y)$ y el tiempo $t$. Se consideró la condición inicial de Dirichlet, el dominio de la
placa $[0,1]\times[0,1]$, $t\in [0,0.5]$ y $\alpha = 1$. Las soluciones propuestas hacen uso de diferencias finitas, el intervalo
$[0,1]$ se discretiza en 200 puntos y el intervalo $[0,0.5]$ en 100,000.

Se presentan dos versiones de la solución: secuencial y **paralela**. La versión paralela se realizó con MPI y se optó por la estrategia de cálculo *overlapping domain decomposition*.

Con el código secuencial el tiempo promedio de solución fueron 54 segundos. Con el código paralelo el tiempo promedio fue de 2.78, 1.68 y 1.52 para 4, 8 y 12 procesos respectivamente.
Estos códigos se encuentran en los archivos *ParallelCode.cpp* y *SequentialCode.cpp*.

Para leer el reporte completo, consulte el archivo *Report.pdf*.

## English version

The two-dimensional heat equation
$$\frac{\partial u}{\partial t} = \alpha \left(\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2}\right)$$
—where $u(x,y,t)$ is the temperature function in the coordenates $(x,y)$ and the time $t$— was solved. Dirichlet's initial condition, the domain of the
plate as $[0,1]\times[0,1]$, $t\in [0,0.5]$ and $\alpha = 1$ were considered. The proposed solutions make use of finite differences, the interval
$[0,1]$ discretized to 200 points and the interval $[0,0.5]$ to 100,000.

Two versions of the solution are presented: sequential and **parallel**. The parallel version was carried out with MPI and the calculation strategy *overlapping domain decomposition* was chosen.

With the sequential code, the average solution time was 54 seconds. With the parallel code, the average time was 2.78, 1.68 and 1.52 for 4, 8 and 12 processes respectively.
These codes can be found in the files *ParallelCode.cpp* y *SequentialCode.cpp*.

For reading the complete report, see the file *Report.pdf*.
