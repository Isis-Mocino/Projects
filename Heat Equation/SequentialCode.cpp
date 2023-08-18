#include <iostream>
#include <time.h>
#include <cmath>

using namespace std;

int main(){

    /* --------------------- DECLARACION VARIABLES -------------------------------------
    ---- Consideraremos la funcion de calor u(x,y,t), x -> filas, y -> columnas ----- */

    // DISCRETIZACION DOMINIO ECUACION:
    int Nx, Ny, Nt;                         // Numero de divisiones de x, y, t
    double x_i, x_f, y_i, y_f, t_i, t_f;    // Extremos intervalos de x, y, t
    double dx, dy, dt;                      // Diferencial x, y, t
    double *x, *y;                          // Puntos resultantes en x, y
    double **u;                             // Matriz del dominio evaluado en la iteracion n+1
    double **u_old;                         // Matriz del dominio evaluado en la iteracion n

    // ELEMENTOS ECUACION:
    double alp;                             // Coeficiente de difusión
    double rx, ry;                          // Coeficientes usados en la suma
    double sum;                             // Para verificar resultados

    /* ------------------------ INICIALIZACION --------------------------------------- */

    // DOMINIO: Consideraremos (x,y) en [0,1]x[0,1] y t en [0,0.5]
    x_i = y_i = t_i = 0.0;
    x_f = y_f = 1.0;
    t_f = 0.5;

    // DISCRETIZACION:
    Nt = 100000;
    Nx = Ny = 200;
    dx = (x_f - x_i) / (Nx - 1);
    dy = (y_f - y_i) / (Ny - 1);
    dt = (t_f - t_i) / (Nt - 1);

    // ELEMENTOS ECUACION:
    alp = 1;
    rx = alp * (dt / (dx * dx));
    ry = alp * (dt / (dy * dy));

    // MENSAJE:
    cout << "\nDominio: [0,1] x [0,1]\nDivision: " << Nx << " x " << Ny << "\n";
    cout << "Criterio CFL: " << rx + ry << " < 1/2\n";

    /* ------------------------ COMIENZA TIEMPO -------------------------------------- */
    t_i = clock();

    /* ------------------------ RESERVA MEMORIA ----------------------------------------- 
    ---------- Tomamos un espacio mas pues el contador inicia en 1 --------------------*/

    x = new double [Nx + 1];
    y = new double [Ny + 1];

    u = new double* [Nx + 1];
    u_old = new double* [Nx + 1];

    for(int i = 1; i <= Nx; i++){
        u[i] = new double [Ny + 1];
        u_old[i] = new double [Ny + 1];
    }

    /* ---------------------------- PUNTOS INICIALES --------------------------------- */
    
    // Puntos en x
    for(int i = 1; i <= Nx; i++)
        x[i] = x_i + (i - 1) * dx;
    
    // Puntos en y
    for(int i = 1; i <= Ny; i++)
        y[i] = y_i + (i - 1) * dy;

    // Llenamos toda la matriz
    for(int i = 1; i <= Nx; i++)
        for(int j = 1; j<= Ny; j++)
            u_old[i][j] = u[i][j] = sin(x[i] + y[j]) * sin(x[i] + y[j]);

    // Cambiamos la frontera de la matriz en iteracion n + 1
    for(int i = 1; i <= Ny; i++)
        u[1][i] = u[Nx][i] = 1.0;

    for(int i = 1; i <= Nx; i++)
        u[i][1] = u[i][Ny] = 1.0;

    /* ----------------------------------- METODO ------------------------------------ */

    for(int k = 1; k <= Nt; k++){
        for(int i = 2; i < Nx; i++){
            for(int j = 2; j < Ny; j++){
                double c = 1.0 - (2.0 * (rx + ry));
                u[i][j] = (c * u_old[i][j])
                        + (rx * u_old[i - 1][j]) + (rx * u_old[i + 1][j])
                        + (ry * u_old[i][j - 1]) + (ry * u_old[i][j + 1]);
            }
        }
        
        // Si es la primer iteracion, cambiamos la frontera de la matriz inicial
        if(k == 1){
            for(int i = 1; i <= Ny; i++)
                u_old[1][i] = u_old[Nx][i] = 1.0;
            for(int i = 1; i <= Nx; i++)
                u_old[i][1] = u_old[i][Ny] = 1.0;
        }

        swap(u, u_old); // En lugar de copiar matriz solo cambiamos la referencia
    }
    swap(u, u_old);

    /* ------------------------- TERMINA TIEMPO -------------------------------------- */
    t_f = clock();

    /* ------------------------- VERIFICAMOS RESULTADOS ------------------------------ */
    sum = 0.0;

    for(int i = 1; i <= Nx; i++)
        for(int j = 1; j <= Ny; j++)
            sum += u[i][j];
    
    cout << "Suma total: " << sum << '\n';
    cout << "Tiempo: " << (t_f - t_i) / CLOCKS_PER_SEC << "\n\n";

    /* ------------------------- LIBERA MEMORIA -------------------------------------- */
    for(int i = 1; i <= Nx; i++){
        delete [] u[i];
        delete [] u_old[i];
    }
    delete [] x;
    delete [] y;
    delete [] u;
    delete [] u_old;

    return 0;
}