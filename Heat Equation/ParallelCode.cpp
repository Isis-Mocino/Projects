#include <iostream>
#include <time.h>
#include <cmath>
#include <mpi.h>

using namespace std;

int main(int argc, char *argv[]){

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

    /* ------------------- DECLARACION VARIABLES MPI ---------------------------------*/

    // PROCESOS:
    int ntasks;                             // Numero de procesos
    int taskid;                             // ID del proceso

    // VARIABLES DE PROPOSITO GLOBAL
    int NxG, NyG;                           // Divisiones (globales) sobre eje x,y
    int *index_global;                      // Mapeo entre indices locales a globales
    double *xG, *yG;                        // Puntos x,y globales

    // DIVISIONES POR BLOQUE:
    int NN;                                 // Divisiones sobre el bloque a paralelizar
    int it_i, it_f;                         // Variables que definen rangos de evaluacion segun el bloque

    // COMUNICACION:
    int vecino[2];                          // Vector de vecino proveniente y destino
    int direction;                          // x-direction = 0 | y-direction = 1
    int displasment;                        // Distancia a vecino

    // TOPOLOGIA CARTESIANA:
    int ndims = 2;                          // Dimension del espacio a usar
    int dims[2];                            // Tamano de cada componente de dimension
    int periods[2];                         // Periodicidad por dimension
    int reorder;                            // Reorganizacion del grid
    int coords[2];                          // Coordenadas de acuerdo con topologia cartesiana

    // AUXILIARES:
    int etiqueta = 888;                     // Etiqueta de comunicacion
    double suma_global;                     // Verificacion de resultados

    /* ------------------------ INICIALIZACION --------------------------------------- */

    // DOMINIO: Consideraremos (x,y) en [0,1]x[0,1] y t en [0,0.5]
    x_i = y_i = t_i = 0.0;
    x_f = y_f = 1.0;
    t_f = 0.5;

    // DISCRETIZACION:
    Nt = 100000;
    NxG = NyG = 200;
    dx = (x_f - x_i) / (NxG - 1);
    dy = (y_f - y_i) / (NyG - 1);
    dt = (t_f - t_i) / (Nt - 1);

    // ELEMENTOS ECUACION:
    alp = 1.0;
    rx = alp * (dt / (dx * dx));
    ry = alp * (dt / (dy * dy));

    // MENSAJE:
    cout << "\nDominio: [0,1] x [0,1]\nDivision: " << NxG << " x " << NyG << "\n";
    cout << "Criterio CFL: " << rx + ry << " < 1/2\n";

    /* -------------------- INICIALIZACION COMPONENTES MPI --------------------------- */

    // INICIAMOS LA REGION PARALELA
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &ntasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &taskid);

    // INFORMACION PARA LA TOPOLOGIA CARTESIANA
    dims[0] = ntasks; 
    dims[1] = 1;
    periods[0] = periods[1] = 0;
    reorder = 0;

    // CREAMOS COMUNICADOR CON LA TOP CART
    MPI_Comm comm2D;
    MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &comm2D);

    // COORDENADAS DEL HILO
    MPI_Cart_coords(comm2D, taskid, ndims, coords);
    cout << "Task ID: " << taskid << ", Coordenada: (" << coords[0] << "," << coords[1] << ").\n";

    // VECINOS
    vecino[0] = vecino[1] = MPI_PROC_NULL;
    direction = 0;
    displasment = 1;
    MPI_Cart_shift(comm2D, direction, displasment, &vecino[0], &vecino[1]);
    cout << "Task ID: " << taskid << ", Vecinos: (" << vecino[0] << ", " << vecino[1] << ").\n";

    /* ----------------------------- DIVISION DOMINIO -------------------------------- */

    // EJE X (renglon):
    NN = floor(1.0 * NxG / ntasks);

    if(ntasks == 1) // Caso serial
        Nx = NxG;
    else 
        if(taskid == 0) // Proceso 0
            Nx = NN;
        else
            if(taskid == ntasks - 1) // Proceso n - 1
                Nx = NxG - NN * taskid + 2;
            else // Proceso 0 < i < ntasks - 1
                Nx = NN + 2;
    
    // EJE Y (columnas):
    Ny = NyG;

    /* ------------------------ COMIENZA TIEMPO -------------------------------------- */
    t_i = clock();

    /* ------------------------ RESERVA MEMORIA ----------------------------------------- 
    ---------- Tomamos un espacio mas pues el contador inicia en 1 --------------------*/

    x = new double [Nx + 1];
    y = new double [Ny + 1];
    xG = new double [NxG + 1];
    yG = new double [NyG + 1];

    u = new double* [Nx + 1];
    u_old = new double* [Nx + 1];

    for(int i = 1; i <= Nx; i++){
        u[i] = new double [Ny + 1];
        u_old[i] = new double [Ny + 1];
    }

    index_global = new int [Nx + 1];

    /* -------------------------------- INDICES ---------------------------------------*/

    if(ntasks == 1) // Caso serial
        for(int i = 1; i <= Nx; i++)
            index_global[i] = i;
    else
        if(taskid == 0) // Proceso 0
            for(int i = 1; i <= Nx; i++)
                index_global[i] = i;
        else // Proceso 0 < i <= ntasks
            for(int i = 1; i <= Nx; i++)
                index_global[i] = (taskid * NN) - 1 + (i - 1);

    cout << "Task ID: " << taskid << ", Nx Local: " << Nx << ", Indices globales: (" << index_global[1] << "," << index_global[Nx] << ").\n";

    /* ------------------------- DEFINICION TIPO DE DATO-------------------------------*/
    MPI_Datatype tipo_row;
    MPI_Type_vector(Ny, 1, 1, MPI_DOUBLE, &tipo_row);
    MPI_Type_commit(&tipo_row);

    /* ---------------------------- PUNTOS INICIALES --------------------------------- */

    // Puntos globales en x
    for(int i = 1; i <= NxG; i++)
        xG[i] = x_i + (i - 1) * dx;
    
    // Puntos globales y locales en y
    for(int i = 1; i <= NyG; i++)
        y[i] = yG[i] = y_i + (i - 1) * dy;

    // Puntos en x
    for(int i = 1; i <= Nx; i++)
        x[i] = xG[index_global[i]];

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
        
        // Comunicacion
        if(ntasks > 1){
            MPI_Sendrecv(&u[2][1], 1, tipo_row, vecino[0], etiqueta,
                        &u[Nx][1], 1, tipo_row, vecino[1], etiqueta,
                        comm2D, MPI_STATUS_IGNORE);

            MPI_Sendrecv(&u[Nx - 1][1], 1, tipo_row, vecino[1], etiqueta,
                        &u[1][1], 1, tipo_row, vecino[0], etiqueta,
                        comm2D, MPI_STATUS_IGNORE);
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

    // Rango correspondiente al proceso
    if(ntasks == 1){ // Caso serial
        it_i = 1;
        it_f = Nx;
    }else{
        if(taskid == 0){ // Proceso 0
            it_i = 1;
            it_f = Nx - 1;
        }else if(taskid == ntasks - 1){ // Proceso n - 1
            it_i = 2;
            it_f = Nx;
        }else{ // Proceso 0 < i <= ntasks
            it_i = 2;
            it_f = Nx - 1;
        }
    }

    sum = 0.0;
    for(int i = it_i; i <= it_f; i++)
        for(int j = 1; j <= Ny; j++)
            sum += u[i][j];

    // Reduccion sobre todos los procesos
    MPI_Allreduce(&sum, &suma_global, 1, MPI_DOUBLE, MPI_SUM, comm2D);
    sum = suma_global;

    cout << "TaskID = " << taskid << ", Suma total: " << sum << '\n';
    cout << "Tiempo: " << (t_f - t_i) / CLOCKS_PER_SEC << '\n';

    /* ------------------------- LIBERA MEMORIA -------------------------------------- */

    MPI_Type_free(&tipo_row); // Liberamos memoria del tipo de dato creado
    MPI_Finalize(); // Termina region paralela

    for(int i = 1; i <= Nx; i++){
        delete [] u[i];
        delete [] u_old[i];
    }
    delete [] x;
    delete [] y;
    delete [] xG;
    delete [] yG;
    delete [] u;
    delete [] u_old;
    delete [] index_global;

    return 0;
}