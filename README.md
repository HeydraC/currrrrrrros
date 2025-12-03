# Pregunta 1
Para initialBoard se implementaron 2 funciones más.

positions: Retorna una lista con las posiciones que ocupa un vehículo en el tablero.

verify: Recibe un vehículo y el tablero sin ese vehículo para verificar si está en una posición válida. Para esto primero revisa que el vehículo está dentro del tablero y en caso de estarlo se revisa si comparte alguna posición con otro vehículo en el tablero.

Por último initialBoard chequea vehículo por vehículo si estos están en una posición válida respecto a los demás del tablero.
# Pregunta 2
Para isValidMove move se usaron 2 funciones auxiliares.

iteRate: Retorna el vehículo k de un tablero.

move: Recibe un vehículo y lo desplaza en base a su orientación.

Por último isValidMove genera los movimientos intermedios del movimiento que se quiere hacer y comprueba que en todos estos se encuentre en una posición válida.

# Pregunta 3
En moveVehicle se recorre la lista y se usa move para hacer el desplazamiento buscado.

(No se usa iteRate ya que igualmente se debe mantener el orden del tablero)

# Pregunta 4
Para solveRushHour se implementaron 3 funciones que realizan el recorrido por anchura.

getNeighbors: Recibe un tablero y obtiene todos los tableros válidos a los que se puede llegar desde el mismo. Se implementó reusando las funciones isValidMove y moveVehicle de las preguntas anteriores, aplicándolas para crear la lista por comprensión que contiene los resultados.

isSolved: Revisa si el primer vehículo principal del tablero llegó a la posición buscada.

bfs: Se encarga de hacer el recorrido para obtener la cantidad de movimientos mínima usando las 2 funciones ya nombradas, recibe una lista de pares (tablero, recorrido) que será usada como cola de tableros posibles. En cada llamada se revisa si ya se resolvió el tablero para retornar la lista con el recorrido (incluyendo el tablero actual) invertida y la cantidad de movimientos que se necesitaron, luego se revisa si el tablero ya fue visitado para saltarlo y, en caso de no caer en las 2 guardas anteriores, se hace la llamada recursiva agregando a la cola de tableros los obtenidos con getNeighbors con su recorrido correspondiente y agregando el tablero actual a la lista de tableros visitados.

Por último, solveRushHour tiene la primera llamada a bfs con la cola conteniendo al tablero inicial sin ningún recorrido y una lista vacía de tableros visitados.

# deriving(Show, Eq)
La sentencia usada para definir la estructura de datos Orientation, funciona para generar una declaración estándar de la clase especificada (En este caso Show y Eq), ahorrando el trabajo de hacer una definición de las mismas para el nuevo tipo creado. En este caso hace posible que se impriman y comparen orientaciones.
