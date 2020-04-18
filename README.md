# IA-Clips

<details>
<summary><big><b>Laboratorio 1</b></big></summary>

<br>

La documentación se está realizando [aquí](https://www.overleaf.com/7918422837rtzyrsfxvnhb).

### Tarea 1: Aspiradora

- En cada habitación la “suciedad” puede aparecer más de una vez.  Ejecutar el algoritmo de búsqueda exhaustiva a partir de un estado inicial con más apariciones de suciedad y guardar el fichero de la sesión.

- Implementar los estados prohibidos y volver a ejecutar.

- Dibujar el árbol de búsqueda con y sin estados prohibidos.

### Tarea 2: Recipientes

- Implementar la búsqueda en anchura.

- Revisar la implementación de los estados prohibidos.

- Ejecutar la búqueda en profundidad y la búsqueda en anchura con y sin estados prohibidos guardando las sesiones .

### Tarea 3: Puzzle NxN

- Representar el problema del Puzzle NxN e implementarlo.

- Implementa la función __*h(puzzle) = número_de_fichas*__ que no están en su sitio con respecto a la configuración objetivo de la figura.

- Implementar la búsqueda *British Museum* y aplicarla para __*n=4*__ con la función anterior.

```clips
(BM
    <estado inicial>
    <dirección> ; profundidad o anchura
    <función> ; función h
    <optimización> ; maximizar o minimizar
    <visitados> ; (opcional) lista que evita visitar dos veces el mismo estado
)
```

- Realiza una ejecución guardando la sesión.

</details>

<details>
<summary><big><b>Laboratorio 2</b></big></summary>

<br>

La documentación se está realizando [aquí](https://www.overleaf.com/9429211598kpghjntzzjbc).

### Tarea 1: Aspiradora

Describe y explica las funciones nuevas o que hayan cambiado con respecto al laboratorio 1.

### Tarea 2: Recipientes

- Completa el algoritmo de __*busqueda-informada*__ para que pasando los parámetros __*g*__ y __*h*__ ejecute la búsqueda correspondiente a ![equation](https://latex.codecogs.com/gif.latex?f&space;=&space;g&space;&plus;&space;h).

- Incluye otro parámetro para indicar dónde queremos guardar la ejecución: si que de por pantalla (__*t*__) o en un archivo (__*nombre lógico*__). En el último caso debe preguntar al usuario el nombre del fichero para guardar la ejecución.  Define los mensajes adecuados para que se entienda el algoritmo.

- Soluciona el [examen](https://github.com/kaecius/IA-Clips/blob/master/Lab2/Laboratorio%202%20Busqueda%20heur%C3%ADstica/Examen%20eval%201-%2018-19-sin%20erratas.docx) de la primera evaluación continua del curso 2018-2019.

</details>

<details>
<summary><big><b>Pecera</b></big></summary>

<br>

La documentación se está realizando [aquí](https://www.overleaf.com/4457388223dxhxgyxqrdby)

</details>

## Autores

- [Xabier Lahuerta Vázquez](https://github.com/Xabilahu)
- [Daniel Cañadillas Patricio](https://github.com/kaecius)
- [Ainhoa Serrano Guerrero](https://github.com/sgainhoa)
