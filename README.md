# IA-Clips

La documentación se está realizando [aquí](https://www.overleaf.com/7918422837rtzyrsfxvnhb).

## Tarea 1: Aspiradora

- En cada habitación la “suciedad” puede aparecer más de una vez.  Ejecutar el algoritmo de búsqueda exhaustiva a partir de un estado inicial con más apariciones de suciedad y guardar el fichero de la sesión.

- Implementar los estados prohibidos y volver a ejecutar.

- Dibujar el árbol de búsqueda con y sin estados prohibidos.

## Tarea 2: Recipientes

- Implementar la búsqueda en anchura.

- Revisar la implementación de los estados prohibidos.

- Ejecutar la búqueda en profundidad y la búsqueda en anchura con y sin estados prohibidos guardando las sesiones .

## Tarea 3: Puzzle NxN

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

## Autores

- [Xabier Lahuerta Vázquez](https://github.com/Xabilahu)
- [Daniel Cañadillas Patricio](https://github.com/kaecius)
- [Ainhoa Serrano Guerrero](https://github.com/sgainhoa)
