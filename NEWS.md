# plotannotate 1.3-0 (2025-06-17)

- improved the circle drawing code, so that either the x- or y-axis distance properly determines the circle radius

- added circle and ellipse drawing modes that work like freehand drawing but the nearest circle/ellipse corresponding to the input coordinates is drawn (makes use of the `conicfit` package)

- added a smooth mode for freehand drawing

- much faster undo for freehand drawing

- can also click on the colors to select them

# plotannotate 1.2-0 (2025-05-22)

- minor documentation updates

- use `on.exit()` as needed to reset `par()` changes

# plotannotate 1.0-0 (2025-05-19)

- first release
