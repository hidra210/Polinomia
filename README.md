# Polinomia
**Práctica 4: Álgebra de Baldor o el val(d)or del álgebra**

## Descripción

Este proyecto consiste en la creación de un entorno educativo interactivo para operar con polinomios, inspirado en el libro "Álgebra de Baldor". Se han desarrollado funciones para trabajar con polinomios en Haskell y se ha implementado una interfaz gráfica en Python que genera preguntas de forma aleatoria al estilo de un juego.

### Contenido mínimo implementado (en Haskell)

- TAD para polinomios
- Visualización de polinomios legible (`Show`)
- Operaciones básicas:
  - Evaluación
  - Derivación
  - Integración
  - División con Ruffini
  - Factorización
  - Cálculo de raíces enteras

### Contenido adicional (herramienta educativa en Python)

- Interfaz con `tkinter` (`ruleta.py`)
- Generación de preguntas automáticas (`preguntas.py`, `respuesta.py`)
- Comunicación con el ejecutable Haskell mediante `subprocess`
- Preguntas con opciones múltiples tipo test

---

## Requisitos

### Haskell
- GHC (Glasgow Haskell Compiler), versión recomendada ≥ 8.0
- No requiere instalar librerías externas

### Python
- Python 3.x
- Librerías estándar: `tkinter`, `subprocess`, `random`, `sys`, `io`, `math`, `os`
- Librería externa:
  - `Pillow` para mostrar imágenes:
  
```bash
pip install --upgrade tkinter
```
```bash
pip install pillow
```

## Uso del programa

### 1. Compilar el ejecutable en Haskell

```bash
ghc -o polinomio PF_Practica4.hs
```
### 2. Ejecutar ruleta.py

```bash
python ruleta.py