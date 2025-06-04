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
### 2. Ejecutar polinomio
```bash
./polinomio
```
### 3. Escribir las entradas del programa: 
1) Primera línea: Coeficientes del polinomio, separados por espacios (de menor a mayor grado).
2) Segunda línea: Nombre de la operación (raices, dividir, derivar, integrar, factorizar, evaluar).
3) Tercera línea (Opcional): Un número si se requiere (por ejemplo, valor de x para evaluar o dividir).

- Ejemplo de uso 1: Evaluar 𝑝 ( 𝑥 ) = 1 + 2𝑥 + 3𝑥² en 𝑥 = 2 
```bash
1 2 3
evaluar
2
```

- Ejemplo de uso 2: Derivar 𝑝 ( 𝑥 ) = 1 + 2𝑥 + 3𝑥²
```bash
1 2 3
derivar
```
### 4. Ejecutar ruleta.py para utilizar la IU

```bash
python ruleta.py