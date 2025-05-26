import random
import subprocess
import sys
import io

# Configuración global UTF-8
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8', errors='replace')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8', errors='replace')

def multiplicar_polinomios(p1, p2):
    grado1 = len(p1) - 1
    grado2 = len(p2) - 1
    resultado = [0] * (grado1 + grado2 + 1)
    for i in range(len(p1)):
        for j in range(len(p2)):
            resultado[i + j] += p1[i] * p2[j]
    return resultado

def generar_polinomio(max_grado=4, raiz_min=-5, raiz_max=5):
    """Genera un polinomio con raíces enteras, factores (x - r) reales, coef principal = 1."""
    grado = random.randint(2, max_grado)
    raices = [random.randint(raiz_min, raiz_max) for _ in range(grado)]
    polinomio = [1]  # coeficientes de menor a mayor grado
    for r in raices:
        factor = [-r, 1]  # esto representa (x - r) en orden: constante, x
        polinomio = multiplicar_polinomios(polinomio, factor)
    return polinomio

def ejecutar_haskell(coefs, operacion, x=None):
    proceso = subprocess.Popen(
        ['./polinomio'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding='utf-8',
        bufsize=1
    )
    
    entrada = ' '.join(map(str, coefs)) + '\n'
    entrada += operacion + '\n'
    if x is not None:
        entrada += str(x) + '\n'
    
    salida, err = proceso.communicate(entrada)

    if err:
        print("Error:", err)
        return None
    
    resultados = {'polinomio': ''}
    for linea in salida.split('\n'):
        if 'Polinomio p:' in linea:
            resultados['polinomio'] = linea.split(':', 1)[1].strip()
        elif 'Raices enteras:' in linea:
            resultados['raices'] = linea.split(':', 1)[1].strip()
        elif 'Factorizar:' in linea:
            resultados['factorizacion'] = linea.split(':', 1)[1].strip()
        elif 'Divisor usado:' in linea:
            try:
                resultados['divisor'] = float(linea.split(':', 1)[1].strip())
            except:
                resultados['divisor'] = 0
        elif 'Cociente:' in linea:
            resultados['cociente'] = linea.split(':', 1)[1].strip()
        elif 'Resto:' in linea:
            resultados['resto'] = linea.split(':', 1)[1].strip()
        elif 'Derivada:' in linea:
            resultados['derivada'] = linea.split(':', 1)[1].strip()
        elif 'Integral:' in linea:
            resultados['integral'] = linea.split(':', 1)[1].strip()
        elif linea.startswith('Evaluar en x='):
            idx = linea.rfind(':')
            if idx != -1 and idx + 1 < len(linea):
                resultados['evaluacion'] = linea[idx+1:].strip()
        elif 'Grado:' in linea:
            try:
                resultados['grado'] = int(linea.split(':', 1)[1].strip())
            except:
                resultados['grado'] = 0


    return resultados

def generar_y_ejecutar(operacion):
    coefs = generar_polinomio()
    x = random.randint(-5, 5) if operacion in ["Evaluar", "Raíces", "Dividir"] else None
    
    # Mapear operaciones a comandos Haskell
    op_map = {
        "Raíces": "raices",
        "Dividir": "dividir",
        "Derivar": "derivar",
        "Integrar": "integrar",
        "Factorizar": "factorizar",
        "Evaluar": "evaluar"
    }
    
    resultados = ejecutar_haskell(coefs, op_map[operacion], x)
    
    if not resultados:
        return None
    
    # Añadir información adicional que pueda necesitar la interfaz
    if operacion in ["Dividir", "Evaluar"]:
        resultados['x'] = x
    
    return resultados