import tkinter as tk
from tkinter import messagebox
import respuesta
import random
# from ruleta import contador

lista_juego = [0]

# Colores de los botones de cada operación
OPERACIONES = [
    "Raíces",
    "Dividir",
    "Derivar",
    "Integrar",
    "Factorizar",
    "Evaluar"
]
COLORES = ["#FF8C42", "#FFE816", "#6BCB77", "#4D96FF", "#DD4EBE", "#FF6B6B"]
COLOR_MAPA = dict(zip(OPERACIONES, COLORES))

# Respuestas incorrectas predefinidas por tipo de operación
RESPUESTAS_INCORRECTAS = {
    "Derivar": [
        "3x² + 2x + 1", "4x³ - 3x", "2x + 5", "6x² - 2x + 3",
        "x³ + 4x² - 2", "5x⁴ - 3x² + 7", "2x - 1", "4x³ + 6x² - 1",
        "x² - 4x + 4", "7x³ + x - 5", "6x⁴ - x² + 2x", "3x² + x", 
        "5x³ + 2x² + 4", "2x² - 5", "8x - 3", "x³ + 6x"
    ],
    "Integrar": [
        "x³ + x²", "0.5x⁴ - x²", "2x³ + 3x", "x²/2 + 3x", "x⁴/4 - x³/3",
        "3x² + 5x", "x³/3 + 2x²", "4x³ - 2x² + x", "x⁵/5 + x³/3",
        "0.25x⁴ - x² + 2x", "x² + 3x", "x³ + x²", "x⁴ - 2x³ + x", 
        "x³ - x²", "3x⁴ - 2x²", "1.5x³ + x² + x"
    ],
    "Dividir": [
        "Cociente: x² + 2x + 1\nResto: 0.0", "Cociente: 2x - 3\nResto: 1.0",
        "Cociente: x³ - x\nResto: 2.0", "Cociente: x + 4\nResto: -1.0",
        "Cociente: x² - 2x + 3\nResto: 0.0", "Cociente: 3x + 2\nResto: 5.0",
        "Cociente: x² + x - 1\nResto: 3.0", "Cociente: 2x² - x + 4\nResto: -2.0",
        "Cociente: x² + 4x + 4\nResto: 1.0", "Cociente: x³ + x²\nResto: -2.0",
        "Cociente: x + 1\nResto: 0.5", "Cociente: 4x² + x - 3\nResto: 2.5",
        "Cociente: x² - 5x + 6\nResto: 0", "Cociente: 2x + 1\nResto: -1.0",
        "Cociente: x³ - x² + 2x\nResto: 1", "Cociente: x² + 1\nResto: 0.0"
    ]
}

def obtener_respuestas_incorrectas(operacion, respuesta_correcta, grado=0):
    if operacion == "Evaluar":
        incorrectas = set()
        intentos = 0
        respuesta_correcta_str = str(respuesta_correcta)
        while len(incorrectas) < 3 and intentos < 20:
            num = random.randint(-100, 100)
            opcion = f"{float(num):.1f}"
            if opcion != respuesta_correcta_str:
                incorrectas.add(opcion)
            intentos += 1
        opciones = list(incorrectas)
        opciones.insert(random.randint(0, 3), respuesta_correcta_str)
        return opciones


    if operacion == "Raíces":
        incorrectas = set()
        intentos = 0

        try:
            # Asegurarse de que la respuesta correcta es una lista de enteros ordenada
            raices_correctas = sorted(int(float(r)) for r in eval(str(respuesta_correcta)))
            respuesta_correcta_str = str(raices_correctas)
        except Exception as e:
            print("Error procesando raíces correctas:", e)
            return [str(respuesta_correcta), "[0]", "[1]", "[2]"]

        grado = len(raices_correctas)

        while len(incorrectas) < 3 and intentos < 100:
            raices = sorted(random.sample(range(-9, 10), grado))
            propuesta = str(raices)
            if propuesta != respuesta_correcta_str:
                incorrectas.add(propuesta)
            intentos += 1

        opciones = list(incorrectas)
        opciones.insert(random.randint(0, 3), respuesta_correcta_str)
        return opciones


    if operacion == "Factorizar":
        try:
            num_factores = grado
            incorrectas = set()
            intentos = 0
            while len(incorrectas) < 3 and intentos < 50:
                raices = list({random.randint(-6, 6) for _ in range(num_factores)})
                if len(raices) < num_factores:
                    continue  # Evitar repetidos

                tiene_x = 0 in raices
                otras_raices = sorted(r for r in raices if r != 0)

                factores = []
                if tiene_x:
                    factores.append("x")
                for r in otras_raices:
                    if r > 0:
                        factores.append(f"(x - {r})")
                    else:
                        factores.append(f"(x + {abs(r)})")

                propuesta = "".join(factores)
                if propuesta != respuesta_correcta:
                    incorrectas.add(propuesta)
                intentos += 1

            opciones = list(incorrectas)
            opciones.insert(random.randint(0, 3), respuesta_correcta)
            return opciones

        except Exception as e:
            print("Error generando factorizaciones falsas:", e)

    # Default (Derivar, Integrar, Dividir)
    pool = RESPUESTAS_INCORRECTAS.get(operacion, [])
    if len(pool) < 3:
        pool += [f"Opción incorrecta {i+1}" for i in range(3 - len(pool))]

    incorrectas = []
    intentos = 0
    while len(incorrectas) < 3 and intentos < 10:
        candidata = random.choice(pool)
        if candidata != respuesta_correcta and candidata not in incorrectas:
            incorrectas.append(candidata)
        intentos += 1

    opciones = incorrectas.copy()
    opciones.insert(random.randint(0, 3), respuesta_correcta)
    return opciones



def centrar_ventana(ventana, ancho, alto):
    ventana.update_idletasks()
    x = (ventana.winfo_screenwidth() - ancho) // 2
    y = (ventana.winfo_screenheight() - alto) // 2
    ventana.geometry(f"{ancho}x{alto}+{x}+{y}")

def mostrar_pregunta(operacion, parent_window):
    resultados = respuesta.generar_y_ejecutar(operacion)
    
    if not resultados:
        messagebox.showerror("Error", "No se pudo generar la pregunta")
        return

    ventana = tk.Toplevel(parent_window)
    ventana.title(f"Pregunta: {operacion}")
    centrar_ventana(ventana, 800, 600)
    ventana.configure(bg="#F4F4F4")

    frame_principal = tk.Frame(ventana, bg="#F4F4F4")
    frame_principal.pack(expand=True, fill='both', padx=20, pady=20)

    tk.Label(frame_principal, text=f"Polinomio: {resultados['polinomio']}", 
             font=("Helvetica", 14), bg="#F4F4F4").pack(pady=15)

    if operacion == "Raíces":
        pregunta = "Encuentra las raíces del polinomio:"
        raices_crudas = resultados.get('raices', [])
        # Normaliza como lista de enteros ordenados, convertida en string
        try:
            raices_enteras = sorted(int(float(r)) for r in eval(str(raices_crudas)))
            respuesta_correcta = str(raices_enteras)
        except Exception as e:
            print("Error formateando raíces:", e)
            respuesta_correcta = str(raices_crudas)
    elif operacion == "Dividir":
        divisor = resultados.get('divisor', 0)
        if divisor < 0:
            divisor_str = f"+ {abs(int(divisor))}" if divisor == int(divisor) else f"+ {abs(divisor)}"
        else:
            divisor_str = f"- {int(divisor)}" if divisor == int(divisor) else f"- {divisor}"

        pregunta = f"Divide el polinomio por (x {divisor_str}) usando Ruffini:"

        respuesta_correcta = (
            f"Cociente: {resultados.get('cociente', '?')}\n"
            f"Resto: {resultados.get('resto', '?')}"
        )
    elif operacion == "Derivar":
        pregunta = "Deriva el polinomio:"
        respuesta_correcta = resultados.get('derivada', '?')
    elif operacion == "Integrar":
        pregunta = "Integra el polinomio:"
        respuesta_correcta = resultados.get('integral', '?')
    elif operacion == "Factorizar":
        pregunta = "Factoriza el polinomio:"
        respuesta_correcta = resultados.get('factorizacion', '?')
    elif operacion == "Evaluar":
        x = resultados.get('x', 0)
        pregunta = f"Evalúa el polinomio en x = {x}:"
        respuesta_correcta = resultados.get('evaluacion', '?')
    else:
        pregunta = "¿Cuál es el resultado?"
        respuesta_correcta = "?"

    tk.Label(frame_principal, text=pregunta, font=("Helvetica", 16, "bold"), 
             bg="#F4F4F4").pack(pady=20)

    frame_respuestas = tk.Frame(frame_principal, bg="#F4F4F4")
    frame_respuestas.pack(pady=30)

    grado = resultados.get("grado", 0)
    respuestas = obtener_respuestas_incorrectas(operacion, respuesta_correcta, grado)

    color_base = COLOR_MAPA.get(operacion, "#CCCCCC")

    for i, opcion in enumerate(respuestas):
        es_correcta = (opcion == respuesta_correcta)
        btn_color = color_base
        comando = (lambda r=opcion: mostrar_respuesta_correcta(r, ventana, color_base)) if es_correcta else (
                lambda v=ventana: mostrar_respuesta_incorrecta(v, color_base))

        btn = tk.Button(
            frame_respuestas, 
            text=opcion,
            font=("Helvetica", 14),
            bg=btn_color,
            fg="white",
            padx=20,
            pady=10,
            command=comando
        )
        row, col = divmod(i, 2)
        btn.grid(row=row, column=col, padx=15, pady=15, sticky="nsew")


    for i in range(2):
        frame_respuestas.grid_rowconfigure(i, weight=1)
        frame_respuestas.grid_columnconfigure(i, weight=1)

    cerrar_ventana(frame_principal, color_base, ventana).pack(pady=20)
    

def cerrar_ventana(frame_principal, color_base, ventana):
    from ruleta import contador
    contador(lista_juego[0])
    return tk.Button(frame_principal, text="Cerrar", font=("Helvetica", 14),
              bg=color_base, fg="white",
              command=ventana.destroy)


def mostrar_respuesta_correcta(respuesta, ventana, color_base):
    lista_juego[0] += 1
    popup = tk.Toplevel(ventana)
    popup.title("¡Respuesta Correcta!")
    centrar_ventana(popup, 600, 350)
    popup.configure(bg="#F4F4F4")

    tk.Label(popup, text="¡Correcto!", 
             font=("Helvetica", 26, "bold"), 
             bg="#F4F4F4", fg="#4CD15E").pack(pady=20)

    tk.Label(popup, text="La respuesta es:", 
             font=("Helvetica", 18), 
             bg="#F4F4F4").pack(pady=10)

    tk.Label(popup, text=respuesta, 
             font=("Helvetica", 16, "bold"), 
             bg="#F4F4F4", fg="#333").pack(pady=20)

    cerrar_ventana(popup, color_base, popup).pack(pady=20)

def mostrar_respuesta_incorrecta(ventana, color_base):
    lista_juego[0] = 0
    popup = tk.Toplevel(ventana)
    popup.title("¡Respuesta Incorrecta!")
    centrar_ventana(popup, 600, 350)
    popup.configure(bg="#F4F4F4")

    tk.Label(popup, text="¡Incorrecto!", 
             font=("Helvetica", 24, "bold"), 
             bg="#F4F4F4", fg="#FA4343").pack(pady=20)

    tk.Label(popup, text="Intenta de nuevo.", 
             font=("Helvetica", 16), 
             bg="#F4F4F4").pack(pady=10)
    
    cerrar_ventana(popup, color_base, popup).pack(pady=20)

if __name__ == "__main__":
    root = tk.Tk()
    root.withdraw()
    mostrar_pregunta("Derivar", root)
    root.mainloop()
