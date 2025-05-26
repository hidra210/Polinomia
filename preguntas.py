import tkinter as tk
from tkinter import messagebox
import respuesta
import random

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
        pregunta = "Encuentra una raíz entera del polinomio:"
        respuesta_correcta = resultados.get('raices', 'No se encontraron raíces enteras')
    elif operacion == "Dividir":
        divisor = resultados.get('divisor', 0)
        pregunta = f"Divide el polinomio por (x - {divisor}) usando Ruffini:"
        respuesta_correcta = (
            f"Divisor: (x - {divisor})\n"
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

    tk.Label(frame_principal, text=pregunta, font=("Helvetica", 16, "bold"), 
             bg="#F4F4F4").pack(pady=20)

    frame_respuestas = tk.Frame(frame_principal, bg="#F4F4F4")
    frame_respuestas.pack(pady=30)

    respuestas = [respuesta_correcta]

    if operacion == "Raíces":
        respuestas.extend([str(random.randint(-10, 10)) for _ in range(3)])
    elif operacion in ["Derivar", "Integrar"]:
        respuestas += [resultados['polinomio'], "0", "1"]
    elif operacion == "Evaluar":
        try:
            valor_correcto = float(resultados['evaluacion'])
            incorrectas = set()

            while len(incorrectas) < 3:
                modificador = random.uniform(1, 3)
                tipo = random.choice(["+", "-", "*"])
                if tipo == "+":
                    valor = round(valor_correcto + modificador, 2)
                elif tipo == "-":
                    valor = round(valor_correcto - modificador, 2)
                else:
                    valor = round(valor_correcto * (1 + modificador / 10), 2)

                if round(valor, 2) != round(valor_correcto, 2):
                    incorrectas.add(str(valor))

            respuestas += list(incorrectas)

        except (ValueError, KeyError):
            respuestas += ["Opción incorrecta 1", "Opción incorrecta 2", "Opción incorrecta 3"]
    else:
        respuestas += ["Opción incorrecta 1", "Opción incorrecta 2", "Opción incorrecta 3"]

    random.shuffle(respuestas)

    botones_respuesta = []
    for i, opcion in enumerate(respuestas):
        if opcion == respuesta_correcta:
            btn = tk.Button(
                frame_respuestas, 
                text=f"Respuesta: {opcion}", 
                font=("Helvetica", 14),
                bg="#6BCB77",
                fg="white",
                padx=20,
                pady=10,
                command=lambda r=opcion: mostrar_respuesta_correcta(r, ventana)
            )
        else:
            btn = tk.Button(
                frame_respuestas, 
                text="Respuesta incorrecta", 
                font=("Helvetica", 14),
                bg="#FF6B6B",
                fg="white",
                padx=20,
                pady=10,
                command=lambda v=ventana: mostrar_respuesta_incorrecta(v)
            )
        botones_respuesta.append(btn)

    for i, btn in enumerate(botones_respuesta):
        row = i // 2
        col = i % 2
        btn.grid(row=row, column=col, padx=15, pady=15, sticky="nsew")

    frame_respuestas.grid_rowconfigure(0, weight=1)
    frame_respuestas.grid_rowconfigure(1, weight=1)
    frame_respuestas.grid_columnconfigure(0, weight=1)
    frame_respuestas.grid_columnconfigure(1, weight=1)

    tk.Button(frame_principal, text="Cerrar", font=("Helvetica", 14),
              bg="#4D96FF", fg="white",
              command=ventana.destroy).pack(pady=20)

def mostrar_respuesta_correcta(respuesta, ventana):
    popup = tk.Toplevel(ventana)
    popup.title("¡Respuesta Correcta!")
    centrar_ventana(popup, 600, 350)
    popup.configure(bg="#F4F4F4")

    tk.Label(popup, text="¡Correcto!", 
             font=("Helvetica", 26, "bold"), 
             bg="#F4F4F4", fg="#6BCB77").pack(pady=20)

    tk.Label(popup, text="La respuesta es:", 
             font=("Helvetica", 18), 
             bg="#F4F4F4").pack(pady=10)

    tk.Label(popup, text=respuesta, 
             font=("Helvetica", 16, "bold"), 
             bg="#F4F4F4", fg="#333").pack(pady=20)

    tk.Button(popup, text="Cerrar", font=("Helvetica", 14),
              bg="#4D96FF", fg="white",
              command=popup.destroy).pack(pady=20)

def mostrar_respuesta_incorrecta(ventana):
    popup = tk.Toplevel(ventana)
    popup.title("¡Respuesta Incorrecta!")
    centrar_ventana(popup, 600, 350)
    popup.configure(bg="#F4F4F4")

    tk.Label(popup, text="¡Incorrecto!", 
             font=("Helvetica", 24, "bold"), 
             bg="#F4F4F4", fg="#FF6B6B").pack(pady=20)

    tk.Label(popup, text="Intenta de nuevo.", 
             font=("Helvetica", 16), 
             bg="#F4F4F4").pack(pady=10)

    tk.Button(popup, text="Cerrar", font=("Helvetica", 14),
              bg="#4D96FF", fg="white",
              command=popup.destroy).pack(pady=20)

if __name__ == "__main__":
    root = tk.Tk()
    root.withdraw()
    mostrar_pregunta("Derivar", root)
    root.mainloop()
