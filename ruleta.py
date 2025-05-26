import tkinter as tk
import math
import preguntas
import tkinter.font as tkFont

# Operaciones disponibles para la ruleta
operaciones = [
    "Raíces",
    "Dividir",
    "Derivar",
    "Integrar",
    "Factorizar",
    "Evaluar"
]
colores = ["#FF8C42", "#FFD93D", "#6BCB77", "#4D96FF", "#DD4EBE", "#FF6B6B"]

# Tkinter setup
root = tk.Tk()
root.title("Polinomia")

# Fuente
default_font = tkFont.nametofont("TkDefaultFont")
default_font.configure(family="Segoe UI", size=12)  # Usa una fuente que soporte Unicode

# Maximize without hiding Windows taskbar
root.update_idletasks()
screen_width = root.winfo_screenwidth()
screen_height = root.winfo_screenheight()
root.geometry(f"{screen_width}x{screen_height - 40}+0+0")
root.configure(bg="#F4F4F4")
root.resizable(False, False)

# Título
titulo = tk.Label(root, text="🎓 Polinomia 🎓", font=("Helvetica", 20, "bold"),
                  bg="#F4F4F4", fg="#333")
titulo.pack(pady=10)

# Frame para botones (arriba de la ruleta)
frame_botones = tk.Frame(root, bg="#F4F4F4")
frame_botones.pack(pady=25)  # aún más arriba

boton_girar = tk.Button(frame_botones, text="🎡 Girar Ruleta", font=("Helvetica", 16, "bold"),
                        bg="#4D96FF", fg="white", activebackground="#3C7DD9",
                        padx=30, pady=15, command=lambda: girar())
boton_girar.pack(side="left", padx=15)

boton_detener = tk.Button(frame_botones, text="✋ Detener", font=("Helvetica", 16, "bold"),
                          bg="#FF6B6B", fg="white", activebackground="#E04A4A",
                          padx=30, pady=15, command=lambda: detener_giro(), state="disabled")
boton_detener.pack(side="left", padx=15)

# Canvas más grande para la ruleta (debajo de los botones)
canvas_size = 600
canvas = tk.Canvas(root, width=canvas_size, height=canvas_size, bg="#F4F4F4", highlightthickness=0)
canvas.pack(pady=0)

# Estado
secciones = len(operaciones)
angulo_seccion = 360 / secciones
girando = False
detener = False
angulo_flecha = 0
velocidad = 10

def dibujar_ruleta(angulo):
    canvas.delete("all")
    centro_x, centro_y = canvas_size // 2, canvas_size // 2
    radio = 250  # radio más grande

    for i in range(secciones):
        start_angle = i * angulo_seccion
        color = colores[i]
        texto = operaciones[i]

        canvas.create_arc(centro_x - radio, centro_y - radio,
                          centro_x + radio, centro_y + radio,
                          start=start_angle, extent=angulo_seccion,
                          fill=color, outline="white", width=2)

        angle = math.radians(start_angle + angulo_seccion / 2)
        text_x = centro_x + (radio / 1.6) * math.cos(angle)
        text_y = centro_y - (radio / 1.6) * math.sin(angle)

        canvas.create_text(text_x, text_y, text=texto,
                           fill="white", font=("Helvetica", 14, "bold"),
                           justify="center", anchor="center")

    # Flecha más grande y proporcional
    flecha_x1 = centro_x + (radio - 20) * math.cos(math.radians(angulo))
    flecha_y1 = centro_y - (radio - 20) * math.sin(math.radians(angulo))
    flecha_x2 = centro_x + (radio + 40) * math.cos(math.radians(angulo + 5))
    flecha_y2 = centro_y - (radio + 40) * math.sin(math.radians(angulo + 5))
    flecha_x3 = centro_x + (radio + 40) * math.cos(math.radians(angulo - 5))
    flecha_y3 = centro_y - (radio + 40) * math.sin(math.radians(angulo - 5))

    canvas.create_polygon([flecha_x1, flecha_y1, flecha_x2, flecha_y2, flecha_x3, flecha_y3],
                          fill="red", outline="black")

def animar_ruleta():
    global angulo_flecha, velocidad, girando, detener

    if not girando:
        return

    angulo_flecha = (angulo_flecha + velocidad) % 360
    dibujar_ruleta(angulo_flecha)

    if detener:
        velocidad = max(0.3, velocidad - 0.15)
        if velocidad <= 0.5:
            girando = False
            detener = False
            velocidad = 10
            determinar_operacion()
            boton_girar.config(state="normal")
            boton_detener.config(state="disabled")
            return

    root.after(16, animar_ruleta)  # ~60 FPS

def girar():
    global girando, detener, velocidad
    if girando:
        return
    velocidad = 10
    detener = False
    girando = True
    boton_girar.config(state="disabled")
    boton_detener.config(state="normal")
    animar_ruleta()

def detener_giro():
    global detener
    detener = True

def determinar_operacion():
    # Ajustar ángulo porque la flecha apunta a 90°
    angulo_referenciado = (angulo_flecha - 30) % 360
    
    # Calcular índice sumando la mitad del sector para centrar
    index = int(((angulo_referenciado + angulo_seccion / 2) % 360) // angulo_seccion)
    operacion = operaciones[index]

    preguntas.mostrar_pregunta(operacion, root)

# Inicial
dibujar_ruleta(angulo_seccion / 2)
root.mainloop()