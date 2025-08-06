import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import glob
import os

# === Parametri ===
time_values = [700] #range(0, 10000)  # Modifica questo intervallo in base ai tuoi dati
folder = "."  # Cartella con i file .dat

# === Setup iniziale ===
fig, ax = plt.subplots(figsize=(8, 6))
im = ax.imshow(np.zeros((10, 10)), cmap='bone', origin='lower')
cbar = plt.colorbar(im, ax=ax, label="Valore")
ax.set_title("Campo globale da bounding boxes")
ax.set_xlabel("i (global)")
ax.set_ylabel("j (global)")
ax.grid(True, alpha=0.3)

def load_global_field(time):
    time_str = f"{time:04d}"
    field_files = sorted(glob.glob(os.path.join(folder, f"field_{time_str}_*.dat")))

    all_blocks = []

    for field_file in field_files:
        basename = os.path.basename(field_file)
        parts = basename.split('_')
        rank_str = parts[2].split('.')[0]
        rank = int(rank_str)

        coords_file = os.path.join(folder, f"coords_{rank:02d}.dat")

        try:
            imin, imax, jmin, jmax = np.fromfile(coords_file, dtype=np.int32)
            nx = imax - imin + 1
            ny = jmax - jmin + 1

            field = np.fromfile(field_file, dtype=np.int8).reshape((nx, ny), order='F')

            all_blocks.append({
                'field': field,
                'imin': imin,
                'imax': imax,
                'jmin': jmin,
                'jmax': jmax
            })

        except Exception as e:
            print(f"Errore con {field_file}: {e}")
            continue

    if not all_blocks:
        return None

    imax_total = max(b['imax'] for b in all_blocks) + 1
    jmax_total = max(b['jmax'] for b in all_blocks) + 1

    global_field = np.full((imax_total, jmax_total), -1, dtype=np.int8)

    for block in all_blocks:
        f = block['field']
        i0, j0 = block['imin'], block['jmin']
        nx, ny = f.shape
        global_field[i0:i0+nx, j0:j0+ny] = f

    return global_field

# === Funzione per aggiornare il frame ===
def update(frame_idx):
    time = time_values[frame_idx]
    field = load_global_field(time)
    if field is not None:
        im.set_data(field.T)
        im.set_extent([0, field.shape[0], 0, field.shape[1]])
        im.set_clim(np.min(field), np.max(field))  # Opzionale: scala colori dinamica
        ax.set_title(f"Campo globale - Tempo {time:04d}")
    return [im]

# === Crea animazione ===
ani = animation.FuncAnimation(fig, update, frames=len(time_values), interval=10, blit=False)

# === Mostra ===
plt.tight_layout()
plt.show()
