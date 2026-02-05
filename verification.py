import matplotlib.pyplot as plt
import re
import glob
import subprocess


def read_flux(filepath):
    """Read flux data from file, returning positions and flux values."""
    positions = []
    fluxes = []
    with open(filepath) as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            if len(parts) >= 2:
                positions.append(float(parts[0]))
                fluxes.append(float(parts[1]))
    return positions, fluxes


def add_material_annotations(ax):
    """Add material region boundary lines to a plot."""
    ax.axvline(x=2.0, color="gray", linestyle=":", alpha=0.5)
    ax.axvline(x=3.0, color="gray", linestyle=":", alpha=0.5)
    ax.axvline(x=5.0, color="gray", linestyle=":", alpha=0.5)

# Run the tests and extract K_EFF values for snes
subprocess.run(["make", "clobber"])
subprocess.run(["make", "tests"])

# Extract K_EFF values for snes
with open("/tmp/keffs", "w", encoding="utf-8") as f_out:
    for file in sorted(glob.glob("qa/*.outs")):
        with open(file, "r", encoding="utf-8") as f_in:
            for line in f_in:
                if "K EFFECTIVE" in line:
                    match = re.search(r"K[ ]*EFFECTIVE[ ]*=[ ]*([0-9.]+)", line)
                    if match:
                        f_out.write(match.group(1) + "\n")
                    else:
                        print(f"No match found in line: {line}")

# Run the tests and extract K_EFF values for snel
subprocess.run(["make", "clean"])
subprocess.run(["make", "testl"])
# Extract K_EFF values for snel
with open("/tmp/keffl", "w", encoding="utf-8") as f_out:
    for file in sorted(glob.glob("qa/*.outl")):
        with open(file, "r", encoding="utf-8") as f_in:
            for line in f_in:
                if "K EFFECTIVE" in line:
                    match = re.search(r"K[ ]*EFFECTIVE[ ]*=[ ]*([0-9.]+)", line)
                    if match:
                        f_out.write(match.group(1) + "\n")
                    else:
                        print(f"No match found in line: {line}")

# Read the K_EFF values from the files
with open("/tmp/keffs", "r", encoding="utf-8") as f:
    keffs = f.read()
with open("/tmp/keffl", "r", encoding="utf-8") as f:
    keffl = f.read()

# Convert the K_EFF values to float and prepare for plotting
snes = [float(s) for s in keffs.split()]
snel = [float(s) for s in keffl.split()]
n = len(snes)
x = range(1, n+1)
xc = [x for x in range(n+2)]  # Critical curve
yc = [1 for i in xc]          # Critical curve

# Plot the K_EFF results
plt.figure(figsize=(20, 10))
plt.plot(x, snes, 'o', color='orange', markersize=26, label="Diamond-difference")
plt.plot(x, snel, 'd', color='indigo', markersize=18, label="Linear discontinuous")
plt.plot(xc, yc, 'k--', label="Critical ($k_\\mathrm{eff}=1$)")
plt.xlim([0.5, n+0.5])
plt.ylim([0.5, 1.1])
plt.xlabel("Test problem number", fontsize=20)
plt.ylabel("$k_\\mathrm{eff}$", fontsize=20)
plt.title("$k_{\\text{eff}}$ results", fontsize=30)
plt.legend(fontsize=18, shadow=True, borderpad=1.0, labelspacing=1.2)
plt.grid()
plt.xticks(x)
plt.savefig("images/figures/keff_results.png")
plt.savefig("docs/docs/images/keff_results.png")

# Plot test 11 (Reed problem) flux comparison
pos_snes, flux_snes = read_flux("qa/snestp011.flxs")
pos_snel, flux_snel = read_flux("qa/snestp011.flxl")

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), height_ratios=[2, 1])

# Top plot: absolute fluxes (log scale)
ax1.plot(pos_snes, flux_snes, "b-", label="SNES (diamond diff)", linewidth=1.5)
ax1.plot(pos_snel, flux_snel, "r--", label="SNEL (linear discont)", linewidth=1.5)
ax1.set_ylabel("Scalar Flux")
ax1.set_title("Test 11 (Reed Problem) - SNES vs SNEL Flux Comparison")
ax1.legend()
ax1.grid(True, alpha=0.3)
ax1.set_yscale("log")
add_material_annotations(ax1)

# Add material labels
ax1.text(1.0, 0.01, "Mat 1\nσ=50", ha="center", fontsize=8)
ax1.text(2.5, 0.01, "Mat 2\nσ=5", ha="center", fontsize=8)
ax1.text(4.0, 0.01, "Void", ha="center", fontsize=8)
ax1.text(6.5, 0.01, "Mat 4\nσ=1", ha="center", fontsize=8)

# Bottom plot: relative difference
rel_diff = [
    (s - l) / s * 100 if abs(s) > 1e-10 else 0
    for s, l in zip(flux_snes, flux_snel)
]
ax2.plot(pos_snes, rel_diff, "k-", linewidth=1)
ax2.set_xlabel("Position x (cm)")
ax2.set_ylabel("Relative Difference (%)")
ax2.set_title("(SNES - SNEL) / SNES")
ax2.grid(True, alpha=0.3)
ax2.axhline(y=0, color="r", linestyle="-", alpha=0.3)
add_material_annotations(ax2)

plt.tight_layout()
plt.savefig("images/figures/test11_flux_comparison.png", dpi=150)
plt.savefig("docs/docs/images/test11_flux_comparison.png", dpi=150)
