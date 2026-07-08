import matplotlib.pyplot as plt
import glob
from pathlib import Path

KEFF_PATTERN = "K EFFECTIVE"
PLOT_OUTPUTS = [
    "images/figures/keff_results.png",
    "images/figures/test11_flux_comparison.png",
]


def write_plot_gallery(image_paths):
    """Write a simple HTML gallery for quick visual inspection."""
    gallery_path = Path("images/figures/index.html")
    gallery_path.parent.mkdir(parents=True, exist_ok=True)

    cards = []
    for image_path in image_paths:
        image_name = Path(image_path).name
        title = image_name.replace("_", " ").replace(".png", "")
        cards.append(
            f"""
    <section class="card">
      <h2>{title}</h2>
      <img src="{image_name}" alt="{title}">
    </section>"""
        )

    gallery_path.write_text(
        """<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>SNES verification plots</title>
    <style>
      :root {
        color-scheme: light;
        font-family: Helvetica, Arial, sans-serif;
      }
      body {
        margin: 0;
        padding: 2rem;
        background: #f6f7f8;
        color: #111;
      }
      main {
        max-width: 1200px;
        margin: 0 auto;
      }
      h1 {
        margin: 0 0 1.5rem;
      }
      .card {
        margin: 0 0 2rem;
        padding: 1rem;
        background: #fff;
        border: 1px solid #d9dde3;
        border-radius: 0.75rem;
        box-shadow: 0 0.25rem 0.75rem rgba(0, 0, 0, 0.06);
      }
      .card h2 {
        margin: 0 0 1rem;
        text-transform: capitalize;
      }
      .card img {
        display: block;
        width: 100%;
        height: auto;
      }
    </style>
  </head>
  <body>
    <main>
      <h1>SNES verification plots</h1>"""
        + "".join(cards)
        + """
    </main>
  </body>
</html>
""",
        encoding="utf-8",
    )


def extract_keff_values(pattern):
    """Extract k-effective values from matching output files."""
    values = []
    for filepath in sorted(glob.glob(pattern)):
        with open(filepath, encoding="utf-8") as handle:
            for line in handle:
                if KEFF_PATTERN not in line:
                    continue
                values.append(float(line.split("=")[-1].strip()))
                break
    if not values:
        raise ValueError(f"No {KEFF_PATTERN!r} lines found for pattern {pattern!r}")
    return values


def read_flux(filepath):
    """Read flux data from file, returning positions and flux values."""
    positions = []
    fluxes = []
    with open(filepath, encoding="utf-8") as handle:
        for line in handle:
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

def plot_keff_results():
    """Plot k-effective values for both schemes."""
    snes = extract_keff_values("qa/*.outs")
    snel = extract_keff_values("qa/*.outl")
    n = len(snes)
    x = range(1, n + 1)
    xc = list(range(n + 2))
    yc = [1 for _ in xc]

    plt.figure(figsize=(20, 10))
    plt.plot(x, snes, "o", color="orange", markersize=26, label="Diamond-difference")
    plt.plot(x, snel, "d", color="indigo", markersize=18, label="Linear discontinuous")
    plt.plot(xc, yc, "k--", label="Critical ($k_\\mathrm{eff}=1$)")
    plt.xlim([0.5, n + 0.5])
    plt.ylim([0.9, 1.1])
    plt.xlabel("Test problem number", fontsize=20)
    plt.ylabel("$k_\\mathrm{eff}$", fontsize=20)
    plt.title("$k_{\\text{eff}}$ results", fontsize=30)
    plt.legend(fontsize=18, shadow=True, borderpad=1.0, labelspacing=1.2)
    plt.grid()
    plt.xticks(x)
    plt.savefig(PLOT_OUTPUTS[0])
    plt.close()


def plot_reed_flux_comparison():
    """Plot the Reed problem flux comparison."""
    pos_snes, flux_snes = read_flux("qa/snestp011.flxs")
    pos_snel, flux_snel = read_flux("qa/snestp011.flxl")

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), height_ratios=[2, 1])

    ax1.plot(pos_snes, flux_snes, "b-", label="SNES (diamond diff)", linewidth=1.5)
    ax1.plot(pos_snel, flux_snel, "r--", label="SNEL (linear discont)", linewidth=1.5)
    ax1.set_ylabel("Scalar Flux")
    ax1.set_title("Test 11 (Reed Problem) - SNES vs SNEL Flux Comparison")
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    ax1.set_yscale("log")
    add_material_annotations(ax1)

    ax1.text(1.0, 0.01, "Mat 1\nσ=50", ha="center", fontsize=8)
    ax1.text(2.5, 0.01, "Mat 2\nσ=5", ha="center", fontsize=8)
    ax1.text(4.0, 0.01, "Void", ha="center", fontsize=8)
    ax1.text(6.5, 0.01, "Mat 4\nσ=1", ha="center", fontsize=8)

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
    plt.savefig(PLOT_OUTPUTS[1], dpi=150)
    plt.close(fig)


def main():
    """Generate verification plots from existing test outputs."""
    plot_keff_results()
    plot_reed_flux_comparison()
    write_plot_gallery(PLOT_OUTPUTS)


if __name__ == "__main__":
    main()
