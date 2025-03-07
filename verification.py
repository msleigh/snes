import matplotlib.pyplot as plt
import re
import glob
import subprocess
import sys

# Set the figure size
plt.rcParams["figure.figsize"] = (20, 10)

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
subprocess.run(["make", "clobber"])
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
plt.plot(x, snes, 'o', color='orange', markersize=26, label="Diamond-difference")
plt.plot(x, snel, 'd', color='indigo', markersize=18, label="Linear discontinuous")
plt.plot(xc, yc, 'k--', label="Critical ($k_\\mathrm{eff}=1$)")
plt.xlim([0.5, n+0.5])
plt.ylim([0.5, 1.1])
plt.xlabel("Test problem number", fontsize=20)
plt.ylabel("$k_\\mathrm{eff}$", fontsize=20)
plt.title("$k_\\mathrm{eff}$ results", fontsize=30)
plt.legend(fontsize=18, shadow=True, borderpad=1.0, labelspacing=1.2)
plt.grid()
plt.xticks(x)
plt.savefig("docs/docs/images/keff_results.png")
