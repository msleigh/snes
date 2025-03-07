import matplotlib.pyplot as plt

# Set the figure size
plt.rcParams["figure.figsize"] = (20, 10)

import subprocess

# Run the tests and extract K_EFF values for snes
subprocess.run(["make", "clobber"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run(["make", "tests"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run("grep 'K[ ]*EFF' qa/*.outs | tr -s ' ' | cut -d' ' -f4 > /tmp/keffs", shell=True, check=True)

# Run the tests and extract K_EFF values for snel
subprocess.run(["make", "clobber"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run(["make", "testl"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
subprocess.run("grep 'K[ ]*EFF' qa/*.outl | tr -s ' ' | cut -d' ' -f4 > /tmp/keffl", shell=True, check=True)

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
plt.show()
