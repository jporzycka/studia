import numpy as np
import matplotlib.pyplot as plt

def expand_in_sines(f, a, b, n_modes):
  x = np.linspace(a, b, 1000)
  modes = np.arange(1, n_modes+1)
  a = np.concatenate(([0], (2 / (b - a)) * np.trapz(f(x) * np.sin(modes[:, None] * x), x, axis=1)))
  return a

def reconstruct_from_sines(a, x):
  y = np.sum(a * np.sin(x[:, None] * np.arange(1, len(a)+1)[None, :]), axis=1)
  return y

def f(x):
  return -1

# define interval
a = 0
b = np.pi

# define number of modes to use
n_modes = 1024

# expand function in sines
a_coeffs = expand_in_sines(f, a, b, n_modes)

# create x values to plot
x = np.linspace(a, b, 1024)

# reconstruct function from expansion coefficients
y = reconstruct_from_sines(a_coeffs, x)

# plot function
plt.plot(x, y)
plt.show()
