import numpy as np
import matplotlib.pyplot as plt

def calculate_coefficient(f, a, b, n_modes, x):
    """
    Rozwijanie funkcji `f` w sinusy na przedziale `(a, b)` przy użyciu podanych modów.
    - a, b: lewy i prawy kraniec przedziału
    - n_modes: liczba modów do użycia
    
    Zwraca współczynniki rozwinięcia
    """
    modes = np.arange(1, n_modes+1)
    a = np.concatenate(([0], (2 / (b - a)) * np.trapz(f(x) * np.sin(modes[:, None] * x), x, axis=1)))
    return a


def calculate_result(a, x):
    """
    Odtworzenie funkcji ze współczynników rozwinięcia `a` dla wartości `x`.
    - a: współczynniki rozwinięcia
    - x: wartości, dla których odtwarzana jest funkcja
    
    Zwraca odtworzoną funkcję
    """
    return a[0] / 2 + np.sum(a[1:] * np.sin(x[:, None] * np.arange(1, len(a))[None, :]), axis=1)


### ZADANIE 1

# Definiowanie odpowiednich funkcji
def f(x):
  return np.sin(x)**2
def g(x):
  return x**2
def h(x):
  return x**3

# Podany przedział
a = 0
b = np.pi

# Talica argumentów
x = np.linspace(a, b, 1024)

# Podane mody
n_modes = [64, 256, 1024]

# Funkcja do rysowania wykresów z użyciem zdefiniowanych funkcji
def result(f,name):
    for n in n_modes:
        a_coeffs = calculate_coefficient(f, a, b, n, x)
        y = calculate_result(a_coeffs, x)
        plt.plot(x, y, label=f"{n} modes")

    plt.plot(x, f(x), label=name)
    plt.legend()
    plt.show()

result(f, "sin^2(x)")
result(g, "x^2")
result(h, "x^3")


### ZADANIE 3

# Definiowanie funkcji
def f(x):
  y = np.where((a <= x) & (x <= b) | (c <= x) & (x <= d), -1, 0)
  return y

# Przedział (jakiś spełniający wymagania)
a = -1
b = 0
c = 1
d = 2

n_modes = 1024

# Tablica argumentów
x = np.linspace(a, d, 1000)

a_coeffs = calculate_coefficient(f, a, d, n_modes, x)
y = calculate_result(a_coeffs, x)

plt.plot(x, y, label=f"{1024} modes")
plt.legend()
plt.show()
