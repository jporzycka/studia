points_x = 1024
N = 512
import numpy as np
import matplotlib.pyplot as plt

coefficients = np.zeros(N)



def calculate_coefficient(i):
    return 0


for i in range(N):
    coefficients[i] = calculate_coefficient(i+1)

res = np.zeros(points_x)


def calculate_result(x, series_coefficients):
    res = 0
    for i in range(N):
        res = res + series_coefficients[i] * np.sin((i+1.0)*x)

        
    return res    


t = np.linspace(0, np.pi, points_x)


for i in range(points_x):
    res[i] = calculate_result(t[i], coefficients)





plt.plot(t, res)