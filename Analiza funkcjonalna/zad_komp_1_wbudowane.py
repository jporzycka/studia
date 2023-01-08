import sympy as sym
from sympy import pi

t = sym.symbols('t')
f = sym.Piecewise((0, t<=0), (pi-t, t>0))
ser = sym.fourier_series(f, (t,-pi,pi))
#print(ser.truncate(5), (t,-pi,pi))
sym.plot((ser.truncate(5), (t,-pi,pi)),(ser.truncate(30), (t,-pi,pi)),(f, (t,-pi,pi)))

