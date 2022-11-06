import sympy
import scipy.integrate
import numpy
from matplotlib import pyplot
import math

def functionCOS(x,n):
    if x<0:
        return (0)*math.cos(n*x)
    return (math.pi - x)*math.cos(n*x)/2

def functionSIN(x,n):
    if x<0:
        return (0)*math.sin(n*x)
    return (math.pi - x)*math.sin(n*x)/2


def S(y):
    x = sympy.Symbol('x')
    result = 0
    for n in range(1,30):
        integratedAN = scipy.integrate.quad(functionCOS,-math.pi,math.pi,args=(n))
        integratedBN = scipy.integrate.quad(functionSIN,-math.pi,math.pi,args=(n))
        AN = 2/math.pi * integratedAN[0]
        BN = 2/math.pi * integratedBN[0]
        result += AN*math.cos(n*y) + BN*math.sin(n*y)
    integratedAN = scipy.integrate.quad(functionCOS,-math.pi,math.pi,args=(n==0))
    result += 1/math.pi * integratedAN[0]
    return result

z = numpy.linspace(-math.pi,math.pi,num=100)
pyplot.plot(z,[S(i) for i in z])
pyplot.show()
