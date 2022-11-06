# Newtwon method

import sympy as sym
import numpy as np
import numpy as np

def newtonMethod(x0, iterationNumber, f, df):
    x=x0 # Start point
    # We iterate a certain number of times. 
    # If we do not find a satisfactory result in the process, 
    # we return "Can't estimate"
    for _ in range(iterationNumber):
        # Step in Newton's method
        x=x-f(x)/df(x)
        residual=np.abs(f(x))
        # If the value of the function at a given point is close 
        # enough to zero, we return this point as the root
        if residual < 1e-8:
            return x
    else:
        return "Can't estimate"

def prepare_data_for_NM(coefficients):
    # Defining function from given list of degree and coefficients
    x=sym.Symbol('x')
    f=0
    pow = 0
    for coefficient in coefficients:
        f+=coefficient*x**pow
        pow += 1
    
    # Calculate derivative
    diff_f=sym.diff(f,x)
    
    # Changing function and its derivative such that we can use them in NM
    f_func=sym.lambdify(x,f,'numpy')
    diff_f_func=sym.lambdify(x,diff_f,'numpy')

    # Calculating bound for roots (Cauchy's bound)
    # 1 + max{|a_n-1/a_n|,...,|a_0/a_n|}
    divided_by_an = list(map(lambda x: abs(x/coefficients[-1]), coefficients[:-1]))
    bound = 1 + max(divided_by_an)
    
    return f_func, diff_f_func, bound

def startNewtonMethod():
    coefficients_file = open('coefficients.txt','r')
    # Reading line by line
    lines = coefficients_file.readlines()

    # Defining function and 
    for line in lines:
        # List conversion, degree to int, coefficients to complex
        coefficients_str = line.strip().split(' ')
        deg = int(coefficients_str[0])

        # This ensures that we ignore characters that are not polynomial coefficients in the file
        coefficients = list(map(complex, coefficients_str[1:(deg+2)]))

        # Calculating the necessary data
        f_func, diff_f_func, bound = prepare_data_for_NM(coefficients)

        # Create a table of starting points in the calculated bounded area
        real_values = np.linspace(-bound, bound, 30)
        imaginary_values = np.linspace(-bound, bound, 30)

        # Finding all roots of given function
        roots = []
        for r in real_values:
            for im in imaginary_values:
                x0 = r + im*1j
                next_root = newtonMethod(x0, 200, f_func, diff_f_func)

                # Check if the calculated root is unique or was already calculated
                duplicate = False
                for root in roots:
                    if abs(root - next_root) < 1e-8:
                        duplicate = True
                        break
                if not duplicate:
                    roots.append(round(next_root, 10))
        
        print(roots)
    coefficients_file.close()

startNewtonMethod()

