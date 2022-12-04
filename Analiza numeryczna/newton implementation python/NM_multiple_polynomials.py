# Newton's method

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
        if residual < 1e-10:
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
    
    # Calculate derivative, second derivative
    diff_f=sym.diff(f,x)
    diff_diff_f = sym.diff(diff_f,x)
    
    # Changing function and its derivatives such that we can use them in NM
    f_func=sym.lambdify(x,f,'numpy')
    diff_f_func=sym.lambdify(x,diff_f,'numpy')
    diff_diff_f_func=sym.lambdify(x, diff_diff_f, 'numpy')

    # Calculating bound for roots (Cauchy's bound)
    # 1 + max{|a_n-1/a_n|,...,|a_0/a_n|}
    # https://en.wikipedia.org/wiki/Geometrical_properties_of_polynomial_roots
    divided_by_an = list(map(lambda x: abs(x/coefficients[-1]), coefficients[:-1]))
    bound = 1 + max(divided_by_an)
    
    return f_func, diff_f_func, diff_diff_f_func, bound


def contraction(f, df, ddf, L, root):
    r = root.real
    im = root.imag
    # Setting R
    R = 0.01

    # Points in closed ball
    real_values = np.linspace(r-R, r+R, 4)
    imaginary_values = np.linspace(im-R, im+R, 4)

    # Assumptions checking
    for r in real_values:
        for im in imaginary_values:
            x = r + im*1j
            if np.abs(1-(df(x)*df(x)-f(x)*ddf(x))/df(x)**2) > L or 1e-10/(1-L)>R:
                return False

    return True


def startNewtonMethod():
    coefficients_file = open('coefficients.txt','r')
    # Reading line by line
    lines = coefficients_file.readlines()

    # Defining function etc. for every line
    for line in lines:
        # List conversion, degree to int, coefficients to complex
        coefficients_str = line.strip().split(' ')
        deg = int(coefficients_str[0])

        # This ensures that we ignore characters that are not polynomial coefficients in the file
        coefficients = list(map(complex, coefficients_str[1:(deg+2)]))

        # Calculating the necessary data
        f_func, diff_f_func, diff_diff_f_func, bound = prepare_data_for_NM(coefficients)

        # Create a table of start points in the calculated bounded area
        real_values = np.linspace(-bound, bound, 30)
        imaginary_values = np.linspace(-bound, bound, 30)

        # Finding (hopefully) all roots of given function
        roots = []
        for r in real_values:
            for im in imaginary_values:
                x0 = r + im*1j
                next_root = newtonMethod(x0, 200, f_func, diff_f_func)

                # Check if the calculated root is unique or was already calculated
                duplicate = False
                if type(next_root) != str:
                    for root in roots:
                        if abs(root - next_root) < 1e-8:
                            duplicate = True
                            break
                    if not duplicate:
                        for L in [0.009, 0.09, 0.9]:
                            if contraction(f_func, diff_f_func, diff_diff_f_func, L, next_root):
                                roots.append(next_root)
                                break
                        
        rounded_roots = [np.round(root, 6) for root in roots]
        print(rounded_roots)
    coefficients_file.close()


startNewtonMethod()

    






