+++
title = "Gaussian quadrature in scipy"
author = ["Chahak Mehta"]
date = 2021-07-14
tags = ["python", "maths"]
draft = false
+++

Scipy has an gaussian quadrature integration built-in in the `integrate` module as the `integrate.quadrature` function.

```jupyter-python
import numpy as np
from scipy import integrate
```

A simple example of quadrature integration can be seen as follows, where we integrate the simple function \\(f(x) = 2x\\) from limits 0 to 2.

```jupyter-python
f = lambda x: 2*x

print("Quadrature integration:", integrate.quadrature(f, 0, 2))
print("Analytical solution:", 2**2)
```

A more complex function can also be passed to the `quadrature` function.

```jupyter-python
def func(x):
    return x**2 + 2*x + 3

a = 2
print("Quadrature integration:", integrate.quadrature(func, 0, 2))
print("Analytical solution:", a**3/3 + a**2 + 3*a)
```

Now, to take into consideration functions that are dependent on more than just the integrating variable.

```jupyter-python
def func_2(t, z):
    return z*t + z**2 + 2*t

a = 2
z = 2
print("Quadrature integration:", integrate.quadrature(func_2, 0, 2, args=(z,)))
print("Analytical solution:", z*a**2/2 + a*z**2 + a**2)
```

If the function to integrate depends on more variables and even other functions, then we can pass those functions as parameters to the integrating function and solve the quadrature integration.

```jupyter-python
def func_3(t, z, f1, f2):
    return f1(t) + z*t + f2(t)**2

f1 = lambda x: x**2
f2 = lambda x: x**3

a, z = 2, 2
print("Quadrature integration:", integrate.quadrature(func_3, 0, 2, args=(z, f1, f2)))
print("Analytical solution:", a**3/3 + z*a**2/2 + a**7/7)
```

Functions with vector output need to provide the `vec_func` argument as `True`.

```jupyter-python
def f(x):
    # print("x:", x)
    # print("z:", z)
    return z*x*2

z = np.array([2, 3])
integrate.quad_vec(f, 0, 2)
```

For fixed order Gaussian quadrature integration

```jupyter-python
f = lambda x, a: np.sin(np.kron(a, x)).reshape(-1, x.shape[0])
x = np.array([1,2,3])
a = np.array([1,2])
print(f(x, a).shape)
```



```jupyter-python
np.kron(a, x)
```

:results:

Writing gaussian quadrature integration from scratch using numpy

```jupyter-python
def norm_pdf(x, mu, sigma):
    mu, sigma = mu.reshape(-1, 1), sigma.reshape(-1, 1)
    x = x.reshape(-1, 1)
    variance = sigma**2
    numerator = x - mu
    denominator = 2 * variance
    pdf = ((1/(np.sqrt(2 * np.pi) * sigma)) * np.exp(-(numerator**2) / denominator))
    return pdf

```

```jupyter-python
def h_z(a, b, T_i, x, y, sigma_m2, delta_a, delta_b, indicator, n_time_samples=1000):
    mc_sum = np.zeros(x.shape)
    t = rng.uniform(0, T_i, size=n_time_samples)
    alpha = t/T_i
    mu_x = a.x + alpha * (b.x - a.x)
    mu_y = a.y + alpha * (b.y - a.y)
    sigma = np.sqrt(t * (1 - alpha) * sigma_m2
                    + (1 - alpha)**2 * (delta_a**2)
                    + (alpha**2) * (delta_b**2))

    pdf_x = norm_pdf(x, mu_x, sigma)
    pdf_y = norm_pdf(y, mu_y, sigma)
    mc_sum += indicator * pdf_x * pdf_y

    return mc_sum
```
