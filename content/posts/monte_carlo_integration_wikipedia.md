+++
title = "Monte Carlo integration - Wikipedia"
author = ["Chahak Mehta"]
date = 2021-05-14
tags = ["maths", "monte-carlo"]
draft = false
+++

Monte Carlo Integration is a technique for numerical integration using random numbers. It is a type of Monte Carlo methods that can be used to numerically compute a definite integral. The main differing point of such integration is that while standard methods use a regular interval to evaluate the integrand, Monte Carlo uses a random set of points to evaluate. This method is particularly useful for higher-dimensional integrals. There are various methods to perform such an integration: Uniform sampling, stratified sampling, importance sampling, sequential Monte Carlo, and mean field particle methods. We'll be taking a look at the uniform sampling, and importance sampling methods in brief.


## Overview {#overview}

At the core of it, Monte Carlo is an approximation method used to approximate the value of the integral as compared to the deterministic approach of methods like the [trapezoidal-rule]({{<relref "trapezoidal_rule.md#" >}}). Each simulation of a monte carlo integral provides a different outcome, which can be averaged over multiple simulations.

Let I be a multidimensional definite integral defined as

\\[
I=\int\_{a}^{b}f(x)dx
\\]

and a random variable \\(X\_i ~ p(x)\\) where \\(p(x)\\) must be nonzero for all \\(x\\) where \\(f(x)\\) is nonzero. Then, the Monte Carlo estimator is defined as

\\[
F\_{N} = \frac{1}{N}\sum\_{i=1}^{N}\frac{f(X\_i)}{p(X\_i)}
\\]

The value of \\(I\\) can be estimated by taking an average of several such Monte Carlo estimator values.


## Basic Monte Carlo Estimator {#basic-monte-carlo-estimator}

The basic monte carlo estimator is a special case of Importance Sampling Estimator case where we sample the points from a uniform random variable, to calculate the integral. Therefore, \\(X\_i ~ p(x) = c\\). This follows that for interval \\((a, b)\\), the value of \\(c = \frac{1}{b-a}\\). Therefore, the Monte Carlo estimator then becomes

We can also extend this to be N-Dimensional. For example, a 3D basic estimator for an integral

would be defined as

Therefore, a general rule can be written as follows. For an n-dimensional integral

the MC Estimator is defined as

where \\(N\\) is the number of samples that are taken from the uniform distribution for evaluation.


### Simulation {#simulation}

This method can be simulated fairly easily using python. Let us try and integrate the following function for \\(0.8 < x < 3\\)

To do this, we'll first have to define a python function using numpy

```python
  import numpy as np

  def f(x):
      return 1/(1+np.sinh(2*x)*np.log(x)**2)
```

Now that we have a function that calculates the value of \\(f(x)\\) at a given set of points, let us start with the MC estimator. First we will define the limits of the integral \\((a, b)\\) and the number of estimators \\(N\\). We will also define the number of points that are sampled.

```python
  n_estimators = 100
  N = 100
  a, b = 0.8, 3
```

Now, we need to perform the calculation for each MC estimator and find the average. We can do this more efficiently by using numpy's vector operations and random number generator.

```python
  rng = np.random.default_rng()

  r = rng.uniform(a, b, size=(n_estimators, N))
  result = (1/n_estimators)*((b-a)/N)*(np.sum(f(r)))
  result
```

```text
0.6786189790691812
```

We can check this result by comparing it with the function `scipy.integrate.quad`

```python
  from scipy import integrate

  integrate.quad(f, a, b)[0]
```

```text
0.6768400757156462
```

As we can see, the two results are fairly similar. Do note that the result due to MC estimators is bound to change but it is still a fairly close estimate to the integration function from scipy.


## Importance Sampling {#importance-sampling}

The formula for a MC estimator that we saw above was for an importance sampling estimator. What it means is that, instead of choosing random points over an interval with uniform probability, we try to sample points based on its expected contribution to the integral. This means that instead of a uniform distribution, we use a distribution \\(p(x)\\) of our choice that we hope makes the calculation more efficient. The intuition behind this is that if a particular point \\(x\_i\\) is picked up with a higher probability, then we weigh it down by a factor of its probability \\(p(x\_i)\\).
