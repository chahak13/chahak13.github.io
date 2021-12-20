+++
title = "How to derive the weak form of the PDE？"
author = ["Chahak Mehta"]
draft = false
+++

## [How to derive the weak form of the PDE？](https://math.stackexchange.com/questions/1339709/how-to-derive-the-weak-form-of-the-pde) {#how-to-derive-the-weak-form-of-the-pde}

I have some difficulties solving the weak form of the PDE：
The proof of the preceding statement is elementary. The weak form of the PDE
\\(\nabla \cdot (A(x) \nabla u) + \omega^2 q(x) u = 0\\)
for all \\(\phi\\) that vanish at \\(\partial \Omega\\). Could you suggest some approaches to solve this?
Thank you very much for helping me out！！

Comments

-   (_user147263_) \\(\begingroup\\)
-   (_Chee Han_) \\(\begingroup\\)
-   (_Robert Lewis_) \\(\begingroup\\)
-   (_Robert Lewis_) \\(\begingroup\\)


### Answer 1 {#answer-1}

A general way to derive a weak form is to multiply a test function on both sides of the equation and then integrate them. The second step is to use some kind of divergence theorems to derive the weak solution such that the solution is some what not so smooth as in the strong form. For your question here, we can derive the weak form as follows:
Let the equation multiply a \\(\phi\in C\_0^\infty\\) on both sides and then integrate them, we get

\begin{equation}
\int\_\Omega-\nabla\cdot (A(x)\nabla u)\phi dx=\int\_\Omega\omega^2q(x)u\phi dx.
\end{equation}

Using the divergence theorem and the condition that \\(\phi\\) vanishes on bdry, the LHS becoms

\begin{equation}
\int\_\Omega (A(x)\nabla u)\cdot\nabla\phi dx.
\end{equation}

And we have the weak form

\begin{equation}
\int\_\Omega (A(x)\nabla u)\cdot\nabla\phi dx=\int\_\Omega\omega^2q(x)u\phi dx.
\end{equation}

for all \\(\phi\in C\_0^\infty\\).

Comments:
