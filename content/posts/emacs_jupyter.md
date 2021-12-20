+++
title = "emacs-jupyter and inline images in org-mode"
author = ["Chahak Mehta"]
tags = ["emacs", "org-mode", "python"]
draft = false
+++

`emacs-jupyter` is an emacs interface for jupyter kernels. It can be installed using

```emacs-lisp
(use-package jupyter
  :straight t)
```

If `ob-async` is installed, then `jupyter-python` needs to be added to the list of languages that are not run asychronously. (Refer [this issue](https://github.com/astahlman/ob-async/pull/71)). This can be done by

```emacs-lisp
(setq ob-async-no-async-languages-alist '("jupyter-python"))
```

An example python block:

```jupyter-python
print("ab")
```

```text
ab
```


## emacs-jupyter works doesn't work in server {#emacs-jupyter-works-doesn-t-work-in-server}

When I first installed `emacs-jupyter` I found a weird situation where I could not run jupyter code blocks in org-mode if emacs was started as a daemon. On the other hand, if it was started separately as a window, everything ran fine. The issue was that I was adding `/home/boticelli/.local/bin/` (path to jupyter executable) to `exec-path` at the end of the config files. When opening emacs from a terminal as an independent session, it was considering this added path somehow, maybe by inheriting it from the shell itself, but it was. I moved that line to near the beginning of the `init.el` file itself and this seemed to solve the issue. This issue was very similar to the [Pipenv in emacs]({{<relref "pipenv_in_emacs.md#" >}}) issue that I had recently.


## Why emacs-jupyter {#why-emacs-jupyter}

Running python code in normal org-mode source block is nice and easy. But using them to plot images was a bit painful/difficult. Since `emacs-jupyter` uses `ipython` kernel, it allows some magic commands like `%matplotlib inline` etc to make this easier. It also gives `%timeit` which is really useful.

Now, with `emacs-jupyter`, I can do imports like this and make plotting easier for myself.

```jupyter-python
%matplotlib inline
import numpy as np
import matplotlib.pyplot as plt

fig, ax = plt.subplots()
ax.hist(np.random.default_rng().normal(0, 1, 50000), bins=100);
```

{{< figure src="/ox-hugo/normal_dist.png" >}}
