+++
title = "Completion in Emacs"
author = ["Chahak Mehta"]
date = 2021-07-11
tags = ["emacs"]
draft = false
+++

Trying out different completion frameworks for Emacs instead of simply Ivy, just for the heck of it.

```emacs-lisp
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package vertico-buffer
  :after vertico)
```

Also use the `orderless` completion style to improve the completion methods for vertico/selectrum etc. This also allows using space as delimeter in vertico, since it reads space as a character to match otherwise.

```emacs-lisp
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))
```

To persist history over Emacs restarts, vertico uses the `savehist-mode` that is built into emacs by default.

```emacs-lisp
(use-package savehist
  :straight nil
  :init
  (savehist-mode))
```


## <span class="org-todo todo TODO">TODO</span> Embark {#embark}


## <span class="org-todo todo TODO">TODO</span> Marginalia {#marginalia}

Marginalia provides extra metadata depending on the context and type of object being actioned upon. This provides an `ivy-rich` type interface for vertico.

```emacs-lisp
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))
```


## <span class="org-todo todo TODO">TODO</span> Consult {#consult}
