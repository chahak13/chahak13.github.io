+++
title = "Org publish site"
author = ["Chahak Mehta"]
date = 2021-12-09
draft = false
+++

```emacs-lisp
(defvar blog-src-dir "/Users/cmehta/Dropbox/Mac/Documents/org/roam/org")
(setq org-hugo-base-dir "/Users/cmehta/Documents/chahak13.github.io/")
(defun cm/publish-blog ()
  "Export all org file entries to hugo markdown in the blog content dir."
  (interactive)
  (setq blog-posts (directory-files blog-src-dir t "\.org$"))
  (dolist (file blog-posts)
    (org-hugo--export-file-to-md file t)
    ;; (print file)
    ))

(cm/publish-blog)
```

Publishing single file using `ox-hugo`.

```emacs-lisp
(require 'find-lisp)
(with-current-buffer (find-file-noselect (expand-file-name "moonlander-notes.org" blog-src-dir))
  (setq org-agenda-text-search-extra-files (find-lisp-find-files blog-src-dir "\.org$"))
  (org-hugo-export-wim-to-md))
```
