+++
title = "Org publish site"
author = ["Chahak Mehta"]
date = 2021-12-09
draft = false
+++

`org-publish-project-alist` is the main variable to configure to set up the publishing of projects. It can be configured as a well formed property list with alternating keys and values.

This can be used to publish the org-roam entries.

```text
(setq org-publish-project-alist
      '(("hugo"
         :base-directory "~/Documents/org/roam/org/"
         :publishing-directory "~/Documents/chahak13.github.io/"
         :section-numbers nil
         :table-of-contents nil
         :publishing-function org-hugo-export-to-md
         :base-extension "org"
         )))

```

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
