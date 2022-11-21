;;; publish.el chahak13.github.io -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Chahak Mehta
;;
;; Author: Chahak Mehta <chahakcr7@gmail.com>
;; Maintainer: Chahak Mehta <chahakcr7@gmail.com>
;; Created: November 19, 2022
;; Modified: November 19, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/chahak/publish
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'ox-publish)

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")
        ("right-justify" . "@@html:<span class=\"right-justify\">$1</span>@@")))

(defun org-sitemap-custom-entry-format (entry style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s%" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
              entry
              filename))))

;; Enable HTML5
(setq org-html-html5-fancy t
      org-html-doctype "html5")

;; Disable default CSS and JS
(setq org-html-head-include-default-style nil
      org-html-head-include-scripts nil)

(defvar publish-chahak13-css "<link rel=\"stylesheet\" href=\"../style.css\" type=\"text/css\"/>")
(defvar publish-chahak13-header "<div id=\"updated\">Updated: %C</div>
<nav>
<a href=\"/\">Home</a>
<a href=\"/blog/sitemap.html\">Posts</a>
</nav>")

(defvar publish-chahak13-footer "<hr/>
<footer>
<div class=\"generated\">
Created with %c
</div>
</footer>")

(defvar publish-chahak13-base-dir "~/Documents/chahak13.github.io/org/")
(defvar publish-chahak13-publish-dir "~/Documents/chahak13.github.io/docs/")

;; Render ~verbatim~ as kbd tag in HTML
(add-to-list 'org-html-text-markup-alist '(verbatim . "<kbd>%s</kbd>"))

(setq org-publish-project-alist
      `(("index"
         :base-directory ,publish-chahak13-base-dir
         :base-extension "org"
         :exclude "."
         :include ("index.org")
         :publishing-directory ,publish-chahak13-publish-dir
         :publishing-function org-html-publish-to-html

         :html-head "<link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\"/>"
         :html-preamble "<div id=\"updated\">Updated: %C</div> <nav> <a href=\"/blog/sitemap.html\">Posts</a>"
         :html-postamble ,publish-chahak13-footer)

        ("pages"
         :base-directory ,publish-chahak13-base-dir
         :base-extension "org"
         :exclude "index.org"
         :recursive t
         :publishing-directory ,publish-chahak13-publish-dir
         :publishing-function org-html-publish-to-html

         :html-head ,publish-chahak13-css
         :html-preamble ,publish-chahak13-header
         :html-postamble ,publish-chahak13-footer)

        ("blog"
         :base-directory ,(concat publish-chahak13-base-dir "blog/")
         :base-extension "org"
         :publishing-directory ,(concat publish-chahak13-publish-dir "blog/")
         :publishing-function org-html-publish-to-html

         :auto-sitemap t
         :sitemap-title "Posts"
         :sitemap-filename "sitemap.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry org-sitemap-custom-entry-format

         :html-head ,publish-chahak13-css
         :html-preamble ,publish-chahak13-header
         :html-postamble ,publish-chahak13-footer)

        ("static"
         :base-directory ,publish-chahak13-base-dir
         :base-extension "css\\|txt\\|jpg\\|gif\\|png"
         :recursive t
         :publishing-directory ,publish-chahak13-publish-dir
         :publishing-function org-publish-attachment)

        ("chahak13.github.io" :components ("index" "pages" "blog" "static"))))

(require 'org-roam)
(org-roam-update-org-id-locations)
(if (member "t" command-line-args)
    (progn
      (print "force publish all org files")
      (org-publish-all t))
    (progn
      (print "only publish modified org files")
      (org-publish-all)))
(provide 'publish)
;;; publish.el ends here
