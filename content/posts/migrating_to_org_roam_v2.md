+++
title = "Migrating to org-roam v2"
author = ["Chahak Mehta"]
tags = ["roam,", "emacs"]
draft = false
+++

`org-roam` recently moved to v2 and introduced many breaking changes. This provided me an opportunity to do a much needed writeover for `org-roam` configuration. I'm going to try and implement a workflow similar to Jethro's, as a starting step.


## Installing {#installing}

All the package repository recipies have been updated to install the newer version of org so a simple installation should be good enough to get the newer version.

```emacs-lisp
(use-package org-roam
  :straight t)
```

On installing v2 for the first time, there'll be warning showing the change of the version. It shows the steps to migrate an existing roam database to the newer version but I think I'll be doing a completely new setup. For reference, the warning would be

> ---
>
> WARNING: You’re now on Org-roam v2!
>
> ---
>
> You may have arrived here from a package upgrade. Please read the
> wiki entry at
> <https://github.com/org-roam/org-roam/wiki/Hitchhiker%E2%80%99s-Rough-Guide-to-Org-roam-V2>
> for an overview of the major changes.
>
> Notes taken in v1 are incompatible with v2, but you can upgrade
> them to the v2 format via a simple command. To migrate your
> notes, first make sure you’re on at least Org 9.4 (check with
> C-h v org-version) and set your org-roam-directory to your notes:
>
> (setq org-roam-directory "path/to/org/files")
>
> then, run:
>
> M-x org-roam-migrate-wizard
>
> If you wish to stay on v1, v1 is unfortunately not distributed on
> MELPA. See org-roam/org-roam-v1 on GitHub on how to install v1.
>
> If you’ve gone through the migration steps (if necessary), and
> know what you’re doing set ‘org-roam-v2-ack’ to ‘t’ to disable
> this warning. You can do so by adding:
>
> (setq org-roam-v2-ack t)
>
> To your init file.

To acknowledge that we do want the newer version, we'll add the variable as suggested in the warning.

```emacs-lisp
(setq org-roam-v2-ack t)
```


## Setup {#setup}

The biggest change in the newer version is that now instead of files, the smallest independent entity is a `node` which is defined as _any headline or top level file with an ID_. The ids can be created by using the `org-id-get-create` function.

Each link between the nodes use the Org's standard ID link feature.

First of all, we have to create the directory for roam to create the notes in.

```emacs-lisp
(make-directory "~/org/roam" t)
(setq org-roam-directory (file-truename "~/org/roam"))
```

`org-roam` doesn't have a major mode anymore. Instead, `org-roam-setup` is to be used to start roam.

```emacs-lisp
(org-roam-setup)
```


## Creating and Linking Nodes {#creating-and-linking-nodes}

There are 3 main functions for creating/linking the nodes in v2.

1.  `org-roam-node-insert` :: Creates a node if it doesn't exist, and inserts a link to he node at point.
2.  `org-roam-node-find` :: Creates a node if it doesn't exist, and visits the node.
3.  `org-roam-capture` :: Creates a node if it doesn't exist, and restores the current window configuration upon completion.

org-roam builds up on the `org-capture` templating system. The default template simply adds a property drawer with an ID and a title to the new file.

```text
:PROPERTIES:
:ID:       9102114b-61c3-4cce-857d-53bd72d3044a
:END:
#+title: Hello roam v2!
```


## Org-roam buffer {#org-roam-buffer}

Instead of the side window, backlinks are now shown in a dedicated org-roam buffer. `org-roam-buffer-toggle` launches an org-roam buffer that tracks the node currently at point. The content of this buffer changes depending on the node under point.


## Tags {#tags}

Org-roam uses the same tagging system that org uses. This means that the tags are set by the `#+filetags` keyword for the file and as regular org tags for headline level nodes.


## Org-roam protocol {#org-roam-protocol}

`org-protocol` provides a way to capture content from external applications like browser. It does it by extending the `org-protocol` with 2 protocols: the `roam-node` and `roam-ref` protocol.

```emacs-lisp
(use-package org-roam-protocol
  :after org-roam)
```


### org-protocol on Linux {#org-protocol-on-linux}

First of all, we need to install the `org-protocol`. To do that, we first need to create a desktop appplication in `~/.local/share/applications/org-protocol.desktop`:

```text
[Desktop Entry]
Name=Org-Protocol
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/org-protocol
```

We then associate the `org-protocol://` links with the desktop application by running the following command in the shell:

```shell
xdg-mime default org-protocol.desktop x-scheme-handler/org-protocol
```


### roam-node protocol {#roam-node-protocol}

The roam-node protocol opens the node with the ID specified `node` key (eg. `org-protocol://roam-node://roam-node?node=node-id`). `org-roam-graph` uses this to make the graph navigable.


### roam-ref protocol {#roam-ref-protocol}

This protocol find or creates a new note with a given `roam_key`. This can be used with a javascript bookmarklet in the browser.

```javascript
javascript:location.href =
    'org-protocool://roam-ref?template=r&ref='
    + encodeURIComponent(location.href)
    + '&title='
    + encodeURIComponent(document.title)
    + '&body='
    + encodeURIComponent(window.getSelection())
```

where `template` is the template key for a template in `org-roam-capture-ref-templates`. These templates should contain a `#+roam_key: ${ref}` in it.


## Templating system {#templating-system}

For templates, the first step is to create a normal org capture template.

```emacs-lisp
(setq cm/org-agenda-directory (file-truename "~/org/gtd"))
(setq org-agenda-files cm/org-agenda-directory)
(setq org-capture-templates
      `(("i" "Inbox" entry (file ,(expand-file-name "inbox.org" cm/org-agenda-directory))
	 ,(concat "* TODO %?\n"
		  "/Entered on/ %u"))
	("c" "org-protocol-capture" entry (file+olp ,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory) "The List")
	 "* TO-READ [[%:link][%:description]]"
	 :immediate-finish t)))
```

For org-roam templates,

```emacs-lisp
(setq org-roam-capture-templates
      '(("d" "default" plain
	 "%?"
	 :if-new (file+head "${slug}.org"
			    "#+TITLE: ${title}\n")
	 :immediate-finish t
	 :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain "/${body}/"
	 :if-new (file+head "${slug}.org"
			    "+TITLE: ${title}\n"
			    "+ROAM_KEY: ${ref}\n")
	 :unnarrowed t)))
```


## Using general.el to set keybindings {#using-general-dot-el-to-set-keybindings}

To add new keymaps and bindings, I'm going to use `general.el` to create new keybindings.

```emacs-lisp
(use-package general
  :straight t
  :config
  (general-create-definer cm/roam-leader
    :prefix "M-o")

  (cm/roam-leader
   "f" 'org-roam-node-find
   "c" 'org-roam-capture
   "i" 'org-roam-node-insert
   "o" 'org-roam-buffer-toggle)
  )
```
