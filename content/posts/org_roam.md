+++
title = "Setup for org-roam and its compatriots"
author = ["Chahak Mehta"]
tags = ["emacs", "org-roam", "org-mode"]
draft = false
+++

`org-roam` is an Emacs package that improves note-taking process in `org-mode`. Per the manual, _Org-roam is a tool for network thought_. I've been trying to get into the habit of note-taking and the result of extreme boredom led me to set this up in my system after hearing a lot of good things about it. The complete config block for init.el can be found at the end.


## Zettelkasten {#zettelkasten}

`Zettelkasten` is a German word, and the name of a note taking system that was developed by a German sociologist **Niklas Luhmann** who used to take notes on cards and then put them into a slip-box. The idea is that, with enough notes linked to each other, one can come up with various ideas to think, write, watch about. This  introduction is, by no means, a good introduction and one should definitely read about Zettelkasten on their own if they find it interesting. `org-roam` aims to be the slip-box, where every note is a zettel/card of its own.


### Fleeting notes {#fleeting-notes}

Notes that are essentially short quick reminders of information or ideas that need to be processed later on, or trashed. I have tried using `org-capture` for this with a decent amount of success but I plan to try `org-roam`'s daily notes feature first to have a comparison. These kinds of notes are a central inbox that are to be further refiled and processed into permanent notes.


### Permanent notes {#permanent-notes}

Permanent notes form the basic structure of the network of notes. They are full-fledged notes that are supposed to be self-explanatory, and can be of two types: **literature notes** and **concept notes**.


#### Literature notes {#literature-notes}

Each note is a condensed summary of a particular source of reading (eg. book, website, paper etc.). It can also contain any annotations that are made during the reading. These notes provide an easy access to sift through past readings, and provides a way to retain more consumed information.


#### Concept notes {#concept-notes}

Concept notes are more in-depth and require more care in authoring. They need to be detailed notes on a particular concept that one can refer to whenever required. They can then be linked to various fleeting or literature notes, which provides a better network of view for that particular concept.


## Installation {#installation}

I used the `straight.el` package manager, with the `use-package` package as a configuration manager to install and customize `org-roam` and other packages. This can be done by running the following

```emacs-lisp
  (use-package org-roam
    :straight t)
```

`org-roam` needs `sqlite3` to work. Please make sure that it is installed and available to emacs (Check `exec-path` variable)


## Getting started {#getting-started}

The first step to start using `org-roam` is to set the `org-roam-directory` variable. This directory will contain all the notes that are created during the time of using `org-roam`. We will set this using the `:config` block in `use-package`

```emacs-lisp
  (use-package org-roam
    :straight t
    :config
    (setq org-roam-directory "~/org"))
```

Org-roam manual recommends using a flat hierarchy to store all the notes, instead of creating a folder hierarchy. I wasn't sure how I felt about that but I decided to go with it anyway. Instead of any form of explicit categorization, links established between the notes should provide a form of grouping.

The next step is to enable the minor mode `org-roam-mode` globally. While this sets up Emacs with several hooks, it is not enabled on startup. To do that, we'll add a hook to the `use-package` block we've been building.

```emacs-lisp
  (use-package org-roam
    :straight t
    :hook
    ((after-init-hook . org-roam-mode))
    :config
    (setq org-roam-directory "~/org-roam"))
```

To build the note cache manually, one needs to use `M-x org-roam-db-build-cache`. Cache builds might take a while in the first run, but is often very quick in subsequent runs because it only reprocesses modified files.

To start taking notes, one of the two functions can be used `org-roam-find-file` or `org-roam-capture`. While both of them will create a new file with the provided title, the major difference is that `org-roam-find-file` will point to the newly created note in the buffer, while `org-roam-capture` will point the user back to the original buffer, similar to `org-capture`.

To insert links in a note, use `M-x org-roam-insert`. This will bring up a prompt with a list of titles for existing notes. Selecting an existing entry will create and insert a link to the current file. Entering a non-existent title will create a new note with that title and link it. Org-roam recommends liberal linking of files, facilitating build up of a denser graph of inter-connected notes. To toggle the visibility of backlinks of a note, along with some context, call `M-x org-roam`.


## Files {#files}

While the bulk of `org-roam`'s functionality is built up on vanilla org, it adds some extensions and keywords to support additional functionality.


### File Titles {#file-titles}

Each note needs to be prescribed with a title (duh?). This allows quick searching of notes. `org-roam` adds to the standard `+title` keyword in org-mode by using an new keyword called `#+roam_alias`. These aliases are space-delimited quoted strings that can also be used to refer the note while using `org-roam`. It also provides support for extracting title from the first headline in the file. The default action is to determine titles using `#+title` and `#+roam_alias` and if `#+title` isn't available, then fallback to the first headline. This is useful in topics like "World War 2" where it can also be referred to as "WW2" or "World War II" or such.

```text
  #+title: World War 2
  #+roam_alias: "WWII" "WW2" "World War II"
```

Title extraction can be customized using the `org-roam-title-sources` variable.


### File Tags {#file-tags}

Tags are used as meta-data for files. They facilitate interaction with notes where titles are insufficient. It also provides a good interface for categorization of notes, especially while searching through all notes. By default, tags are extracted from the `#+roam_tags` property. Tags are space-delimited quoted strings. To customize this behaviour, check `org-roam-tag-sources` variable.

**DOUBT**: The screenshots show a `- tags ::` directive. Need to confirm what works.


### File Refer {#file-refer}

Refs are unique identifiers for files. They are declared using the `+roam_key` and show up as a Ref Backlink in the org-roam link list.


## Templates {#templates}

Org-roam extends the `org-capture` capabilities for its templating system. Org-roam's capture templates are not completely compatible with `org-capture`, hence, to modify the templates, use the `org-roam-capture-templates`. An example template is shown below. This can be added in the `:config` section of the `use-package` configuration.

```emacs-lisp
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "paper" entry (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+AUTHOR: ${authors}\n"
           :unnarrowed t)
          ))
```


## Inserting Links {#inserting-links}

`file` links for files and `id` links for headlines are preferred as they ensure that the links will work even without `org-roam`. `file` links can be inserted using `org-roam-insert` whereas links to headlines can be inserted by navigating to the headline and calling `org-store-link` to store an ID and link into the org-roam database. That link can then be inserted via `org-insert-link`.

Org-roam also extends org linking syntax by adding `roam` links. Org-roam registers this link type, and interprets the path as follows:

-   `[[roam:title]]`: links to an org-roam file with title or alias as "title".
-   `[[roam:*headline]]`: links to the headline "headline" in the current org-roam file.
-   `[[roam:title*headline]]`: links to the headline "headline" in the org-roam file with title or alias "title"


## Graphing {#graphing}

`org-roam-server` is basically a web application to visualize the org-roam database. It needs to be installed separately and can be done easily using `straight.el` and `use-package`

```emacs-lisp
  (use-package org-roam-server
    :straight t
    :config
    (setq org-roam-server-host "127.0.0.1")
    (setq org-roam-server-port 8080)
    (setq org-roam-server-authenticate nil)
    (setq org-roam-server-export-inline-images t)
    (setq org-roam-server-serve-files nil)
    (setq org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
    (setq org-roam-server-network-poll t)
    (setq org-roam-server-network-arrows nil)
    (setq org-roam-server-network-label-truncate t)
    (setq org-roam-server-network-label-truncate-length 60)
    (setq org-roam-server-network-label-wrap-length 20)
    (org-roam-server-mode)
    )
```

`org-roam-server-mode` will enable the global mode. This will start a web server on <http://127.0.0.1:8080> that can be accessed for the org-roam visualization. To open the note on clicking a node, the `org-roam-protocol` needs to be set up, which is done in the next section.


## Roam protocol {#roam-protocol}

To enable org-roam's protocol extensions, add the following to emacs init file

```emacs-lisp
  (use-package org-roam-protocol
    :after org-roam)
```

I had to make a desktop application file for `emacsclient` to use `org-protocol`. It can be made on Linux platforms by creating a file in `~/.local/share/applications/` directory. An example file is shown below

```text
  [Desktop Entry]
  Name=Org-Protocol
  Exec=emacsclient %u
  Icon=emacs-icon
  Type=Application
  Terminal=false
  MimeType=x-scheme-handle/org-protocol
```


### The `roam-ref` Protocol {#the-roam-ref-protocol}

This protocol finds or creates a new note with a given `roam_key`. This is one of the most appealing features of `org-roam` for me. I'm guessing that this in conjunction with Zotero will make my note-taking life much easier on the web. To use this, a bookmarklet needs to be created in the browser. For my choice, that is currently Firefox. The steps to create a bookmarklet are

1.  Add a new bookmark from the bookmarks toolbar.
2.  In the name field, add the required name for the bookmark.
3.  In the location field, add the following code

    ```javascript
         javascript:location.href =
             'org-protocol://roam-ref?template=w&ref='
    ​         + encodeURIComponent(location.href)
    ​         + '&title='
    ​         + encodeURIComponent(document.title)
    ​         + '&body='
    ​         + encodeURIComponent(window.getSelection())
    ```

Here, `template` is the template key for a template in `org-roam-capture-ref-templates`

```emacs-lisp
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+roam_key: ${ref}"
           :unnarrowed t)
          ("w" "webpage" plain (function org-roam--capture-get-point)
           "/${body}/"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+TITLE: ${title}\n#+roam_key: ${ref}\n\n"
           :unnarrowed t)
          ))

```

Now, I can simply click on this bookmarklet from whatever website/article I am looking at, to directly create a note in emacs using org-roam. With the `webpage` template, I write down the selected text from the browser into the top of the body of the note, in italics.


## Daily-notes {#daily-notes}

Org-roam provides functions similar to org-journal. I decided to stick to org-roam for the daily notes too, to keep all the notes under one umbrella. To configure daily notes, there are 2 main variables: `org-roam-dailies-directory` and `org-roam-dailies-capture-templates`. They are the path to the daily-notes and the capture templates for daily notes respectively. A sane default config taken directly from the manual is

```emacs-lisp
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry (function org-roam--capture-get-point)
           "* #?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d>\n\n")))
```

The `org-roam-dailies-directory` should appear in the `:file-name` for the notes to be recognized as daily-notes.

To start writing daily-notes, call the `org-roam-dailies-capture-today` function. It creates an entry in the notes for today. `org-roam-dailies-find-today` finds the note for today, creating it if necessary. Another important function that can be used for searching is the `org-roam-dailies-find-date` which will open the note for the particular date using a calendar.

Other functions to keep in mind:

-   `org-roam-dailies-find-directory`
-   `org-roam-dailies-find-previous-note`
-   `org-roam-dailies-find-next-note`


## Diagnosing and Repairing Files {#diagnosing-and-repairing-files}

Once in a while, it would be a good idea to check for the correctness of the links. To do this easily, org-roam provides the `org-roam-doctor` function. This function checks the links in the current org-roam buffer. To run it on all files, use `C-u M-x org-roam-doctor` but this might take some time.


## Finding Unlinked References {#finding-unlinked-references}

It is difficult, and pointless to an extent, to try and remember all the notes to link to. For some such cases, org-roam has the `org-roam-unlinked-references`. It tries to find strings in the current buffer that match the title or aliases of any existing note in the org-roam database. These can then be converted to new links.

**PS**: Using this requires [ripgrep](https://github.com/BurntSushi/ripgrep) with PCRE support installed on the system.


## <span class="org-todo todo NEXT">NEXT</span> Deft, org-noter, org-roam-bibtex {#deft-org-noter-org-roam-bibtex}


### Deft {#deft}

`deft` is an interface for enhanced searching and filtering of notes. This provides a better interface to quickly go to a required note as it searches both, title and body of the note. It can be installed directly from MELPA and is fairly easy to get started with.

```emacs-lisp
  (use-package deft
    :straight t
    :bind
    ("<f8>" . deft)
    :config
    (setq deft-directory "~/org-roam")
    (setq deft-extensions '("org" "md"))
    )
```


## Keybindings for `org-roam` functions {#keybindings-for-org-roam-functions}

I decided to create a keymap for org-roam, and I had heard about the `general` package on David Wilson's Emacs-from-scratch series so I decided to give that a shot.

```emacs-lisp
  (use-package general
    :straight t
    :config
    (general-create-definer cm/leader-keys
      :prefix "M-o")

    (cm/leader-keys
     "f" 'org-roam-find-file
     "c" 'org-roam-capture
     "i" 'org-roam-insert
     "o" 'org-roam
     "M-t" 'org-roam-dailies-find-today
     "t" 'org-roam-dailies-capture-today
     "u" 'org-roam-unlinked-references)
    )
```


## `use-package` code block {#use-package-code-block}

```emacs-lisp
  (use-package org-roam
    :straight t
    :config
    (setq org-roam-directory "~/org-roam")
    (setq org-roam-capture-templates
          '(("d" "default" plain (function org-roam--capture-get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n"
             :unnarrowed t)
            ("p" "paper" entry (function org-roam--capture-get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n#+AUTHOR: ${authors}\n"
             :unnarrowed t)
            ))
    )

  (use-package org-roam-protocol
    :after org-roam
    :config
    (setq org-roam-capture-ref-templates
          '(("r" "ref" plain (function org-roam-capture--get-point)
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n#+roam_key: ${ref}"
             :unnarrowed t)
            ("w" "webpage" plain (function org-roam--capture-get-point)
             "/${body}/"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+TITLE: ${title}\n#+roam_key: ${ref}\n\n"
             :unnarrowed t)
            ))
    )

  (use-package org-roam-server
    :straight t
    :config
    (setq org-roam-server-host "127.0.0.1")
    (setq org-roam-server-port 8080)
    (setq org-roam-server-authenticate nil)
    (setq org-roam-server-export-inline-images t)
    (setq org-roam-server-serve-files nil)
    (setq org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
    (setq org-roam-server-network-poll t)
    (setq org-roam-server-network-arrows nil)
    (setq org-roam-server-network-label-truncate t)
    (setq org-roam-server-network-label-truncate-length 60)
    (setq org-roam-server-network-label-wrap-length 20)
    (org-roam-server-mode)
    )

  (use-package deft
    :straight t
    :bind
    ("<f8>" . deft)
    :config
    (setq deft-directory "~/org-roam")
    (setq deft-extensions '("org" "md"))
    )

```
