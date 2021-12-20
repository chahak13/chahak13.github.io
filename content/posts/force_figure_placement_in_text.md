+++
title = "Force figure placement in text"
author = ["Chahak Mehta"]
draft = false
+++

## [Force figure placement in text](https://tex.stackexchange.com/questions/8625/force-figure-placement-in-text) {#force-figure-placement-in-text}

I have a problem when a lot of figures are in question. Some figures tend to "fly around", that is, be a paragraph below, although I placed them before that paragraph. I use code:

```text
\begin{figure}[ht]
\begin{center}
\advance\leftskip-3cm
\advance\rightskip-3cm
\includegraphics[keepaspectratio=true,scale=0.6]{slike/visina8}
\caption{}
\label{visina8}
\end{center}\end{figure}

```

to place my figures. How can I tell latex I REALLY want the figure in that specific place, no matter how much whitespace will be left?

Comments

-   (_Martin Scharrer_) Sidenote: Don't use the center environment but the \centering command for figures. See Should I use center or centering for figures and tables?
-   (_codeman48_) To definitely place a paragraph after a figure, use the command \FloatBarrier somewhere between the figure and the paragraph. It forces all figures defined before the command to render before that point in text. You will need to add \usepackage{placeins} in the preamble to use the command. Sometimes, I have found this very useful.
-   (_Benjamin McKay_) I gave up using the figure environment, and just use small figures embedded in text or centered between paragraphs. Smaller figures fit better.


### Answer 1 {#answer-1}

**The short answer:** use the “[float](http://www.ctan.org/tex-archive/help/Catalogue/entries/float.html)” package and then the `[H]` option for your figure.

```text
\usepackage{float}

...

\begin{figure}[H]
\centering
\includegraphics{slike/visina8}
\caption{Write some caption here}\label{visina8}
\end{figure}

```

**The longer answer:** The default behaviour of figures is to float, so that LaTeX can find the best way to arrange them in your document and make it look better. If you have a look, this is how books are often typeset. So, usually the best thing to do is just to let LaTeX do its work and don't try to force the placement of figures at specific locations. This also means that you should avoid using phrases such as “~in the following figure:~”, which requires the figure to be set a specific location, and use “~in Figure~\ref{..}~“ instead, taking advantage of LaTeX's cross-references.
If for some reason you _really_ want some particular figure to be placed “HERE”, and not where LaTeX wants to put it, then use the `[H]` option of the “[float](http://www.ctan.org/tex-archive/help/Catalogue/entries/float.html)” package which basically turns the floating figure into a regular non-float.
Also note that, if you don't want to add a `caption` to your figure, then you don't need to use the `figure` environment at all! You can use the `\includegraphics` command anywhere in your document to insert an image.

Comments:

-   (_Juan A. Navarro_) thanks for noting this! I wasn't aware of the change. H doesn't seem to work without any packages, but does work loading float. Is H from float the same as !h?
-   (_user2478_) no, [!h] is changed anyway by most documentclasses to [!ht]. And the meaning of h is only: here, if possible, but not absolutely here. The ! allows LaTeX to minimze all counters and lengths which refer to floating environments.
-   (_Evgeni Sergeev_) For documents not intended to be printed, there is no reason to try and save paper, so large areas of whitespace aren't a problem. So it's much better to have the figure breaking the text at the most logical point, rather than floating somewhere else. These awkward conventions will go just like Latin went.
-   (_Juan A. Navarro_) I don't think that the usual concerns are about saving space, rather than stylistically trying to find the best place where to place a figure, table, etc.
-   (_Henrique Fernandes Cipriano_) you save lifes!


### Answer 2 {#answer-2}

do _not_ use a floating environment if you do not want it float.

```text
\usepackage{caption}
...
\noindent%
\begin{minipage}{\linewidth}% to keep image and caption on one page
\makebox[\linewidth]{%        to center the image
  \includegraphics[keepaspectratio=true,scale=0.6]{slike/visina8}}
\captionof{figure}{...}\label{visina8}%      only if needed
\end{minipage}

```

or

```text
\begin{center}
  \includegraphics[...]{slike/visina8}}
\captionof{figure}{...}\label{visina8}%      only if needed
\end{center}

```

Comments:

-   (_sdaau_) Hi @Herbert - thanks a LOT for this answer! I had never before understood that \begin{figure} is a floating environment - while \begin{minipage} is not! I had a problem with wanting to include an image on bottom of page w/ text, and not even [H] helped; only this! I just replaced minipage for figure - and captionof for caption - and finally got what I wanted!! Thanks a lot again, cheers!
-   (_Tobi_) Why do you use a \makebox? Doesn’t it look the same without it?
-   (_user2478_) @Tobi: if the image is not larger than \textwidth yes, otherwise not! \makebox centers the image independently from its width
-   (_user2820379_) This was so super helpful; IMHO this should be the accepted answer as it works more as expected as the currently accepted one.


### Answer 3 {#answer-3}

One solution not mentioned by any of the other answers that just sorted me out is to use `\clearpage`
No special packages are needed.
`\clearpage` forces all figures above it in the `.tex` file to be printed before continuing with the text. This can leave large white spaces.
For me this was the best solution because I did not have to change any of the formatting and it just made sure that all figures were printed before the next bit of text. My issue was a part of the document with lots of figures and not much text.

Comments:


### Answer 4 {#answer-4}

You can now use the `adjustbox` package to turn your boxed stuff into a non-floating float replacement using the `nofloat=&lt;type&gt;` key. Caption and label can be added by own keys, _before_ the `nofloat`. For centering the `center` key can be used. To add the vertical space use the `vspace` key. This solution has the benefit, that you can also use all the many other features of `adjustbox` to modify the content (min/max scaling, framing, etc.)
Note that if the figure content is just a single image you can just use the same keys on `\adjustbox` and get a one-liner. If all you want is a tabular then there is the `tabular` key for `{adjustbox}`.
If you later want to change it to a real float just turn `nofloat` to `float` and remove the `vspace` key. `adjustbox` places the caption on top for `table~s and on bottom for ~figure~s. This can be changed by using the keys ~captionbelow` or `captionabove` instead of `caption`.
See the `adjustox` manual for all options.
If you don't want to box the content you can still use the `adjustbox` package as it provides the `{adjnofloat}{&lt;type&gt;}` environment. It is used internally to implement the `nofloat` key. Users can either redefine this environment to change or patch the `nofloat` behavior or use the environment directly.  The environment uses code very similar as in [Herberts answer](https://tex.stackexchange.com/a/8631/2975).

```text
\documentclass{article}
\usepackage{blindtext}% for example text here only
\usepackage{adjustbox}
\begin{document}
\blindtext

\begin{adjustbox}{center,caption={some caption},label={somelabel},nofloat=figure,vspace=\bigskipamount}
% maybe other stuff
\includegraphics[width=\textwidth]{example-image}% example only, could also be \adjustimage
% maybe other stuff
\end{adjustbox}

\blindtext

% For simple images, a one liner is enough
\adjustimage{width=\textwidth,center,caption={some caption},label={somelabel},nofloat=figure,vspace=\bigskipamount}{example-image}

\blindtext


\begin{adjustbox}{center,caption={some caption},label={somelabel},nofloat=table,vspace=\bigskipamount}
% maybe other stuff
\begin{tabular}{lll}
 some & tabular & is\\
 also & possible & with this \\
\end{tabular}
% maybe other stuff
\end{adjustbox}

\blindtext

% For just a tabular:
\begin{adjustbox}{tabular=lll,center,caption={some caption},label={somelabel},nofloat=table,vspace=\bigskipamount}
 some & tabular & is\\
 also & possible & with this \\
\end{adjustbox}

\blindtext
\end{document}

```

Part of the result:
[[![](https://i.stack.imgur.com/ERcI5.png)][]]

Comments:


### Answer 5 {#answer-5}

Use `[ht!]` to be more emphatic about placement. And, if you want text to follow the figure, instead of beginning before, employ `\usepackage{parskip}` in your preamble, and allow a space before the text in question.
I also employ `\vspace{}` or `\vspace*{}` before the figure environment in order to manipulate the space between text and figure to give a more balanced look. I don't have any trouble this way, even with more complex multifigure environments employing `\subfigure`.

Comments:
