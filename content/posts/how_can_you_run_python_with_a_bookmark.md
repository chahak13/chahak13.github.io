+++
title = "How can you run python with a bookmark?"
author = ["Chahak Mehta"]
draft = false
+++

## [How can you run python with a bookmark?](https://stackoverflow.com/questions/53882628/how-can-you-run-python-with-a-bookmark) {#how-can-you-run-python-with-a-bookmark}

I am writing a script to log into web pages (I know it's a bad idea, and I don't care), but I need to run it with a bookmark from Firefox. Does anyone know how to help me?

Comments

-   (_jarst_) Save yourself a hassle and create a javascript bookmarklet.
-   (_Glazbee_) Following on @jarst's comment, why not just create an extention?
-   (_orangeInk_) stackoverflow.com/questions/2034373/…


### Answer 1 {#answer-1}

There is no pre-made solution and as it stands, you left part of your question ambiguous.
But here is one possible outline on how you could accomplish what you want from completely inside firefox.
First off you need to get python in the browser, but it is normally compiled into machine code.  The solution is the [Transcrypt project](http://www.transcrypt.org), which is Python is transcompiled into javascript and that script is then loaded into your browser. This allows you to run python code.
Extensions to python are made so it can access the DOM in a similiar way you can from javascript.
The transcrypt website has some examples, and some more can be found at a [draft page at Mozilla](https://developer.mozilla.org/en-US/docs/Learn/Drafts/Python).
Your bookmarklet would be to load a script to load transcrypt and feed it your python code, something like
javascript:(function(){var el=document.createElement('script');el.src='file:///something/something/transcript\_loader\_and\_my\_python\_code.js';document.body.appendChild(el);})();
But the exact bookmarklet would depend on how you go about things.  Writing transcript\_loader\_and\_my\_python\_code.js is on your own, but if you get the examples from the links above working it should be trivial.

Comments:
