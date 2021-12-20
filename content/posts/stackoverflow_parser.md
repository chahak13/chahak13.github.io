+++
title = "PoC: Stackoverflow parser"
author = ["Chahak Mehta"]
date = 2021-12-08
draft = false
+++

Load the `requests` and `BeautifulSoup` libraries that will be used for scraping the webpage.

```python
import sys
import re
import requests
from bs4 import BeautifulSoup as bs
```

As a proof of concept, we will grab a random stackoverflow question that has more than one answers, one accepted answer, and code as a part of the answer to try and cover the major cases (still need to check about images). One such example is - <https://stackoverflow.com/questions/357307/how-to-call-a-parent-class-function-from-derived-class-function>

We'll download the webpage for further scrutiny using the `requests.get` function and then parsing it using `BeautifulSoup`.

```python
url = sys.argv[1]
# url = "https://math.stackexchange.com/questions/1339709/how-to-derive-the-weak-form-of-the-pde"
base_url = url.split("/")[2]
response = requests.get(url)
if response.ok:
    soup = bs(response.text, features="html.parser")
```

There are two main classes of objects that are of our interest - 1. Question and 2. Answers (with possibly an accepted marker). Looking at the source code tells us that there are `question` and `answer` properties that can be used to filter them out. We'll take a stab at finding the question first.

A point to note, we cannot directly search for `question-hyperlink` links because there are other questions on the page too, so we need to first extract the question header from where we can get the required title.

<a id="code-snippet--question-title"></a>
```python
question_header = soup.find('div', {'id': 'question-header'})
question_title = question_header.find('a', {'class': 'question-hyperlink'}).text.strip()
question_link = f"https://{base_url}{question_header.find('a', {'class': 'question-hyperlink'}).get('href')}"
# print((question_title, question_link))
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--question-title">Code Snippet 1</a></span>:
  This piece of code helps us in extracting the title of the question along with a link to it.
</div>

Now that we have the title and the link to the question, we parse for the question content.

<a id="code-snippet--question-body"></a>
```python
prog = re.compile(r"\<a .*?\/a\>")
def parse_post(post_banner):
    elements = post_banner.find_all(recursive=False)
    name_set = {'p', 'pre'}
    full_post = []
    for element in elements:
        if element.name in name_set:
            if element.name == "p":
                post = str(element)
                post = post.removeprefix("<p>").removesuffix("</p>")
                # Wrap code blocks in ~
                post = post.replace("<code>", "~").replace("</code>", "~")
                # Wrap emphasis in /
                post = post.replace("<em>", "/").replace("</em>", "/")
                # Wrap bold in *
                post = post.replace("<strong>", "*").replace("</strong>", "*")
                # Change href links to org links
                result = prog.finditer(post)
                for res in result:
                    temp = bs(res.group(0), features="html.parser")
                    href_link = temp.find('a').get('href')
                    href_text = temp.text
                    post = prog.sub(f"[[{href_link}][{href_text}]]", post, count=1)
                full_post.append(f"{post}\n")
            else:
                codeblock = element.find('code')
                lang = codeblock.get('class', "lang-").rsplit('-')[-1]
                begin_tag = f"#+begin_example {lang}"
                end_tag = "#+end_example"
                code = codeblock.text
                full_post.append(f"{begin_tag}\n{code}\n{end_tag}\n")
    post_text = ''.join(full_post)
    return post_text

question = soup.find('div', {'id': 'question'})
post_banner = question.find('div', {'class': 's-prose'})
question_text = parse_post(post_banner)
# print(question_text)
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--question-body">Code Snippet 2</a></span>:
  Parse the content of the question
</div>

Each question also contains comments that can be really useful, so we will try to parse them too.

<a id="code-snippet--question-comments"></a>
```python
def parse_comments(cell):
    comments = []
    for comment in cell.find_all('div', {'class': 'comment-body'}):
        comment_str = comment.find('span').text
        try:
            comment_user = comment.find('a', {'class': 'comment-user'}).text
        except:
            comment_user = comment.find('span', {'class': 'comment-user'}).text
        comments.append(f"+ (/{comment_user}/) {comment_str}")
    comments_text = '\n'.join(comments)
    return comments_text

question_comments = parse_comments(question)
# print(question_comments)
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--question-comments">Code Snippet 3</a></span>:
  Parse comments in the question
</div>

We can now combine all the parts to create a complete org question string.

<a id="code-snippet--question-org-string"></a>
```python
question_org_str = f"* [[{question_link}][{question_title}]]\n\n{question_text}\nComments\n{question_comments}"
# print(question_org_str)
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--question-org-string">Code Snippet 4</a></span>:
  Generate complete org string for the question
</div>

Now that we have covered most of the question components, we will start parsing the answers.

<a id="code-snippet--parse-answers"></a>
```python
answers = []
for i, answer in enumerate(soup.find_all('div', {'class': 'answer'})):
    answer_post = answer.find('div', {'class': 'post-layout'})
    answer_cell = answer_post.find('div', {'class': 'answercell'}).find('div', {'class': 's-prose'})
    answer_str = parse_post(answer_cell)
    answer_comments = parse_comments(answer_post)
    answers.append(f"** Answer {i+1}\n\n{answer_str}\n Comments:\n{answer_comments}\n")

answer_org_str = '\n'.join(answers)
# print(answer_org_str)
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--parse-answers">Code Snippet 5</a></span>:
  Parse all answers to the post
</div>

We now have all the required components to write an org file for the stackoverflow question now. Let's compile everything and write it to a file.

<a id="code-snippet--full-compile"></a>
```python
title_lowercase = question_title.lower()
filename = f"/Users/cmehta/Documents/org/roam/org/{'_'.join(title_lowercase.split()).removesuffix('?')}.org"
full_file_str = (f":PROPERTIES:\n"
                 f":ID: {'-'.join(title_lowercase.split())}\n"
                 f":END:\n"
                 f"#+title: {question_title}\n\n"
                 f"{question_org_str}\n"
                 f"{answer_org_str}")
with open(f"{filename}", "w") as f:
    f.write(full_file_str)
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--full-compile">Code Snippet 6</a></span>:
  Compile all the elements and write to an org file
</div>

We can now create a bookmarklet that we can install in the browser. Then, we can just click on whatever stackoverflow page we want to save for future reference.

```text
$.ajax({
    type: "POST",
    url: "/Users/cmehta/Documents/org/roam/org/stackoverflow_parser.py " + document.URL,
    crossdomain: true,
}).done(function() {
    alert("Saved to roam!");
});
```
