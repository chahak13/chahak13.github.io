import sys
import re
import requests
from bs4 import BeautifulSoup as bs

url = sys.argv[1]
# url = "https://math.stackexchange.com/questions/1339709/how-to-derive-the-weak-form-of-the-pde"
base_url = url.split("/")[2]
response = requests.get(url)
if response.ok:
    soup = bs(response.text, features="html.parser")

question_header = soup.find('div', {'id': 'question-header'})
question_title = question_header.find('a', {'class': 'question-hyperlink'}).text.strip()
question_link = f"https://{base_url}{question_header.find('a', {'class': 'question-hyperlink'}).get('href')}"
# print((question_title, question_link))

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

question_org_str = f"* [[{question_link}][{question_title}]]\n\n{question_text}\nComments\n{question_comments}"
# print(question_org_str)

answers = []
for i, answer in enumerate(soup.find_all('div', {'class': 'answer'})):
    answer_post = answer.find('div', {'class': 'post-layout'})
    answer_cell = answer_post.find('div', {'class': 'answercell'}).find('div', {'class': 's-prose'})
    answer_str = parse_post(answer_cell)
    answer_comments = parse_comments(answer_post)
    answers.append(f"** Answer {i+1}\n\n{answer_str}\n Comments:\n{answer_comments}\n")

answer_org_str = '\n'.join(answers)
# print(answer_org_str)

import os

title_lowercase = question_title.lower()
filename = f"{os.path.dirname(__file__)}/{'_'.join(title_lowercase.split()).removesuffix('?')}.org"
full_file_str = (f":PROPERTIES:\n"
                 f":ID: {'-'.join(title_lowercase.split())}\n"
                 f":END:\n"
                 f"#+title: {question_title}\n\n"
                 f"{question_org_str}\n"
                 f"{answer_org_str}")
with open(f"{filename}", "w") as f:
    f.write(full_file_str)
