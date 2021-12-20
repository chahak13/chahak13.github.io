+++
title = "Chunky Pandas - Read CSV in chunks"
author = ["Chahak Mehta"]
date = 2021-06-11
tags = ["python"]
draft = false
+++

Huge CSV files are a pain to read as they start overloading RAM a lot. The `read_csv` function provides the ability to read a file in chunks. This is potentially helpful to perform operations on such large files in parts. I'd like to look more into what all can be done using chunking.
