+++
title = "Suppress warnings in python"
author = ["Chahak Mehta"]
date = 2021-07-14
tags = ["python"]
draft = false
+++

While working with packages like numpy and scipy, python can possibly give a lot of warnings. These warnings can be suppressed easily by using the warnings module in the python library.

```python
import warnings
warnings.filterwarnings("ignore")
```

And done. This should suppress all the warnings your code produces.

**NOTE OF CAUTION:** Do this _only_ if you know what you're doing and what warnings are being suppressed. Most of the times, having a warning means that something in your code can be improved. Hence, warnings should only be suppressed when there is enough information about the warning and it is an informed decision to suppress it.
