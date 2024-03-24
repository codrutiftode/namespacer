# Namespacer (::)

RStudio add-on for adding explicit namespace declarations to functions.

### Installation

Follow the instructions in the INSTALL file.

### Usage

Two options:

1)  Using the "Addins -\> Namespacerise this file!" command in RStudio.

2)  Importing the add-on with <code>library(namespacer)</code>. Then, running <code>addns()</code>

### Example

Before:

```
library(some_lib) 
some_func()
```

After:

``` 
library(some_lib) 
some_lib::some_func()
```
