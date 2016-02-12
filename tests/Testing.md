# Testing README

### Running Tests
```scheme    
(load "tests")
(test)
```

### Creating Tests
In [```test.scm```](/Jupiter/test.scm) there is a function ```assert-equal``` that takes a file name and an
expected value and runs ```interpret``` on the test file.

1. Add test file in path ```tests/<assignment #>/<test file #>```. Take a looks in [```/tests```](/Jupiter/tests) for an example.

2. In [```test.scm```](/Jupiter/test.scm) add a line calling ```assert-equal``` on the file name from step 1 and pass the expected value.


### Desired Functionality
Eventually the goal is to beef up the ```test``` function. Here are some potential examples:
```scheme
(test '01)      ; tests files in tests/01/
(test '(02 03)) ; tests files in tests/02/ & tests/03/
```
Printing out respected vs. actually values would be nice too.
