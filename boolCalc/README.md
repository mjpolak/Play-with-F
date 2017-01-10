# boolCalc
### What is it?
Quite good logic expression interpreter. 
Made fo learning pruposes.

### How it works?

It takes single line expression of logic and creates truth tables for all sub expressions.
#### Example:
##### Input:
`(a&~b)|(~a&b)`
##### Output:
```
 a  b  ~a  ~a&b  ~b  a&~b  (a&~b)|(~a&b)
-----------------------------------------
 0  0   1    0    1    0         0
 1  0   0    0    1    1         1
 0  1   1    1    0    0         1
 1  1   0    0    0    0         0
```
Last column hold result for whole expression.

### Expression Structure

Expressions can contain:
* Logic value (e.g. `a`,`b`,`c`)
* Operators:
	* `~` - Negation (put it before Logic value, e.g. `~b`)
	* `&` - And gate (e.g. `a&b`)
	* `|` - Or logic (e.g. `a|b`)
	* `=` - Equal logic (e.g `a=b`)
	* `<` and `>` - Left and right Implication (e.g `a>b` or `a<b`)
	* `^` - XOR operator (e.g. `a^b`
* `( and )` - parenthesis  controls order of processing logic (e. g. `a&(b|c)` , b or c will be evaluated first)
* Spaces - ignored

#### Example inputs

* `(a^b)=((a&~b)|(~a&b))`  	- xor tester
* `(v>z) = ~(v&~z)`			- implication test