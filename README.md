# chisai
A (esoteric) golfing language

```
Single (require 1 stack argument): a
(.) -> output a
(d) -> duplicate a
(*) -> dequote a
       ex: [ (10 +) ] -> [ 10 + ]
(@) -> apply a
       ex: [ 10 10 + ] -> [ 20 ]

Double (require 2 stack arguments): a b
(+) -> a + b
(-) -> b - a
(⋅) -> a ⋅ b
(÷) -> a ÷ b
```