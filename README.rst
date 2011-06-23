
Overview
========

The purspose of this project is to provide with a very simple to use parser generator with a natural C# syntax.
For example, a very simple expression language:::

         Define(() => Root, () => Expr);
         Define(() => Expr, () => Multiplication.FollowedBy("\\+".FollowedBy(Multiplication).Optional()));
         Define(() => Multiplication, () => Factor.FollowedBy("\\*".FollowedBy(Multiplication).Optional()));
         Define(() => Factor, () => Constant.Or("\\(".FollowedBy(Expr, "\\)")));
         Define(() => Constant, () => Digit.OneOrMore()); //> emit list of digits (1 or more)
         Define(() => Digit, () => "1|2|3|4|5|6|7|8|9|0".Terminal());

The resulting graph can be "flattened" and compiled easily using .NET Expression trees, for example.		 
		 
License
=======

(The MIT License)

Copyright (c) 2011 Marcus van Houdt

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


Acknowledgements
================

Parts of this code are based on Haskell code written by Martin Sulzmann.