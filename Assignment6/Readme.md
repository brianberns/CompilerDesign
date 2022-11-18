[Assignment 6: Taipan: Checking and inferring types](https://course.ccs.neu.edu/cs4410sp21/hw_taipan_assignment.html)

Relevant lectures and other material:

* [Lecture 11: Type Checking](https://course.ccs.neu.edu/cs4410sp21/lec_type-checking_notes.html)
* [Lecture 12: Type Inference](https://course.ccs.neu.edu/cs4410sp21/lec_type-inference_notes.html)
* [Write You a Haskell](https://web.archive.org/web/20211218201109/http://dev.stephendiehl.com/fun/WYAH.pdf) - Internet Archive version, since the original website seems to be permanently offline.

This is a **big** step up from Assignment 5. Type inference is quite difficult to get right, and the course materials have some gaps. Here are a few of the major issues I ran into:

* The [type checking rules](https://course.ccs.neu.edu/cs4410sp21/lec_type-checking_notes.html#%28part._.Functions_and_function_calls%29) don't handle calls to polymorphic functions. For this, I had to use unification, as described in the type inference lecture.

* The type inference "[occurs check](https://course.ccs.neu.edu/cs4410sp21/lec_type-inference_notes.html#%28part._.Unification%29)" should *not* prohibit unification of a type variable with itself. In other words, `'A` unifies with `'A`, even though "the variable we're trying to constrain appears within its constraint".

* It doesn't make any sense to [instantiate a function's type scheme when inferring its type](https://course.ccs.neu.edu/cs4410sp21/lec_type-inference_notes.html#%28part._.Inference_and_.Generalization%29). Instead, I had to invent a "`preinstantiate`" function at this step. Scheme instantiation happens later, when applying the function.

* The comment in following example is incorrect:

```
def ab_bool(a, b): # should have scheme Forall 'A, 'B, ('A, 'B -> Bool)
  isnum(f(a)) && f(b)
```

The correct scheme is `Forall 'A, ('A, Bool -> Bool)`. This wasn't a major mistake, but it confused me for a while.