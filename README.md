# logfm

Faux Monadic Logger

----

# Faux Monadic Logger

This was an attempt to create a monadic logger using a type Union
as the mechanism for storing return values.
While I could get it working, it would be hard to make it
generally useful and it certainly not very performant.
For comprehensions worked but relying on the type Union made it
hard to integrate "functional" code using this logger with
non-functional code; while I could do it I could not expect anyone
else to (want to) do it.

This was built using Scala 11.X so there were no builtin union types.
Rather I used a modified version of com.gensler.scalavro.util.Union.

  Provides a facility for specifying unboxed union types of arbitrary
  ordinality in the Scala type system.
    This clever technique was proposed by Miles Sabin:
    http://chuusai.com/2011/06/09/scala-union-types-curry-howard
    and strongly based upon: com.gensler.scalavro.util.Union
