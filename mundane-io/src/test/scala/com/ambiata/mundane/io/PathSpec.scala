package com.ambiata.mundane.io

import org.specs2._
import org.scalacheck._
import Arbitraries._

class PathSpec extends Specification with ScalaCheck { def is = s2"""

Path
====

Paths are a recursive data structure, defined in terms of two base cases,
and a recursive case.


Folds
-----

  Folding over 'Root' base case should always (and only) cause root expression
  to be evaluated:

    ${ prop((x: Int) => Root.fold(x, ???, (_, _) => ???) ==== x) }

  Folding over 'Relative' base case should always (and only) cause relative
  expression to be evaluated:

    ${ prop((x: Int) => Relative.fold(???, x, (_, _) => ???) ==== x) }

  Folding over 'Component' recursive case should always (and only) cause
  component expression to be evaluated:

    ${ prop((x: Int, base: Path, name: FileName) =>
         Component(base, name).fold(???, ???, (b, n) => (b, n, x)) ==== ((base, name, x))) }

  'isRoot' should return true if and only if this 'Path' is the top level
  'Root' base case:

    ${ Root.isRoot ==== true }

    ${ Relative.isRoot ==== false }

    ${ prop((base: Path, name: FileName) => Component(base, name).isRoot ==== false ) }

  'isRelativeRoot' should return true if and only if this 'Path' is the top
  level 'Relative' base case:

    ${ Root.isRelativeRoot ==== false }

    ${ Relative.isRelativeRoot ==== true }

    ${ prop((base: Path, name: FileName) => Component(base, name).isRelativeRoot ==== false ) }

  'dirname' should return 'Root' or 'Relative' for the respective base cases (effectively
  a no-op), or return the base of 'Component' stripping the file name. Posix specifications
  for 'dirname(1) / dirname(3)' should be used to decide any ambiguity.

    ${ Root.dirname ==== Root }

    ${ Relative.dirname ==== Relative }

    ${ prop((base: Path, name: FileName) => Component(base, name).dirname ==== base ) }

    ${ (Root </> "usr" </> "local").dirname === (Root </> "usr") }

    ${ (Relative </> "usr" </> "local").dirname === (Relative </> "usr") }

    ${ (Root </> "home").dirname === Root }

    ${ (Relative </> "home").dirname === Relative }

  Note also that the result of dirname will always be a prefix of the starting value:

    ${ prop((p: Path) => p.startsWith(p.dirname)) }


  Calling 'parent' where a parent exists, returns 'Some' new path with the top filename
  component stripped off, otherwise it returns 'None'. This behaviour is identical to
  'dirname' except for the base 'Root', 'Relative' cases.

    ${ Root.parent ==== None }

    ${ Relative.parent ==== None }

    ${ prop((base: Path, name: FileName) => Component(base, name).parent ==== Some(base) ) }

    ${ prop((base: Path) => base.parent.isDefined ==> { base.parent ==== Some(base.dirname) })  }

"""
}
