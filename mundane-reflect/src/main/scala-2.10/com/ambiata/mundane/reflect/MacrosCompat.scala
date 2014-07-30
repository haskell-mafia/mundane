package com.ambiata.mundane.reflect

/**
 * This trait is introduced to reduce the number of warnings when using
 *
 * Scala 2.11 where scala.reflect.macros.blackbox.Context must be used instead of just scala.reflect.macros.Context
 */
trait MacrosCompat {
  import language.experimental.macros
  type Context = scala.reflect.macros.blackbox.Context
}


