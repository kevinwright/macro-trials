package macrotrials

import scala.quoted.*

object ShowTree:
  inline def showTree[A](inline a: A): String = ${showTreeImpl[A]('{ a })}

  def showTreeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    Expr(a.show)
