import scala.quoted.*

object Macros {
  inline def serialize(x: Any): Unit = ${serializeImpl('x)}

  def serializeImpl(x: Expr[Any])(using Quotes): Expr[Unit] = {
    import quotes.reflect.*

    def owners(s: Symbol): List[Symbol] = s :: List.unfold(s)(s1 => Option.when(s1.maybeOwner != Symbol.noSymbol)((s1.maybeOwner, s1.maybeOwner)))

    val symbol = x.asTerm.underlying.symbol
    val rhs = symbol.tree match {
      case ValDef(_, _, Some(rhs)) => rhs
    }

    val traverser = new TreeTraverser {
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case Ident(name) =>
            val symbol1 = tree.symbol
            val pos1 = symbol1.pos.get
            println(s"identifier: $name, defined inside lambda: ${owners(symbol1).contains(symbol)}, defined in current file: ${pos1.sourceFile == SourceFile.current}")

          case _ =>
        }

        super.traverseTree(tree)(owner)
      }
    }

    traverser.traverseTree(rhs)(rhs.symbol)

    '{()}
  }
}
