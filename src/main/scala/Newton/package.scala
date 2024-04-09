import Newton.Expr

package object Newton {

  trait Expr
  case class Numero (d: Double) extends Expr
  case class Atomo (x: Char) extends Expr
  case class Suma (e1:Expr, e2:Expr) extends Expr
  case class Prod (e1:Expr, e2:Expr)  extends Expr
  case class Resta (e1:Expr, e2:Expr) extends Expr
  case class Div (e1:Expr, e2:Expr) extends Expr
  case class Expo (e1:Expr, e2:Expr) extends Expr
  case class Logaritmo (e1:Expr) extends Expr


  def mostrar (e: Expr) : String = {
    e match {
      case Numero(d) => d.toString
      case Atomo(x) => x.toString
      case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
      case Prod(e1,e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
      case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
      case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
      case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
      case Logaritmo(e1) => s"(lg(${mostrar(e1)}))"

    }
  }

  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0) // Derivada de una constante es cero
    case Atomo(x) => if (x == a.x) Numero(1) else Numero(0) // Derivada de una variable es 1 si es la variable deseada, 0 en caso contrario
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a)) // Regla de la suma: df(x) + dg(x)
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))) // Regla del producto: f'(x)g(x) + f(x)g'(x)
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a)) // Regla de la resta: df(x) - dg(x)
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2))) // Regla del cociente: (f'(x)g(x) - f(x)g'(x)) / (g(x)^2)
    case Expo(e1, e2) => Prod(Expo(e1,e2),Suma(Div(Prod(derivar(e1,a),e2),e1),Prod(derivar(e2, a), Logaritmo(e1))))
    case Logaritmo(e1) => Div(derivar(e1, a), e1) // Regla del logaritmo: f'(x) / f(x)
  }





}