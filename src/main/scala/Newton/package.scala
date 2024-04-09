import Newton.Expr

package object Newton {

  //Definiciones y funciones complementarias:

  trait Expr
  case class Numero (d: Double) extends Expr
  case class Atomo (x: Char) extends Expr
  case class Suma (e1:Expr, e2:Expr) extends Expr
  case class Prod (e1:Expr, e2:Expr)  extends Expr
  case class Resta (e1:Expr, e2:Expr) extends Expr
  case class Div (e1:Expr, e2:Expr) extends Expr
  case class Expo (e1:Expr, e2:Expr) extends Expr
  case class Logaritmo (e1:Expr) extends Expr

  def buenaAprox(f:Expr, a:Atomo, d:Double): Boolean={
    evaluar(f,a,d) < 0.001
  }


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

  //funcion evaluar

  def evaluar(expr: Expr, a:Atomo, v:Double): Double = expr match {
    case Numero(d) => d
    case Atomo(x) => if(x==a.x) v else throw new IllegalAccessException("La expresion contiene un atomo diferente al proporcionado")
    case Suma(e1,e2) => evaluar(e1,a,v) + evaluar(e2,a,v)
    case Prod(e1,e2) => evaluar(e1,a,v) * evaluar(e2,a,v)
    case Resta(e1,e2) => evaluar(e1,a,v) - evaluar(e2,a,v)
    case Div(e1,e2) => evaluar(e1,a,v) / evaluar(e2,a,v)
    case Expo(e1,e2) =>{
      val base = evaluar(e1,a,v)
      val exponente = evaluar(e2,a,v)

      if (exponente == 0) 1.0
      else base * evaluar(Expo(e1,Numero(exponente-1)),a,v)
    }
    case Logaritmo(e1) => math.log(evaluar(e1,a,v))
  }

  //funcion limpiar

  def limpiar(f: Expr): Expr = f match {
    case Numero(0) => Numero(0)
    case Numero(1) => Numero(1)
    case Suma(e1, e2) => {
      val limpio1 = limpiar(e1)
      val limpio2 = limpiar(e2)
      (limpio1, limpio2) match {
        case (Numero(0), expr) => expr
        case (expr, Numero(0)) => expr
        case (Numero(d), Numero(d2)) => Numero(d + d2)
        case _ => Suma(limpio1, limpio2)
      }
    }
    case Prod(e1, e2) => {
      val limpio1 = limpiar(e1)
      val limpio2 = limpiar(e2)
      (limpio1, limpio2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(0)) => Numero(0)
        case (Numero(1), expr) => expr
        case (expr, Numero(1)) => expr
        case (Numero(d), Numero(d2)) => Numero(d * d2)
        case _ => Prod(limpio1, limpio2)
      }
    }
    case Resta(e1, e2) => {
      val limpio1 = limpiar(e1)
      val limpio2 = limpiar(e2)
      (limpio1, limpio2) match {
        case (_, Numero(0)) => limpio1
        case (Numero(0), expr) => Prod(Numero(-1), expr)
        case (Numero(d), Numero(d2)) => Numero(d - d2)
        case _ => Resta(limpio1, limpio2)
      }
    }
    case Div(e1, e2) => {
      val limpio1 = limpiar(e1)
      val limpio2 = limpiar(e2)
      (limpio1, limpio2) match {
        case (Numero(0), _) => Numero(0)
        case (_, Numero(1)) => limpio1
        case (Numero(d), Numero(d2)) => Numero(d / d2)
        case _ => Div(limpio1, limpio2)
      }
    }
    case Expo(e1, e2) => {
      val limpio1 = limpiar(e1)
      val limpio2 = limpiar(e2)
      (limpio1, limpio2) match {
        case (_, Numero(0)) => Numero(1)
        case (_, Numero(1)) => limpio1
        case (Numero(0), _) => Numero(0)
        case (Numero(1), _) => Numero(1)
        case (Numero(d), Numero(d2)) => Numero(math.pow(d, d2))
        case _ => Expo(limpio1, limpio2)
      }
    }
    case Logaritmo(e1) => {
      val limpio1 = limpiar(e1)
      limpio1 match {
        case Numero(1) => Numero(0)
        case _ => Logaritmo(limpio1)
      }
    }
    case _ => f
  }

  //funcion raizNewton

/*
  def raizNewton(f: Expr, a:Atomo, x0:Double, ba:(Expr,Atomo,Double) => Boolean): Double={

  }


 */




}