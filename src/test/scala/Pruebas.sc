import Newton._

// Definicion de las expresiones para los casos de prueba de mostrar y derivar :

val expr1=Suma(Atomo('x' ) , Numero ( 2 ) )
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Suma(expr1, Expo(expr2,Numero(5)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1,expr2), Resta(expr3,expr4))
val expr6 = Expo (Atomo('x'), Numero(3))
val expr7 = Prod(Resta(Expo(Atomo('x'),Numero(2)),Numero(2)), Numero(5))
val expr8 = Div(Prod(Numero(5),Numero(2)),Numero(3))
val expr9 = Div(Logaritmo(Atomo('x')),Prod(Numero(5),Numero(5)))
val expr10 = Div(Resta(Numero(3),Numero(2)),Suma(Numero(3),Numero(2)))

//Casos de prueba mostrar

mostrar(expr1) //Resultado esperado: (x + 2.0)
mostrar(expr2) //Resultado esperado: (x * x)
mostrar(expr3) //Resultado esperado: ((x + 2.0) + ((x * x) ^ 5.0))
mostrar(expr4) // Resultado esperado: (lg(x))
mostrar(expr5) //Resultado esperado: (((x + 2.0) / (x * x)) * (((x + 2.0) + ((x * x) ^ 5.0)) - (lg(x))))
mostrar(expr6) //Resultado esperado:  (x ^ 3.0)
mostrar(expr7) //Resultado esperado:  (((x ^ 2.0) - 2.0) * 5.0)
mostrar(expr8) //Resultado esperado:  ((5.0 * 2.0) / 3.0)
mostrar(expr9) //Resultado esperado:  ((lg(x)) / (5.0 * 5.0))
mostrar(expr10) //Resultado esperado:  ((3.0 - 2.0) / (3.0 + 2.0))

//Casos de prueba derivar

mostrar(derivar(expr6, Atomo('x'))) // ((x ^ 3.0) * (((1.0 * 3.0) / x) + (0.0 * lg(x))))
mostrar(derivar(expr2, Atomo('x'))) // ((1.0 * x) + (x * 1.0))
mostrar(derivar(expr2, Atomo('y'))) // ((0.0 * x) + (x * 0.0))
mostrar(derivar(expr8, Atomo('x'))) // (((((0.0 * 2.0) + (5.0 * 0.0)) * 3.0) - ((5.0 * 2.0) * 0.0)) / (3.0 ^ 2.0))
mostrar(derivar(expr10,Atomo('x'))) // ((((0.0 - 0.0) * (3.0 + 2.0)) - ((3.0 - 2.0) * (0.0 + 0.0))) / ((3.0 + 2.0) ^ 2.0))


//Pruebas raizNewton

val e1 = Resta(Prod(Atomo('x'),Atomo('x')), Numero(2.0))
val e2 = Resta(Prod(Atomo('x'), Atomo('x')), Numero(4.0))
val e3 = Suma(Resta(Prod(Atomo('x'),Atomo('x')),Numero(4.0)), Prod(Numero(3.0), Atomo('x')))

raizNewton(e1,Atomo('x'), 2.0, buenaAprox)
raizNewton(e2,Atomo('x'), 2.0, buenaAprox)
raizNewton(e3,Atomo('x'), 2.0, buenaAprox)