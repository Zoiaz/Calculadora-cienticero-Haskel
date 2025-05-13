import qualified Calculadora.Parsing as Pars

main :: IO ()
main = do
    putStrLn "==============================="
    putStrLn "CALCULADORA CIENTÍFICA HASKELL"
    putStrLn "Escribe una operación como: 3 + 4 * 2"
    putStrLn "Escribe 'help' para ver funciones disponibles"
    putStrLn "Escribe 'exit' para salir"
    putStrLn "===============================\n"
    loop

loop :: IO ()
loop = do
    putStr "» "
    input <- getLine
    case input of
        "exit" -> despedida
        "salir" -> despedida
        "quit" -> despedida
        "q" -> despedida
        "help" -> mostrarAyuda >> loop
        _ -> case Pars.evaluarExpresion input of
                Left err -> putStrLn ("Error: " ++ err) >> loop
                Right val -> putStrLn ("= " ++ show val) >> loop

despedida :: IO ()
despedida = putStrLn "Gracias por usar la calculadora científica. ¡Hasta luego! "

mostrarAyuda :: IO ()
mostrarAyuda = do
    putStrLn "\n=== Ayuda de uso ==="
    putStrLn "La calculadora acepta operaciones y funciones matemáticas comunes."
    putStrLn ""
    putStrLn "Operadores válidos:"
    putStrLn "  x + y     suma"
    putStrLn "  x - y     resta"
    putStrLn "  x * y     multiplicación"
    putStrLn "  x / y     división"
    putStrLn "  x ^ y     potencia (x elevado a y)"
    putStrLn ""
    putStrLn "Funciones disponibles (requieren paréntesis):"
    putStrLn "  sqrt(x)    raíz cuadrada"
    putStrLn "  fact(x)    factorial (solo enteros >= 0)"
    putStrLn "  sin(x)     seno (en grados)"
    putStrLn "  cos(x)     coseno (en grados)"
    putStrLn "  tan(x)     tangente (en grados)"
    putStrLn "  sinh(x)    seno hiperbólico"
    putStrLn "  cosh(x)    coseno hiperbólico"
    putStrLn "  tanh(x)    tangente hiperbólica"
    putStrLn "  log(x)     logaritmo natural"
    putStrLn "  log10(x)   logaritmo base 10"
    putStrLn ""
    putStrLn "Estadística:"
    putStrLn "  mean(x)    promedio"
    putStrLn "  median(x)  mediana"
    putStrLn "  stddev(x)  desviación estándar"
    putStrLn ""
    putStrLn "Combinatoria:"
    putStrLn "  perm(n, r)     permutaciones"
    putStrLn "  comb(n, r)     combinaciones"
    putStrLn "  binom(n, r)    coeficiente binomial"
    putStrLn ""
    putStrLn "Comandos especiales:"
    putStrLn "  help       muestra esta ayuda"
    putStrLn "  exit       cierra la calculadora"
    putStrLn ""
    loop





