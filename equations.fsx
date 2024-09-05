open System

// Встроенная функция F#
let builtinExp2x x = Math.Exp(2.0 * x)

// Наивный способ вычисления ряда Тейлора
let naiveTaylorExp2x x eps =
    let rec loop n term sum =
        if abs term < eps then (sum, n)
        else
            let term = term * (2.0 * x) / float n
            loop (n + 1) term (sum + term)
    loop 1 (2.0 * x) 1.0

// Умный способ вычисления ряда Тейлора
let smartTaylorExp2x x eps =
    let mutable n = 0
    let mutable term = 2.0 * x
    let mutable sum = 1.0
    while abs term >= eps do
        n <- n + 1
        term <- term * 2.0 * x / float n
        sum <- sum + term
    sum, n

// Интервал [0.1, 0.6] с шагом 0.1
let interval = [0.1 .. 0.1 .. 0.6]
let eps = 1e-10

// Вывод результатов
printfn "x\tBuiltin\t\tNaive\t\tNaive Terms\tSmart\t\tSmart Terms"
for x in interval do
    let builtin = builtinExp2x x
    let (naive, naiveTerms) = naiveTaylorExp2x x eps
    let (smart, smartTerms) = smartTaylorExp2x x eps
    printfn "%.1f\t%.10f\t%.10f\t%d\t\t%.10f\t%d" x builtin naive naiveTerms smart smartTerms
