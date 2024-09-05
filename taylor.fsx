open System

// Встроенная функция F#
let f x = Math.Exp(2.0 * x)

// Интервал [0.0, 1.0] с шагом 0.1
let a = 0.0
let b = 1.0
let n = 10
let interval = [a .. (b - a) / float n .. b]

// Наивный способ вычисления ряда Тейлора
let nt x eps =
    let rec l k t sum =
        if abs t < eps then (sum, k)
        else
            let t = t * (2.0 * x) / float k
            l (k + 1) t (sum + t)
    l 1 (2.0 * x) 1.0

// Умный способ вычисления ряда Тейлора
let st x eps =
    let mutable k = 0
    let mutable t = 2.0 * x
    let mutable sum = 1.0
    while abs t >= eps do
        k <- k + 1
        t <- t * 2.0 * x / float k
        sum <- sum + t
    sum, k

// Вывод результатов
printfn "x\tBuiltin\t\tNaive\t\tNaive Terms\tSmart\t\tSmart Terms"
for x in interval do
    let b = f x
    let (n, ntk) = nt x eps
    let (s, stk) = st x eps
    printfn "%.1f\t%.10f\t%.10f\t%d\t\t%.10f\t%d" x b n ntk s stk
