open System

let bis (f: float -> float) (l: float) (r: float) (tol: float) =
    let rec s l r =
        let m = (l + r) / 2.0
        if (r - l) / 2.0 < tol then m
        elif f(m) = 0.0 then m
        elif f(l) * f(m) < 0.0 then s l m
        else s m r
    s l r

let fix (t: float -> float) (init: float) (tol: float) (maxIter: int) =
    let rec iter curr i =
        let next = t(curr)
        if abs(next - curr) < tol then next
        elif i >= maxIter then failwith "Не сошлось"
        else iter next (i + 1)
    iter init 0

let newton (f: float -> float) (df: float -> float) (init: float) (tol: float) (maxIter: int) =
    let rec iter curr i =
        let next = curr - f(curr) / df(curr)
        if abs(next - curr) < tol then next
        elif i >= maxIter then failwith "Не сошлось"
        else iter next (i + 1)
    iter init 0

let eq1 x = 0.1 * x ** 2.0 - x * log x
let df1 x = 0.2 * x - log x - 1.0
let tr1 x = exp(0.1 * x)

let eq2 x = tan x - (1.0/3.0) * (tan x) ** 3.0 + (1.0/5.0) * (tan x) ** 5.0 - 1.0/3.0
let df2 x = let secX = 1.0 / cos x in secX ** 2.0 - secX ** 4.0 * tan x + secX ** 6.0 * (tan x) ** 3.0
let tr2 x = atan((3.0 * x + 1.0) ** (1.0/3.0))

let eq3 x = acos x - sqrt(1.0 - 0.3 * x ** 3.0)
let df3 x = -1.0 / sqrt(1.0 - x ** 2.0) + (0.45 * x ** 2.0) / sqrt(1.0 - 0.3 * x ** 3.0)
let tr3 x = cos(sqrt(1.0 - 0.3 * x ** 3.0))

let rootBis1 = bis eq1 1.0 2.0 1e-5
let rootNew1 = newton eq1 df1 1.5 1e-5 1000
let rootFix1 = fix tr1 1.5 1e-5 1000

let rootBis2 = bis eq2 0.0 0.8 1e-5
let rootNew2 = newton eq2 df2 0.4 1e-5 1000
let rootFix2 = fix tr2 0.4 1e-5 1000

let rootBis3 = bis eq3 0.0 1.0 1e-5
let rootNew3 = newton eq3 df3 0.5 1e-5 1000
let rootFix3 = fix tr3 0.5 1e-5 1000

printfn "%-12s | %-50s | %-10s" "Метод" "Уравнение" "Корень"
printfn "%-12s | %-50s | %-10s" "------------" "--------------------------------------------------" "----------"
printfn "%-12s | %-50s | %-10f" "Дихотомия" "0.1x^2 - x ln x = 0" rootBis1
printfn "%-12s | %-50s | %-10f" "Ньютон" "0.1x^2 - x ln x = 0" rootNew1
printfn "%-12s | %-50s | %-10f" "Итерации" "0.1x^2 - x ln x = 0" rootFix1
printfn "%-12s | %-50s | %-10f" "Дихотомия" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootBis2
printfn "%-12s | %-50s | %-10f" "Ньютон" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootNew2
printfn "%-12s | %-50s | %-10f" "Итерации" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootFix2
printfn "%-12s | %-50s | %-10f" "Дихотомия" "arccos x - sqrt(1 - 0.3x^3) = 0" rootBis3
printfn "%-12s | %-50s | %-10f" "Ньютон" "arccos x - sqrt(1 - 0.3x^3) = 0" rootNew3

