open System

let bisect (func: float -> float) (left: float) (right: float) (tolerance: float) =
    let rec search left right =
        let mid = (left + right) / 2.0
        if (right - left) / 2.0 < tolerance then mid
        elif func(mid) = 0.0 then mid
        elif func(left) * func(mid) < 0.0 then search left mid
        else search mid right
    search left right

let fixedPoint (transform: float -> float) (initial: float) (tolerance: float) (maxIterations: int) =
    let rec iterate current iter =
        let next = transform(current)
        if abs(next - current) < tolerance then next
        elif iter >= maxIterations then failwith "Метод итераций не сошелся"
        else iterate next (iter + 1)
    iterate initial 0

let newtonRaphson (func: float -> float) (deriv: float -> float) (initial: float) (tolerance: float) (maxIterations: int) =
    let rec iterate current iter =
        let next = current - func(current) / deriv(current)
        if abs(next - current) < tolerance then next
        elif iter >= maxIterations then failwith "Метод Ньютона не сошелся"
        else iterate next (iter + 1)
    iterate initial 0

let equation1 x = 0.1 * x ** 2.0 - x * log x
let deriv1 x = 0.2 * x - log x - 1.0
let transform1 x = exp(0.1 * x)

let equation2 x = tan x - (1.0/3.0) * (tan x) ** 3.0 + (1.0/5.0) * (tan x) ** 5.0 - 1.0/3.0
let deriv2 x = let secX = 1.0 / cos x in secX ** 2.0 - secX ** 4.0 * tan x + secX ** 6.0 * (tan x) ** 3.0
let transform2 x = atan((3.0 * x + 1.0) ** (1.0/3.0))

let equation3 x = acos x - sqrt(1.0 - 0.3 * x ** 3.0)
let deriv3 x = -1.0 / sqrt(1.0 - x ** 2.0) + (0.45 * x ** 2.0) / sqrt(1.0 - 0.3 * x ** 3.0)
let transform3 x = cos(sqrt(1.0 - 0.3 * x ** 3.0))

let rootBisect1 = bisect equation1 1.0 2.0 1e-5
let rootNewton1 = newtonRaphson equation1 deriv1 1.5 1e-5 1000
let rootFixedPoint1 = fixedPoint transform1 1.5 1e-5 1000

let rootBisect2 = bisect equation2 0.0 0.8 1e-5
let rootNewton2 = newtonRaphson equation2 deriv2 0.4 1e-5 1000
let rootFixedPoint2 = fixedPoint transform2 0.4 1e-5 1000

let rootBisect3 = bisect equation3 0.0 1.0 1e-5
let rootNewton3 = newtonRaphson equation3 deriv3 0.5 1e-5 1000
let rootFixedPoint3 = fixedPoint transform3 0.5 1e-5 1000

printfn "%-12s | %-50s | %-10s" "Метод" "Уравнение" "Корень"
printfn "%-12s | %-50s | %-10s" "------------" "--------------------------------------------------" "----------"
printfn "%-12s | %-50s | %-10f" "Дихотомия" "0.1x^2 - x ln x = 0" rootBisect1
printfn "%-12s | %-50s | %-10f" "Ньютон" "0.1x^2 - x ln x = 0" rootNewton1
printfn "%-12s | %-50s | %-10f" "Итерации" "0.1x^2 - x ln x = 0" rootFixedPoint1
printfn "%-12s | %-50s | %-10f" "Дихотомия" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootBisect2
printfn "%-12s | %-50s | %-10f" "Ньютон" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootNewton2
printfn "%-12s | %-50s | %-10f" "Итерации" "tan x - (1/3) tan^3 x + (1/5) tan^5 x - 1/3 = 0" rootFixedPoint2
printfn "%-12s | %-50s | %-10f" "Дихотомия" "arccos x - sqrt(1 - 0.3x^3) = 0" rootBisect3
printfn "%-12s | %-50s | %-10f" "Ньютон" "arccos x - sqrt(1 - 0.3x^3) = 0" rootNewton3
