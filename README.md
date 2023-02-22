# Функциональное программирование
## Лабораторная работа 3

<b>Выполнил:</b> Коваленко Илья Русланович \
<b>Группа:</b> p34102 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Реализация
Функция читает из стандартного потока пока не будет пустая строка. 
В результате получаем список пар (x,y) 
```f#
let rec reader k (pList: (double * double) list) =
    let line = Console.ReadLine()

    if (not (isNull line) && line <> "") then
        let data = line.Split(";")

        if data.Length >= 2 && data[1] <> "" then
            let x = double data[0]
            let y = double data[1]

            match pList.Length with
            | 0 -> [ (x, y) ]
            | _ -> (x, y) :: List.truncate (k - 1) pList
        else
            reader k pList
    else
        pList

```
Полученный список точек обрабатывается, параллельно вычисляются функции, Печатается результат.\ 
Ожидается ввод новой пары чисел и алгоритм повторяется.

Код предоставляет три различные функции для аппроксимации набора двумерных точек с помощью различных моделей: линейной, логарифмической и сегментной. Функция `linear` возвращает функцию, которая подгоняет прямую линию к точкам, а `logarithm` возвращает функцию, которая подгоняет логарифмическую кривую к точкам. Функция `segment` возвращает функцию, которая аппроксимирует точки с помощью отрезков прямой.

```f#
let linear points =
    let sx = List.fold (fun state (x, _) -> state + x) 0. points
    let sxx = List.fold (fun state (x, _) -> state + x * x) 0. points
    let sy = List.fold (fun state (_, y) -> state + y) 0. points
    let sxy = List.fold (fun state (x, y) -> state + x * y) 0. points
    let n = points.Length
    let a = (sxy * (double n) - sx * sy) / (sxx * (double n) - sx * sx)
    let b = (sxx * sy - sx * sxy) / (sxx * (double n) - sx * sx)
    let f x = a * x + b
    f

let segment (points: list<double * double>) =
    let rec findBottomBorder i v =
        if i < points.Length then
            let x, _ = points[i]
            if x < v then i else findBottomBorder (i + 1) v
        else
            -1

    let rec findTopBorder i v =
        if i >= 0 then
            let x, _ = points[i]
            if x >= v then i else findTopBorder (i - 1) v
        else
            -1

    let f x =
        let top = findTopBorder (points.Length - 1) x
        let bottom = findBottomBorder 0 x

        if top = -1 then
            let _, yi = points[0]
            yi
        else if bottom = -1 then
            let _, yi = points[points.Length - 1]
            yi
        else
            let xi, yi = points[top]
            let xiPrev, yiPrev = points[bottom]
            let a = (yi - yiPrev) / (xi - xiPrev)
            let b = yi - a * xi
            a * x + b

    f

let logarifm (points: list<double * double>) =
    let sx = List.fold (fun state (x, _) -> state + log x) 0. points
    let sxx = List.fold (fun state (x, _) -> state + (log x) * (log x)) 0. points
    let sy = List.fold (fun state (_, y) -> state + y) 0. points
    let sxy = List.fold (fun state (x, y) -> state + (log x) * y) 0. points
    let n = points.Length
    let delta = sxx * (double n) - (sx * sx)
    let delta1 = sxy * (double n) - (sx * sy)
    let delta2 = sxx * sy - (sx * sxy)
    let a = delta1 / delta
    let b = delta2 / delta
    let f x = a * log x + b
    f
```

Функция `getFunc` принимает целое число id и список точек и возвращает функцию, соответствующую данному ID: `segment, logarifm или linear`. Функция `genPoint` принимает целое число n и список точек и возвращает функцию, которая генерирует n равноотстоящих друг от друга точек в диапазоне заданных точек.

```f#
let genPoint (n: int) points =
    match points with
    | (x2, _) :: (x1, _) :: _ ->
        let mult = (x2 - x1) / (double n)
        let getPoint (i: int) = x1 + (double i) * mult
        getPoint
    | _ -> fun _ -> 0
```

Функция `print` принимает массив кортежей, где первый элемент - функция, аппроксимирующая точки, а второй элемент - целочисленный ID. Она также принимает функцию, которая генерирует точки, и количество точек, которые нужно сгенерировать. Функция печатает координаты x и y каждой сгенерированной точки, аппроксимированной каждой из функций в массиве.

```
$ cat linear.txt | dotnet run 5 5 1 2 3

x:  4.00, y:  4.00 | x:  4.00, y:  4.04 | x:  4.00, y:  4.00 | 
x:  4.20, y:  4.20 | x:  4.20, y:  4.16 | x:  4.20, y:  4.20 | 
x:  4.40, y:  4.40 | x:  4.40, y:  4.27 | x:  4.40, y:  4.40 | 
x:  4.60, y:  4.60 | x:  4.60, y:  4.38 | x:  4.60, y:  4.60 | 
x:  4.80, y:  4.80 | x:  4.80, y:  4.48 | x:  4.80, y:  4.80 | 

x:  5.00, y:  5.00 | x:  5.00, y:  5.23 | x:  5.00, y:  5.00 | 
x:  5.40, y:  5.40 | x:  5.40, y:  5.54 | x:  5.40, y:  5.40 | 
x:  5.80, y:  5.80 | x:  5.80, y:  5.82 | x:  5.80, y:  5.80 | 
x:  6.20, y:  6.20 | x:  6.20, y:  6.08 | x:  6.20, y:  6.20 | 
x:  6.60, y:  6.60 | x:  6.60, y:  6.33 | x:  6.60, y:  6.60 | 

x:  7.00, y:  7.00 | x:  7.00, y:  7.41 | x:  7.00, y:  7.00 | 
x:  7.60, y:  7.60 | x:  7.60, y:  7.89 | x:  7.60, y:  7.60 | 
x:  8.20, y:  8.20 | x:  8.20, y:  8.33 | x:  8.20, y:  8.20 | 
x:  8.80, y:  8.80 | x:  8.80, y:  8.74 | x:  8.80, y:  8.80 | 
x:  9.40, y:  9.40 | x:  9.40, y:  9.12 | x:  9.40, y:  9.40 | 

```

### Выводы 
Весь мир сейчас идёт наоборот. Хотя написание лаб не оставляет нас, но нельзя запрягать телегу посреди лошади. Что бы мы ни делали — получается одно и то же: либо КПСС, либо автомат Калашникова. Я далёк от мысли…
