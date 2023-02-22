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
$ cat testData/linear.txt | dotnet run 5 5 1 2 3
1 | 2 | 3 | 
x:      4.0000, y:      4.0000 | x:      4.0000, y:      4.0384 | x:      4.0000, y:      4.0000 | 
x:      4.2000, y:      4.2000 | x:      4.2000, y:      4.1565 | x:      4.2000, y:      4.2000 | 
x:      4.4000, y:      4.4000 | x:      4.4000, y:      4.2692 | x:      4.4000, y:      4.4000 | 
x:      4.6000, y:      4.6000 | x:      4.6000, y:      4.3768 | x:      4.6000, y:      4.6000 | 
x:      4.8000, y:      4.8000 | x:      4.8000, y:      4.4799 | x:      4.8000, y:      4.8000 | 
1 | 2 | 3 | 
x:      5.0000, y:      5.0000 | x:      5.0000, y:      5.2345 | x:      5.0000, y:      5.0000 | 
x:      5.4000, y:      5.4000 | x:      5.4000, y:      5.5375 | x:      5.4000, y:      5.4000 | 
x:      5.8000, y:      5.8000 | x:      5.8000, y:      5.8189 | x:      5.8000, y:      5.8000 | 
x:      6.2000, y:      6.2000 | x:      6.2000, y:      6.0814 | x:      6.2000, y:      6.2000 | 
x:      6.6000, y:      6.6000 | x:      6.6000, y:      6.3276 | x:      6.6000, y:      6.6000 | 
1 | 2 | 3 | 
x:      7.0000, y:      7.0000 | x:      7.0000, y:      7.4095 | x:      7.0000, y:      7.0000 | 
x:      7.6000, y:      7.6000 | x:      7.6000, y:      7.8867 | x:      7.6000, y:      7.6000 | 
x:      8.2000, y:      8.2000 | x:      8.2000, y:      8.3277 | x:      8.2000, y:      8.2000 | 
x:      8.8000, y:      8.8000 | x:      8.8000, y:      8.7375 | x:      8.8000, y:      8.8000 | 
x:      9.4000, y:      9.4000 | x:      9.4000, y:      9.1202 | x:      9.4000, y:      9.4000 | 
```

### Выводы 
Весь мир сейчас идёт наоборот. Хотя написание лаб не оставляет нас, но нельзя запрягать телегу посреди лошади. Что бы мы ни делали — получается одно и то же: либо КПСС, либо автомат Калашникова. Я далёк от мысли…
