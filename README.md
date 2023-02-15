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

### Выводы 

