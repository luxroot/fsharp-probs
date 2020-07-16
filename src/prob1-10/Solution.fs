module prob1_10

let rec myLast lst =
    match lst with
    | [x] -> x
    | hd :: tl -> myLast tl
    | [] -> failwith "Empty array"

let rec myButLast lst =
    match lst with
    | [] | [_] -> failwith "Not enough elements"
    | [x; _] -> x
    | hd :: tl -> myButLast tl

let rec elementAt lst idx =
    if idx = 1 then
        List.head lst
    else
        elementAt (List.tail lst) (idx-1)

let rec myLength lst =
    match lst with
    | _ :: tl -> 1 + (myLength tl)
    | [] -> 0

let reverse = List.rev

