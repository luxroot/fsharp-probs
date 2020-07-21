﻿module prob1_10

let rec myLast lst =
    match lst with
    | [] -> failwith "Empty array"
    | [ x ] -> x
    | hd :: tl -> myLast tl

let rec myButLast lst =
    match lst with
    | []
    | [ _ ] -> failwith "Not enough elements"
    | [ x; _ ] -> x
    | _ :: tl -> myButLast tl

let rec elementAt lst idx =
    if idx = 1 then List.head lst else elementAt (List.tail lst) (idx - 1)

let rec myLength lst =
    match lst with
    | _ :: tl -> 1 + (myLength tl)
    | [] -> 0

let reverse = List.rev

let isPalindrome lst = lst = (reverse lst)

type 'a NestedList =
    | List of 'a NestedList list
    | Elem of 'a

let flatten lst =
    let rec flatten_aux acc =
        function
        | Elem x -> x :: acc
        | List inner -> List.foldBack (fun x acc -> flatten_aux acc x) inner acc
    flatten_aux [] lst

let compress lst =
    List.foldBack (fun x acc ->
        if List.isEmpty acc then [ x ]
        elif List.head acc = x then acc
        else x :: acc) lst []
    
let pack lst =
    List.foldBack (fun x acc ->
        if List.isEmpty acc then [ [ x ] ]
        else
        let firstList = List.head acc
        if List.head firstList = x then (x::firstList)::(List.tail acc)
        else [ x ] :: acc) lst []
    
let encode lst =
    lst |> pack |> (List.map (fun x -> (List.length x, List.head x)))