module ProblemSecond

type 'a Encoding =
    | Multiple of int * 'a
    | Single of 'a

let encodeModified lst =
    let collecter x =
        function
        | [] -> [ Single x ]
        | Single item :: tl when x = item -> Multiple(2, item) :: tl
        | Single item :: tl -> (Single x) :: (Single item) :: tl
        | Multiple (times, item) :: tl when x = item -> (Multiple(times + 1, item)) :: tl
        | Multiple (times, item) :: tl -> (Single x) :: (Multiple(times, item)) :: tl
        | _ -> failwith "Error"

    List.foldBack collecter lst []

let decodeModified lst =
    let unzip =
        function
        | Single item -> [ item ]
        | Multiple (times, item) -> [ for _ in 1 .. times -> item ]

    List.collect unzip lst

let encodeDirect = encodeModified

let dupli lst = List.collect (fun x -> [ x; x ]) lst

let repli lst times =
    List.collect (fun x -> [ for _ in 1 .. times -> x ]) lst

let dropEvery lst interval =
    (List.mapi (fun idx item -> if (idx + 1) % interval <> 0 then Some item else None) lst)
    |> List.choose id

let split lst n =
    let rec take cur =
        function
        | hd :: tl -> if cur > 0 then hd :: take (cur - 1) tl else []
        | _ -> []

    let rec drop cur l =
        match cur, l with
        | 0, _ -> l
        | _, _ :: tl -> drop (cur - 1) tl
        | _ -> failwith "Wrong"

    take n lst, drop n lst

let slice lst s e =
    let rec take cur =
        function
        | hd :: tl -> if cur > 0 then hd :: take (cur - 1) tl else []
        | _ -> []

    let rec drop cur l =
        match cur, l with
        | 0, _ -> l
        | _, _ :: tl -> drop (cur - 1) tl
        | _ -> failwith "Wrong"

    lst |> drop (s - 1) |> take (e - s + 1)

let rotate lst n =
    let l = List.length lst
    let a = slice lst 1 ((n + l) % l)
    let b = slice lst ((l + n + 1) % l) l
    b @ a

let removeAt idx lst =
    let a, b = split lst idx
    match b with
    | hd :: tl -> hd, a @ tl
    | [] -> failwith "Error"
