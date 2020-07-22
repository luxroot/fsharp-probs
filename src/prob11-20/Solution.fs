module prob11_20

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

let dropEvery lst interval = failwith "Not implemented"
