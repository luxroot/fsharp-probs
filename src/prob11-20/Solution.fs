module prob1_10

type 'a Encoding = Multiple of int * 'a | Single of 'a

let decodeModified lst =
    let collecter x = function
        | [] -> [Single x]
        | Single item :: tl when x = item -> Multiple (2, item) :: tl
        | Single item :: tl -> (Single x) :: (Single item) :: tl
        | Multiple (times, item) :: tl when x = item -> (Multiple (times + 1, item)) :: tl
        | Multiple (times, item) :: tl -> (Single x) :: (Multiple (times, item)) :: tl
        | _ -> failwith "Error"
    List.foldBack collecter lst []
    