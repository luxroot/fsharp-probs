module ProblemThird

let rec insertAt item lst idx =
    match idx, lst with
    | 1, _ -> item :: lst
    | n, hd :: tl -> hd :: insertAt item tl (n - 1)
    | _ -> failwith "Error"

let range s e = [ s .. e ]
