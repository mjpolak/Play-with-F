open System;
[<EntryPoint>]
let m a= 
    for c=1 to 10 do
        let d = Console.ReadLine().ToCharArray()
        printf "%i\n" (Array.fold (fun a x-> match x with |'T'|'D'|'L'|'F' -> 2*a | _ -> a) 1 d)
    0