// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System.IO;
open System;

[<EntryPoint>]
let main argv = 

    let getInt = (fun () ->Console.ReadLine().Trim().Split(' ')) >> Array.map( fun x->Int32.Parse(x));

    let T = getInt().[0];

    let rec CaseLoop c =
        let rec TestCase =
            let N,C = getInt() |> (fun x-> (x.[0],x.[1]));
            let data = Array.init N (fun x -> Console.ReadLine() |> Int32.Parse) |> Array.sort;

            let SpaceTest size =
                let rec Iteration last_id id sum =
                    match id with
                    | x when x = N ->  false
                    | x ->  if data.[id] - data.[last_id] >= size then
                                if sum+1 = C then
                                    true
                                else
                                    Iteration id (id+1) (sum+1) 
                            else
                                Iteration last_id (id+1) sum
                Iteration 0 1 1;

            let rec Binary L R sol=
                match L with
                | _ when L>=R -> sol
                | x ->  let mid = (R+L) >>> 1
                        match SpaceTest mid with
                        | true  ->  Binary (mid+1) R (max sol mid)
                        | false ->  Binary L mid sol;
            let sol = Binary data.[0] data.[N-1] 1;
            printf "%i" sol;
        match c with
        | 1 -> ();
        | x ->  TestCase
                CaseLoop (x-1);
        ();
        
    CaseLoop T;
    0 // return an integer exit code
