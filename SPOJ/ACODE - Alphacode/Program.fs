open System.IO;
open System.Collections.Generic
open System;

[<EntryPoint>]
let main argv =     
    
    let rec CaseLoop v=
        let ln = Console.ReadLine();
        let Iteration (str:char list) = 
            let rec Calculate last lst pairs singles =
                match lst with
                | []    ->  pairs+singles
                | h::t ->   match h,last with
                            | '0','0'       ->  0;
                            | '0',_         ->  Calculate h t 0 (pairs+singles);
                            | x,'0'         ->  match x with 
                                                | '1' | '2' -> Calculate h t singles 0;
                                                | _         -> 0;
                            | _,_           ->  match Int32.TryParse (h.ToString()+last.ToString()) with
                                                | true,v when v<=1 || v > 26    -> Calculate h t 0 (singles+pairs);
                                                | false,_                       -> Calculate h t pairs singles
                                                | true,v                        -> Calculate h t singles (pairs+singles)
                                                
                
            let sol = Calculate str.Head str.Tail 0 1;
            printf "%i\n" sol;
            ()
        match ln with
        | "0"->  0
        | x ->  Iteration (x.ToCharArray() |> Array.rev |> Array.toList);
                CaseLoop();
        
    CaseLoop();
