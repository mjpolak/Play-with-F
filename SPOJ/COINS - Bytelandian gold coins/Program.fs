open System.IO;
open System.Collections.Generic
open System;

[<EntryPoint>]
let main argv =     
    let dt = new Dictionary<int64,int64>();

    dt.Add(0L,0L);

    let rec CaseLoop() =
        let rec MaxCoins (x:int64) = 
            match dt.ContainsKey x with
            | true -> dt.[x]
            | false ->      let a = int64 (Math.Floor( float x/2.0))
                            let b = int64 (Math.Floor(float x/3.0))
                            let c = int64 (Math.Floor(float x/4.0))

                            let t = (max a (MaxCoins a)) + (max b (MaxCoins b)) + (max (MaxCoins c) c);

                            dt.[x] <- max x t;
                            dt.[x];

        let ln = Console.ReadLine();
        match Int64.TryParse(ln) with
        | (true,x) ->     printf "%i\n" (MaxCoins (x));
                          CaseLoop();
        | _ -> 0;
        
    CaseLoop();
