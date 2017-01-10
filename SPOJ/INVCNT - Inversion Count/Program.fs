open System.IO;
open System.Collections.Generic
open System;

[<EntryPoint>]
let main argv =     
    
    let getInts = (fun()-> Console.ReadLine().Split(' ')) >> Array.map (fun x -> Int64.Parse(x));
    let getInt = fun() -> getInts().[0];

    let T = getInt();
    let rec MergeSort (data:int64 array ) counter=
        let Merge (a:int64 array) (b:int64 array) counter=
            let sol = Array.zeroCreate (a.Length+b.Length);
            let rec Merging_Iter p_a p_b pos counter =
                let addA = fun()    ->  sol.[pos] <- a.[p_a];
                                        Merging_Iter (p_a+1) p_b (pos+1) counter;
                let addB = fun()    ->  sol.[pos] <- b.[p_b];
                                        Merging_Iter p_a (p_b+1) (pos+1) (counter + (int64 (a.Length-p_a)));
                match a.Length > p_a,b.Length > p_b with
                | false ,false  ->  counter;
                | true  ,false  ->  addA();
                | false ,true   ->  addB();
                | true  ,true   ->  if a.[p_a] < b.[p_b] then
                                        addA()
                                    else
                                        addB()
            let moves = Merging_Iter 0 0 0 counter;
            (sol,moves)


        match data.Length,data.Length%2 with
        | 1,_   -> ([|data.[0]|],counter)
        | x,r   ->  let left =  Array.sub data 0 ( (x>>>1) + r);
                    let right = Array.sub data ((x>>>1)+r) (x>>>1);
                    
                    let left_sol,cl = MergeSort left counter;
                    let right_sol,cr = MergeSort right cl;
                    Merge left_sol right_sol cr;

    let rec CaseLoop v=
        match v with
        | 0L ->  0;
        | _ ->  ignore (Console.ReadLine());
                let N = getInt();
                let data =  Array.init (int N) (fun x-> getInt());
                
                let sol = MergeSort data 0L;

                printf "%i\n" (snd sol);
                //fst(sol) |> Array.iter (fun x-> printf "%i\n" x);

                CaseLoop (v-1L);
        
    CaseLoop T;
