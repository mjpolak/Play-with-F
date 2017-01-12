open System;
open System.Threading;
open System.Collections;
[<EntryPoint>]
let main argv = 
    let getInts = fun() -> (Console.ReadLine()).Split(' ') |> Array.map (fun x -> Int64.Parse(x));
    let getInt = fun() -> getInts().[0];

    let N = int (getInt());
    let data = getInts();
    let M = int (getInt());
    let queries = Array.init M (fun x-> let vals = getInts(); 
                                        (-1L+vals.[0] , -1L+ vals.[1] )) ;


    let treeWidth = pown 2 (int (Math.Ceiling(Math.Log((float N),2.0))));
    let treeLevel       = 1 + int (Math.Ceiling(Math.Log((float N),2.0)));
    let treeArrayLength = (pown 2 (treeLevel))-1;
    let treeData = Array.init treeArrayLength (fun x-> 0L);

    Array.Copy(data,0,treeData,(treeArrayLength-N-1),N);
    
    let rec FillLevel n =
        match n < 0 with
        | true  ->  ();
        | false ->  let first = (pown 2 n) - 1;
                    let last = first*2;
                    let rec FillCells i =
                        match i<=last with
                        | false ->  ();
                        | true  ->  //treeData.[i] <- treeData.[ 2*i + 1] + treeData.[ 2*i + 2];
                                    treeData.[i] <- max (treeData.[ 2*i + 1]) ( treeData.[ 2*i + 2] );
                                    FillCells (i+1);

                    FillCells first;
                    FillLevel (n-1);

    FillLevel (treeLevel-2);

    let getRange node_id =
        let lvl = int (Math.Log( float (node_id+1),2.0));
        let first = (pown 2 lvl) - 1;
        let range_size = (treeWidth)/( first + 1);
        let x = node_id-first
        ( x*range_size,(x+1)*range_size - 1 );

    let rec getSums node_id l r = 
        let r_l,r_r = getRange node_id;
        if l <= r_l && r >= r_r then
            treeData.[node_id];
        else if l > r_r || r < r_l then
            0L
        else
            getSums (2*node_id+1) l r + getSums (2*node_id+2) l r;


    let sol = (getSums 0 1 2);
    printf "sol : %i \n" sol;

    0