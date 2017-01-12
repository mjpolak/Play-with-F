open System;
open System.Threading;
open System.Collections;
open System.Text;
type Node =
    struct
        val mutable max_left :int;
        val mutable max_right:int;
        val mutable total       :int;
        val mutable max         :int;
        new(v :int) = { max_left = v;max_right=v;total=v;max=v}
    end

[<EntryPoint>]
let main argv = 

//
//    let f=50000;
//    let r = new Random();
//
//    printf "%i\n" f;
//    for  i in 1..50000 do
//        printf "%i " ( r.Next(-10,10));
//
//    let tc = 100;
//    printf "\n%i\n" tc;
//    for  i in 1..tc do
//        let l = r.Next(1,f);
//        let rs = r.Next(l,f);
//        printf "%i %i\n" l rs;
//
//    printf "\n";

    //let getInts = fun() -> (Console.ReadLine()).Trim().Split(' ') |> Array (fun x -> Int32.Parse(x));
    let getInt = fun() -> Int32.Parse (Console.ReadLine().Trim());
    let N = int (getInt());

    //prepare tree struct
    let treeWidth = pown 2 (int (Math.Ceiling(Math.Log((float N),2.0))));
    let treeLevel       = 1 + int (Math.Ceiling(Math.Log((float N),2.0)));
    let treeArrayLength = (pown 2 (treeLevel))-1;
    let treeData = Array.init treeArrayLength (fun x-> new Node());
    let botom_id = treeArrayLength-treeWidth;
    
    (Console.ReadLine()).Trim().Split(' ') |> Array.iteri (fun i x-> treeData.[i+botom_id] <- new Node(Int32.Parse(x)));
    let M = int (getInt());
    let queries = Array.init M (fun x-> let vals = Console.ReadLine().Trim().Split(' '); 
                                        (-1+ Int32.Parse(vals.[0]) , -1+ Int32.Parse(vals.[1]) )) ;

    //Array.Copy(data,0,treeData,(treeArrayLength-N-1),N);

    let max3 x y z = max x y |> max z;
    
    let rec FillLevel n =
        match n < 0 with
        | true  ->  ();
        | false ->  let first = (pown 2 n) - 1;
                    let last = first*2;
                    let rec FillCells i =
                        match i<=last with
                        | false ->  ();
                        | true  ->  let left    = treeData.[2*i+1];
                                    let right   = treeData.[2*i+2];
                                    treeData.[i].max_left   <- max3 left.max_left left.total (left.total+right.max_left);
                                    treeData.[i].max_right  <- max3 right.max_right right.total (right.total+left.max_right);
                                    treeData.[i].total      <- left.total + right.total;
                                    treeData.[i].max        <- max3 left.max right.max (left.max_right + right.max_left);
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
            treeData.[node_id].max
        else if l > r_r || r < r_l then
            0
        else
            max3 (getSums (2*node_id+1) l r) (getSums (2*node_id+2) l r) (treeData.[2*node_id+1].max_right+treeData.[2*node_id+2].max_left);

    let sb = new StringBuilder();
    let CaseProc (l,r) = 
        let sol = getSums 0 l r;
        ignore (sb.AppendLine(sol.ToString()));
        ();

    queries |> Array.iter CaseProc;

    printf "%s" (sb.ToString());

    0