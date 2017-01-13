open System;
open System.Text;
type Node =
    struct
        val mutable mL :int;
        val mutable mR:int;
        val mutable total       :int;
        val mutable max         :int;
        val mutable l           :int;
        val mutable r           :int;
        val mutable fake        :bool;
        new(v :int) = { mL = v;mR=v;total=v;max=v;l=0;r=0;fake=true}
        new(v :int, p:int) = { mL = v;mR=v;total=v;max=v;l=p;r=p;fake=false}
    end

[<EntryPoint>]
let main argv = 
    let N = Int32.Parse (Console.ReadLine().Trim());

    let treeWidth = pown 2 (int (Math.Ceiling(Math.Log((float N),2.0))));
    let treeLevel       = 1 + int (Math.Ceiling(Math.Log((float N),2.0)));
    let treeArrayLength = (pown 2 (treeLevel))-1;
    let tD = Array.create treeArrayLength (new Node(0));
    let botom_id = treeArrayLength-treeWidth;

    let str = Console.ReadLine();
    
    let temp_data = Array.zeroCreate 6;
    let mutable pos = 0;
    let mutable ip = 0
    let mutable value = 0;
    
    for i=0 to (N-1) do
        ip<-0;
        value <- 0;
        let mutable minus = false
        while str.[pos] <> ' ' do
            match str.[pos] with
            | '-' ->    minus <- true;
            | x when x >='0' && x<='9' ->   temp_data.[ip] <- x;
                                            ip <- ip+1;
            | _ -> ();

            pos<-pos+1;
        temp_data.[ip] <- char 0;
    
        let mutable mul = 1;        
        let len = ip;
        while ip > 0 do
            ip <- ip - 1;
            value <- value + ( ( int temp_data.[ip] - 48)*mul );
            mul <- mul*10;
        if minus then
            value <- - value;
        pos<- pos+1;

        tD.[i+botom_id].mL <- value;
        tD.[i+botom_id].mR <- value;
        tD.[i+botom_id].max <- value;
        tD.[i+botom_id].total <-value;
        tD.[i+botom_id].fake <- false;
        tD.[i+botom_id].l<-i;
        tD.[i+botom_id].r<-i;
        
    let M = Int32.Parse (Console.ReadLine().Trim());
    let queries = Array.init M (fun x-> let vals = Console.ReadLine().Trim().Split(' '); 
                                        (-1+ Int32.Parse(vals.[0]) , -1+ Int32.Parse(vals.[1]) )) 

    let inline max3 (x:int) (y:int) (z:int) =       match x > y with
                                                    | true->    match x > z with
                                                                | true->x
                                                                | false -> z
                                                    | false ->  match y > z with
                                                                | true -> y
                                                                | false -> z

    let pairN = N + (N%2);

    for n = (treeLevel-2) downto 0 do
        let first = (pown 2 n) - 1;
        let range_step = treeWidth /(first+1);
        let last = first + first;

        let mutable range = 0;
        for i =first to last do
            let left    = tD.[2*i+1];
            let right   = tD.[2*i+2];

            tD.[i].fake <- range >= N;
            tD.[i].l <- range;
            tD.[i].r <- range+range_step-1;
            tD.[i].mL   <- max3 left.mL left.total (left.total+right.mL);
            tD.[i].mR  <- max3 right.mR right.total (right.total+left.mR);
            tD.[i].total      <- left.total + right.total;
            tD.[i].max        <- max3 left.max right.max (left.mR + right.mL);
            range <- range+range_step;

    let zN = new Node(0);

    let rec getSums node_id l r = 
        let curr_node = tD.[node_id];
        let r_l = curr_node.l;
        let r_r = curr_node.r;
        if curr_node.fake then
            zN
        else if l <= r_l && r >= r_r then
            curr_node
        else if l > r_r || r < r_l then
            zN;
        else
            
            let rC =   match tD.[2*node_id+2].fake with
                            | true ->   zN;
                            | false ->  getSums (2*node_id+2) l r;

            if rC.fake then
                getSums (2*node_id+1) l r;
            else
                let lC = getSums (2*node_id+1) l r;
                if lC.fake then
                    rC
                else
                    new Node(   max=max3 lC.max rC.max (lC.mR + rC.mL),
                                mL = max3 lC.mL lC.total (lC.total+rC.mL),
                                mR = max3 rC.mR rC.total (rC.total + lC.mR),
                                total = lC.total+rC.total
                        );
                        
    let sb = new StringBuilder();

    for i =0 to M-1 do
        let l,r = queries.[i];
        let sol = getSums 0 l r;
        ignore (sb.AppendLine(sol.max.ToString()));


    Console.Write(sb.ToString());

    0