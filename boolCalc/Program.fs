// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System.Linq;
open System.Text;
open System.IO;
open System;

let ToReader (str:string) =     
    str 
    |> Encoding.UTF8.GetBytes 
    |> (fun x-> new MemoryStream(x))
    |> (fun x-> new StreamReader(x));

type Expresion = 
    | Leaf of char
    | Node of (Expresion list)

[<EntryPoint>]
let main argv = 
    let is_param c = c >= 'a' && c <= 'z';

    let ExpresionToString exp =
        let rec ExpresionToStringLp (exp:Expresion) =
            let rec ExploreNode lst =
                match lst with
                | h::t -> (ExpresionToStringLp h)+(ExploreNode t)
                | [] -> "";
            match exp with
            | Leaf(x) -> x.ToString();
            | Node(lst) ->  let str= ExploreNode lst;
                            if str.Length>2 then
                                "("+str+")"
                            else
                                str;
        let sol = (ExpresionToStringLp exp);
        if sol.[0] ='(' then
            sol.Substring(1,sol.Length-2);
        else 
            sol;
    let CreateExpressionTree (reader:StreamReader) =
        let rec CreateExpressionTreeIN() =
            match reader.Read() with
            | -1 -> []
            | x ->  match char x with
                    | '(' -> Node(CreateExpressionTreeIN())::(CreateExpressionTreeIN());
                    | ')' -> [];
                    | x -> Leaf(x)::(CreateExpressionTreeIN());

        let rec separateLongEquations lst=
            let rec split_by_2nd_operator lst first= 
                match lst with
                | Leaf(x)::t when not (is_param x) && x <> '~' ->   if first then
                                                                        let (a,b) = (split_by_2nd_operator t false);
                                                                        (Leaf(x)::a,b);
                                                                    else
                                                                        ([],lst);
                | h::t ->   let (a,b) = split_by_2nd_operator t first;
                            (h::a,b);
                | []    -> ([],[]);

            match split_by_2nd_operator lst true with
            | x,[] -> lst;
            | x,y -> Node(x)::(separateLongEquations y);
        let rec separateNegation lst = 
            match lst with
            | Leaf(c)::t when c ='~' -> (Node([Leaf('~');t.Head])::t.Tail);
            | Leaf(c)::t -> Leaf(c)::separateNegation(t);
            | Node(lst)::t -> Node(separateNegation(lst))::separateNegation(t);
            | [] -> [];
        let part = separateNegation (CreateExpressionTreeIN());
        let test = separateLongEquations part;
        test

    let rec get_parameter parameters x =
        match parameters with
        | (c,value)::t when c=x -> value
        | h::t -> get_parameter t x;
        | [] -> false;

    let rec EvaluateExpression (exp:Expresion) parameters =



        let rec RPN lst stack=
            match lst with
            | h::t ->   match h with
                        | Leaf(c) as l  ->  match c with
                                            | x when is_param x || x ='~' -> l::(RPN t stack);
                                            | x -> (RPN t (Leaf(x)::stack));
                        | Node(nlst) as node -> node::(RPN t stack)
            | [] -> stack;
        let rec EvaluateNode lst stack=
            match lst with
            | h::t ->   match h with
                        | Node(lst) as node -> EvaluateNode t ((EvaluateExpression (Node(RPN lst [])) parameters)::stack);
                        | Leaf(c) when is_param c -> EvaluateNode t ((get_parameter parameters c)::stack);
                        | Leaf(c) when c ='~' -> EvaluateNode t.Tail ((not (EvaluateExpression t.Head parameters))::stack)
                        | Leaf(c) ->    let a = stack.Head;
                                        let (b::stack_rest) = stack.Tail
                                        match c with
                                        | '&' -> EvaluateNode t ((a && b)::stack_rest);
                                        | '|' -> EvaluateNode t ((a || b)::stack_rest);
                                        | '=' -> EvaluateNode t ((a = b)::stack_rest);
                                        | '<' -> EvaluateNode t ((not a || b)::stack_rest);
                                        | '>' -> EvaluateNode t ((a || not b)::stack_rest);
                                        | '^' -> EvaluateNode t ((a <> b)::stack_rest);
                                        | _ -> stack;
            | [] -> stack;
        match exp with
        | Leaf(x) -> get_parameter parameters x;
        | Node(lst) ->  (EvaluateNode (RPN lst []) []).Head;



    let rec remove_duplicates lst=
        match lst with
        | h::t when List.exists (fun x->h=x) t -> remove_duplicates t;
        | h::t -> h::(remove_duplicates t);
        | [] -> [];

    let get_truth num ofset = (0 < (num &&& ( 1 <<< ofset)));

    let rec compare_expr a b =
        let rec compare_node nA nB =
            match nA,nB with
            | (hA::tA),(hB::tB) when  compare_expr hA hB -> compare_node tA tB;
            | [],[] -> true;
            | _,_ -> false;
        match a,b with
        | Leaf(x),Node(lst) | Node(lst),Leaf(x) -> false;
        | Leaf(x),Leaf(y) -> x = y;
        | Node(lstA),Node(lstB) -> compare_expr a b;

    let GenerateExpresionList lst =
        let rec generate_expr_list lst lvl =
            match lst with
            | h::t ->   match h with
                        | Node(x) as node ->((node,lvl)::(generate_expr_list x (lvl+1))) @ (generate_expr_list t lvl);
                        | _ -> (generate_expr_list t lvl);
            | [] -> [];
        (Node(lst),0) :: (generate_expr_list lst 1);


    let fvromat (str:string) = str.Replace(" ","");
    let Calculate input = 

        let data = fvromat input;
        let reader = ToReader data;
                   
        let base_expresion  = (CreateExpressionTree reader);
        let sol = Node(base_expresion);

        let param_list  =   data.ToCharArray()
                                |> Array.toList
                                |> List.filter (fun x->is_param x)
                                |> remove_duplicates;
        let param = param_list |> List.toArray;

        let pos =  int ( Math.Pow(2.0, float param.Length) );


        let expresions =    GenerateExpresionList base_expresion
                            |> List.sortWith (fun (a,la) (b,lb)->   if la <= lb then 
                                                                        1 
                                                                    else
                                                                        if (ExpresionToString a).Length < (ExpresionToString b).Length then
                                                                            1
                                                                        else
                                                                            -1)
                            |> List.map fst
                            |> (fun x->     let rec rmv x =
                                                match x with
                                                | h::t when List.exists (fun m->m=h) t -> rmv t;
                                                | h::t -> h::(rmv t);
                                                | []-> x;
                                            rmv x)
                            |> (fun x-> (param_list |> List.map (fun z->Leaf(z)) )@x)
                            |> List.toArray;

        let vals = [
            ('p',true);
            ('q',false)
        ]

        let headers =   expresions
                        |> Array.map (fun x->ExpresionToString x);
        let truthmap = Array2D.init pos headers.Length (fun i j->false);
        


        for comb_id in 0..pos-1 do
            let pars = List.init param.Length (fun x-> (param.[x],get_truth comb_id x))
            for p_id in 0..expresions.Length-1 do
                truthmap.[comb_id,p_id] <- (EvaluateExpression expresions.[p_id] pars);
        (headers,truthmap);



    let CalcAndPrint x= 
        let to_bin (v:bool) =
            match v with
            | true -> '1';
            | false -> '0'; 
        let (headers,truthmap) = Calculate x;
        let mutable line = 0
        for h in 0..headers.Length-1 do
            line <- line + 2 + headers.[h].Length;
            printf " %s " headers.[h];
        printf "\n%s\n" (String.replicate line "-");

        let sizeA = truthmap |> Array2D.length1;
        let sizeB = truthmap |> Array2D.length2;
        for comb_id in 0..sizeA-1 do
            for p_id in 0..sizeB-1 do
                let da = int (Math.Floor ((float headers.[p_id].Length)/2.0));
                let db = headers.[p_id].Length-da-1;
                printf "%s%c%s" (String.replicate (da+1) " " ) (to_bin truthmap.[comb_id,p_id]) (String.replicate (db+1) " " );
            printf "\n";
        printf "\n\n";

    CalcAndPrint "p|q&v";

    let rec mainLoop() =
        let cmd = Console.ReadLine();
        match cmd.Trim() with
        | "exit" -> ();
        | "" -> mainLoop();
        | x->   try
                    CalcAndPrint x;
                    mainLoop();
                with
                    | _ ->  printf "Wrong command!";
                            mainLoop();
        
    mainLoop();
    0 // return an integer exit code
