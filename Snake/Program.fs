open System;
open System.Threading;

// Author: Marcin Polak  marcin.jakub.polak<at>gmail.com
// This is simple snake game that I have created during learning F#
//
// Game is avaialable throught windows command line.
// Main board is rectangle of size W*H (modifiable).
// Snake speed is adjustable by Speed parameter.
// Start point is declared by startX and startY Literal

// All feedback is welcome!

// Width of board
[<Literal>]
let W = 20
// Height of board
[<Literal>]
let H = 10
// Snakes speed
[<Literal>]
let Speed = 0.1
// Snakes start X position
[<Literal>]
let startX = 5;
// Snakes start Y position
[<Literal>]
let startY = 5;

[<EntryPoint>]
let main argv = 

    // move position in key direction
    let move key (x,y) =   match key with
                        | ConsoleKey.UpArrow -> (x,y+1)
                        | ConsoleKey.DownArrow -> (x,y-1)
                        | ConsoleKey.LeftArrow -> (x-1,y)
                        | ConsoleKey.RightArrow -> (x+1,y)
                        | _ -> (x,y)

    // check that point is part of snake
    let rec is_snake (snake:(int*int) list) ((x,y):int*int) =    match snake with
                                                                | (sx,sy)::t -> if x= sx && y=sy then
                                                                                    true
                                                                                else
                                                                                    is_snake t (x,y);
                                                                | [] -> false;

    // check that point is valid for next movement of snake
    let valid_pt snake (x,y) = not ( x = 0 || x = W || y = 0 || y = H || (is_snake snake (x,y)) )

    // removes last item from queue ( it is not optimal, how to do efficient FIFO in F#? )
    let remove_last lst = lst |> List.rev |> (fun (a::b) -> b) |> List.rev;

    // "rendering" of board
    let draw snake food pt= 
        Console.Clear();
        for fn in 0..H do
            for m in 0..W do
                let n = (H-fn);
                let pt = (m,n);
                if (is_snake snake pt) then
                    if snake.Head = pt then
                        Console.Out.Write('@');
                    else    
                        Console.Out.Write('*');
                else
                    if food = pt then
                        Console.Out.Write('O');
                    else
                        if m = 0 || m = W || n = 0 || n = H then
                            Console.Out.Write('#');
                        else
                            Console.Out.Write(' ');
            if fn=0 then
                Console.Write(sprintf "  Score: %i" pt)
            Console.Out.WriteLine();

    let rnd = System.Random();

    // generates new food for snake position
    let rec get_food_pos snake =    let try_pt = (rnd.Next(1,W-1),rnd.Next(1,H-1));
                                    if is_snake snake try_pt then
                                        get_food_pos snake
                                    else
                                        try_pt;

    let key_ref = ref ConsoleKey.RightArrow;
    let gg = ref false
      
    // checks that next and last key arent opposite
    let is_oposite k =  match k,!key_ref with
                        | ConsoleKey.RightArrow,ConsoleKey.LeftArrow -> true
                        | ConsoleKey.LeftArrow,ConsoleKey.RightArrow -> true
                        | ConsoleKey.UpArrow,ConsoleKey.DownArrow   -> true
                        | ConsoleKey.DownArrow,ConsoleKey.UpArrow   -> true
                        | _,_ -> false;

    let start = (startX,startY);
    
    // main game loop
    let task = async {                       
                        let rec step snake (x,y) food last p= 
                            // moves snake by key
                            let move_step key= 
                                let np = move key (x,y);   
                                if np=(x,y) || (valid_pt snake np) then
                                    if np = food then
                                        step (np::snake) np (get_food_pos (np::snake)) key (p+1)
                                    else
                                        step (np::(remove_last snake)) np food key p;
                                else
                                    draw (np::(remove_last snake)) food p;
                                    false;
                            draw snake food p;
                            // movement delay (speed control )
                            let delay = async {
                                    do! Async.Sleep((int)(Speed*1000.0));
                                }
                            Async.RunSynchronously delay;

                            // validate next key
                            match !key_ref with
                            | ConsoleKey.Escape -> true;
                            | key ->    if not (is_oposite last) then
                                            move_step key
                                        else
                                            move_step last

                            | _ -> step snake (x,y) food last p;
                        step [start] start (get_food_pos [start]) ConsoleKey.RightArrow 0|> ignore;
                        Console.Out.WriteLine("Game over");
                        gg := true;
                  }
    
    let cancel = new CancellationTokenSource();

    //start game loop
    Async.Start(task,cancel.Token);


    // read keys
    let rec play a =    let new_key = Console.ReadKey();
                        if new_key.Key = ConsoleKey.Escape || !gg then
                            0
                        else
                            key_ref := new_key.Key
                            play a;
    play 0;
    0;