open System
open System.IO

type Program = 
    { Code: char[]
      mutable CodePos: int 
      Mem: int[]
      mutable MemPos: int }

let readFile path =
    let symbols = ['<'; '>'; '.'; ','; '+'; '-'; '['; ']']
    File.ReadAllText(path)
    |> String.filter(fun x -> List.contains x symbols)
    |> Seq.toArray
    
let createProgram code = 
    { Code = code; CodePos = 0; Mem = [|for i in 0..30000 -> 0|]; MemPos = 0 }

let readValue = 
    Console.Read()

let printValue value = 
    Console.Write(char(value))

let getNextOp program = 
    if program.CodePos < program.Code.Length 
    then Some(program.Code.[program.CodePos])
    else None
    
let interpCode program =
    match program.Code.[program.CodePos] with
    | '+' -> program.Mem.[program.MemPos] <- program.Mem.[program.MemPos] + 1
    | '-' -> program.Mem.[program.MemPos] <- program.Mem.[program.MemPos] - 1
    | '>' -> program.MemPos <- program.MemPos + 1
    | '<' -> program.MemPos <- program.MemPos - 1
    | '.' -> printValue program.Mem.[program.MemPos]
    | ',' -> program.Mem.[program.MemPos] <- readValue
    program.CodePos <- program.CodePos + 1

let rec run program = 
    match getNextOp program with
    | Some(x) -> interpCode program; run program
    | None -> 0

[<EntryPoint>]
let main argv =
    if argv.Length < 1
    then printf "We need code!"; 0
    else readFile argv.[0]
        |> createProgram
        |> run