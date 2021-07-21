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
    { Code = code; CodePos = 0; Mem = Array.zeroCreate(30000); MemPos = 0 }

let memoryPos currPos shift = 
    (currPos + shift + 30000) % 30000

let getNextOp program = 
    if program.CodePos < program.Code.Length 
    then Some(program.Code.[program.CodePos])
    else None

let rec startLoop program pos level = 
    match level with
    | 0 -> pos
    | _ -> match program.Code.[pos] with
           | '[' -> startLoop program (pos - 1) (level - 1)
           | ']' -> startLoop program (pos - 1) (level + 1)
           | _ -> startLoop program (pos - 1) level

let rec endLoop program pos level = 
    match level with
    | 0 -> pos - 1
    | _ -> match program.Code.[pos] with
           | '[' -> endLoop program (pos + 1) (level + 1)
           | ']' -> endLoop program (pos + 1) (level - 1)
           | _ -> endLoop program (pos + 1) level

let interpCode prg symb =
    match symb with
    | '+' -> prg.Mem.[prg.MemPos] <- prg.Mem.[prg.MemPos] + 1
    | '-' -> prg.Mem.[prg.MemPos] <- prg.Mem.[prg.MemPos] - 1
    | '>' -> prg.MemPos <- memoryPos prg.MemPos 1
    | '<' -> prg.MemPos <- memoryPos prg.MemPos (-1)
    | '.' -> Console.Write(char(prg.Mem.[prg.MemPos]))
    | ',' -> prg.Mem.[prg.MemPos] <- Console.Read()
    | '[' -> if prg.Mem.[prg.MemPos] = 0 
             then prg.CodePos <- endLoop prg (prg.CodePos + 1) 1 
    | ']' -> if prg.Mem.[prg.MemPos] <> 0
             then prg.CodePos <- startLoop prg (prg.CodePos - 1) 1
    | _ -> ()

let rec run program = 
    match getNextOp program with
    | Some(x) -> interpCode program x
                 program.CodePos <- program.CodePos + 1 
                 run program
    | None -> 0

[<EntryPoint>]
let main argv =
    if argv.Length > 0
    then readFile argv.[0]
         |> createProgram
         |> run
    else 0