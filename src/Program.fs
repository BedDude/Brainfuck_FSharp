open System.IO

/// reads code from file
let readFile path =
    let symbols = ['<'; '>'; '.'; ','; '+'; '-'; '['; ']']
    File.ReadAllText(path)
    |> String.filter(fun x -> List.contains x symbols)
    |> Seq.toArray
    

[<EntryPoint>]
let main argv =
    
    0