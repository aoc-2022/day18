open System.IO
open System 

let lines = File.ReadAllLines "/tmp/aoc/input" |> List.ofSeq
type Cube = Int64*Int64*Int64

let (|Int|_|) (s:string) : int64 option =
    match Int64.TryParse s with
    | true,value -> Some(value)
    | false,_ -> None 

let parse (s: string) : Cube =
    match s.Split [|','|] with
    | [|Int x;Int y;Int z|] -> (x,y,z)

let actualCubes = lines |> List.map parse |> Set.ofList 
            
let freeSized (other:Set<Cube>) ((x,y,z):Cube) : int64 =
    let nextTo1 = other.Contains (x-1L,y,z)
    let nextTo2 = other.Contains (x+1L,y,z)
    let nextTo3 = other.Contains (x,y-1L,z)
    let nextTo4 = other.Contains (x,y+1L,z)
    let nextTo5 = other.Contains (x,y,z-1L)
    let nextTo6 = other.Contains (x,y,z+1L)
    [nextTo1;nextTo2;nextTo3;nextTo4;nextTo5;nextTo6] |> List.filter (fun c -> c = false) |> List.length |> int64  

let sides = actualCubes |> Set.toSeq |> Seq.map (freeSized actualCubes) |> Seq.toList |> List.sum

printfn $"sides={sides}"

let superCube : Set<Cube> =
    let x1,x2 = actualCubes |> Set.map (fun (x,y,z) -> x) |> (fun s -> s.MinimumElement, s.MaximumElement)
    let y1,y2 = actualCubes |> Set.map (fun (x,y,z) -> y) |> (fun s -> s.MinimumElement, s.MaximumElement)
    let z1,z2 = actualCubes |> Set.map (fun (x,y,z) -> z) |> (fun s -> s.MinimumElement, s.MaximumElement)
    let xs = seq { x1 .. x2 } |> Seq.toList
    let ys = seq { y1 .. y2 } |> Seq.toList
    let zs = seq { z1 .. z2 } |> Seq.toList
    xs |> List.collect (fun x -> ys |> List.collect (fun y -> zs |> List.map (fun z -> (x,y,z))))
       |> Set.ofList 
    
let rec fill (superCube:Set<Cube>) (real:Set<Cube>) =
    let outerAir = superCube
                  |> Set.filter (fun cube -> freeSized superCube cube > 0)
                  |> Set.filter (fun cube -> real.Contains cube |> not)
    printfn $"before: outerAir={outerAir.Count} super={superCube.Count}"
    let superCube = superCube |> Set.filter (fun cube -> outerAir.Contains cube |> not)
    printfn $"after: super = {superCube.Count}"
    if outerAir.IsEmpty then superCube
    else fill superCube real 

printfn "########"

printfn $"input: {actualCubes.Count}"
let filled = fill superCube actualCubes

let sides2 = filled |> Set.toSeq |> Seq.map (freeSized filled) |> Seq.toList

printfn $"sides2 = {sides2 |> List.sum}"
