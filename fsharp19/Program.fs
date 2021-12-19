type vec3 = struct
    val         x   : int
    val         y   : int
    val         z   : int

    new(x, y, z) = { x = x; y = y; z = z }

    member x.Item i =
        match i with
        | 0 -> x.x
        | 1 -> x.y
        | 2 -> x.z
        | _ -> failwith "vec3: index out of range"

    static member inline (+) (a: vec3, b: vec3)    = vec3(a.x + b.x, a.y + b.y, a.z + b.z)
    static member inline (-) (a: vec3, b: vec3)    = vec3(a.x - b.x, a.y - b.y, a.z - b.z)
    static member inline (*) (a: vec3, b: vec3)    = vec3(a.x * b.x, a.y * b.y, a.z * b.z)
    static member inline (/) (a: vec3, b: vec3)    = vec3(a.x / b.x, a.y / b.y, a.z / b.z)

    static member inline (~-) (a: vec3)            = vec3(-a.x, -a.y, -a.z)

    static member inline dot (a: vec3, b: vec3)        =  a.x * b.x + a.y * b.y + a.z * b.z
    static member inline length (v: vec3)              = vec3.dot(v, v) |> float |> sqrt
    static member inline distance (v0: vec3, v1: vec3) = let s = v1 - v0 in vec3.length s
    //static member inline normalize (v: vec3)           = let l = vec3.length v in v / l

    static member inline cross (a: vec3, b: vec3)  = vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)

    static member inline min (a: vec3) (b: vec3)    = vec3(min a.x b.x, min a.y b.y, min a.z b.z)
    static member inline max (a: vec3) (b: vec3)    = vec3(max a.x b.x, max a.y b.y, max a.z b.z)

    override x.ToString() = $"(%d{x.x}, %d{x.y}, %d{x.z})"

    end


type CoordinateType =
    | PosX
    | NegX
    | PosY
    | NegY
    | PosZ
    | NegZ

let rotations =
    [
      // positive x
      (PosX, PosY, PosZ)
      (PosX, PosZ, NegY)
      (PosX, NegY, NegZ)
      (PosX, NegY, PosZ)

      // negative x
      (NegX, NegY, PosZ)
      (NegX, PosZ, PosY)
      (NegX, PosY, NegZ)
      (NegX, NegZ, NegY)

      // positive y
      (PosY, PosZ, PosX)
      (PosY, NegX, PosZ)
      (PosY, NegZ, NegX)
      (PosY, PosX, NegZ)

      // negative y
      (NegY, NegZ, PosX)
      (NegY, PosX, PosZ)
      (NegY, PosZ, NegX)
      (NegY, NegX, NegZ)

      // positive z
      (PosZ, PosX, PosY)
      (PosZ, NegY, PosX)
      (PosZ, NegX, NegY)
      (PosZ, PosY, NegX)

      // negative z
      (NegZ, NegX, PosY)
      (NegZ, PosY, PosX)
      (NegZ, PosX, NegY)
      (NegZ, NegY, NegX) ]

let rotate (p: vec3) r =
    let comp (p: vec3) t =
        match t with
        | PosX -> +p.x
        | NegX -> -p.x
        | PosY -> +p.y
        | NegY -> -p.y
        | PosZ -> +p.z
        | NegZ -> -p.z

    let r1, r2, r3 = r

    vec3(comp p r1, comp p r2, comp p r3)

let scanners = [
    [ vec3(404,-588,-901)
      vec3(528,-643,409)
      vec3(-838,591,734)
      vec3(390,-675,-793)
      vec3(-537,-823,-458)
      vec3(-485,-357,347)
      vec3(-345,-311,381)
      vec3(-661,-816,-575)
      vec3(-876,649,763)
      vec3(-618,-824,-621)
      vec3(553,345,-567)
      vec3(474,580,667)
      vec3(-447,-329,318)
      vec3(-584,868,-557)
      vec3(544,-627,-890)
      vec3(564,392,-477)
      vec3(455,729,728)
      vec3(-892,524,684)
      vec3(-689,845,-530)
      vec3(423,-701,434)
      vec3(7,-33,-71)
      vec3(630,319,-379)
      vec3(443,580,662)
      vec3(-789,900,-551)
      vec3(459,-707,401) ]
    [ vec3(686,422,578)
      vec3(605,423,415)
      vec3(515,917,-361)
      vec3(-336,658,858)
      vec3(95,138,22)
      vec3(-476,619,847)
      vec3(-340,-569,-846)
      vec3(567,-361,727)
      vec3(-460,603,-452)
      vec3(669,-402,600)
      vec3(729,430,532)
      vec3(-500,-761,534)
      vec3(-322,571,750)
      vec3(-466,-666,-811)
      vec3(-429,-592,574)
      vec3(-355,545,-477)
      vec3(703,-491,-529)
      vec3(-328,-685,520)
      vec3(413,935,-424)
      vec3(-391,539,-444)
      vec3(586,-435,557)
      vec3(-364,-763,-893)
      vec3(807,-499,-711)
      vec3(755,-354,-619)
      vec3(553,889,-390) ]
    [ vec3(649,640,665)
      vec3(682,-795,504)
      vec3(-784,533,-524)
      vec3(-644,584,-595)
      vec3(-588,-843,648)
      vec3(-30,6,44)
      vec3(-674,560,763)
      vec3(500,723,-460)
      vec3(609,671,-379)
      vec3(-555,-800,653)
      vec3(-675,-892,-343)
      vec3(697,-426,-610)
      vec3(578,704,681)
      vec3(493,664,-388)
      vec3(-671,-858,530)
      vec3(-667,343,800)
      vec3(571,-461,-707)
      vec3(-138,-166,112)
      vec3(-889,563,-600)
      vec3(646,-828,498)
      vec3(640,759,510)
      vec3(-630,509,768)
      vec3(-681,-892,-333)
      vec3(673,-379,-804)
      vec3(-742,-814,-386)
      vec3(577,-820,562) ]
    [ vec3(-589,542,597)
      vec3(605,-692,669)
      vec3(-500,565,-823)
      vec3(-660,373,557)
      vec3(-458,-679,-417)
      vec3(-488,449,543)
      vec3(-626,468,-788)
      vec3(338,-750,-386)
      vec3(528,-832,-391)
      vec3(562,-778,733)
      vec3(-938,-730,414)
      vec3(543,643,-506)
      vec3(-524,371,-870)
      vec3(407,773,750)
      vec3(-104,29,83)
      vec3(378,-903,-323)
      vec3(-778,-728,485)
      vec3(426,699,580)
      vec3(-438,-605,-362)
      vec3(-469,-447,-387)
      vec3(509,732,623)
      vec3(647,635,-688)
      vec3(-868,-804,481)
      vec3(614,-800,639)
      vec3(595,780,-596) ]
    [ vec3(727,592,562)
      vec3(-293,-554,779)
      vec3(441,611,-461)
      vec3(-714,465,-776)
      vec3(-743,427,-804)
      vec3(-660,-479,-426)
      vec3(832,-632,460)
      vec3(927,-485,-438)
      vec3(408,393,-506)
      vec3(466,436,-512)
      vec3(110,16,151)
      vec3(-258,-428,682)
      vec3(-393,719,612)
      vec3(-211,-452,876)
      vec3(808,-476,-593)
      vec3(-575,615,604)
      vec3(-485,667,467)
      vec3(-680,325,-822)
      vec3(-627,-443,-432)
      vec3(872,-547,-609)
      vec3(833,512,582)
      vec3(807,604,487)
      vec3(839,-516,451)
      vec3(891,-625,532)
      vec3(-652,-548,-490)
      vec3(30,-46,-14) ]
]
let scanner0 = [
    vec3(404,-588,-901)
    vec3(528,-643,409)
    vec3(-838,591,734)
    vec3(390,-675,-793)
    vec3(-537,-823,-458)
    vec3(-485,-357,347)
    vec3(-345,-311,381)
    vec3(-661,-816,-575)
    vec3(-876,649,763)
    vec3(-618,-824,-621)
    vec3(553,345,-567)
    vec3(474,580,667)
    vec3(-447,-329,318)
    vec3(-584,868,-557)
    vec3(544,-627,-890)
    vec3(564,392,-477)
    vec3(455,729,728)
    vec3(-892,524,684)
    vec3(-689,845,-530)
    vec3(423,-701,434)
    vec3(7,-33,-71)
    vec3(630,319,-379)
    vec3(443,580,662)
    vec3(-789,900,-551)
    vec3(459,-707,401)
]

let scanner1 = [
    vec3(686,422,578)
    vec3(605,423,415)
    vec3(515,917,-361)
    vec3(-336,658,858)
    vec3(95,138,22)
    vec3(-476,619,847)
    vec3(-340,-569,-846)
    vec3(567,-361,727)
    vec3(-460,603,-452)
    vec3(669,-402,600)
    vec3(729,430,532)
    vec3(-500,-761,534)
    vec3(-322,571,750)
    vec3(-466,-666,-811)
    vec3(-429,-592,574)
    vec3(-355,545,-477)
    vec3(703,-491,-529)
    vec3(-328,-685,520)
    vec3(413,935,-424)
    vec3(-391,539,-444)
    vec3(586,-435,557)
    vec3(-364,-763,-893)
    vec3(807,-499,-711)
    vec3(755,-354,-619)
    vec3(553,889,-390)
]
//let scanner0 = [
//    vec3(-1,-1,1)
//    vec3(-2,-2,2)
//    vec3(-3,-3,3)
//    vec3(-2,-3,1)
//    vec3(5,6,-4)
//    vec3(8,0,7)
//]
//
//let scanner1 = [
//    vec3(1,-1,1)
//    vec3(2,-2,2)
//    vec3(3,-3,3)
//    vec3(2,-1,3)
//    vec3(-5,4,-6)
//    vec3(-8,-7,0)
//]

let rec comb n l =
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, x::xs -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

//let scanner0Distances = comb 2 scanner0 |> Seq.map (fun l -> (vec3.distance (l[0], l[1]), (l[0], l[1]))) |> Map
//let scanner1Distances = comb 2 scanner1 |> Seq.map (fun l -> (vec3.distance (l[0], l[1]), (l[0], l[1]))) |> Map

//let matches =
//    scanner0Distances.Keys
//    |> Seq.filter (fun k -> Map.containsKey k scanner1Distances)
//    |> Seq.map (fun k -> (scanner0Distances[k], scanner1Distances[k]))
//    |> List.ofSeq

let distanceMap vecs =
    comb 2 vecs |> Seq.map (fun l -> (vec3.distance (l[0], l[1]), (l[0], l[1]))) |> Map

let a = scanners[0]
let mapA = distanceMap a

let matches =
    List.skip 1 scanners
    |> List.map (fun s ->
                 let m = distanceMap s
                 Map.keys mapA |> Seq.filter (fun k -> Map.containsKey k m) |> Seq.length)

let distanceMatchesWith (map: Map<float, vec3 * vec3>) list =
    let m = distanceMap list
    map |> Seq.filter (fun pair -> Map.containsKey pair.Key m) |> Seq.map (fun p -> (p.Key, p.Value))

let rec merge scanners map =
    match scanners with
    | [_] -> map
    | _ ->
        let othersSortedByMatchCount =
            scanners
            |> List.map (fun s -> (s, distanceMatchesWith map s))
            |> List.sortByDescending (snd >> Seq.length)

        let candidate = othersSortedByMatchCount[0] |> fst

        let newMap =
            distanceMap candidate |> Map.fold (fun m k v -> Map.change k (fun o -> if o.IsSome then o else Some(v)) m) map

        merge (List.except [candidate] scanners) newMap

let map = merge scanners Map.empty

let unique = map |> Map.values |> Seq.collect (fun (a, b) -> [a;b]) |> Set