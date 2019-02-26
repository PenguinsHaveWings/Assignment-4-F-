module MultiSet

    type MultiSet<'a when 'a : comparison>  = MS of Map<'a, uint32>

    let empty = MS (Map.empty)
    let isEmpty (MS(m)) = Map.isEmpty m
    let size (MS(m)) = uint32(Map.count m) //TODO count values insted of number of keys
    let contains a (MS(m)) = Map.containsKey a m
    let numItems a (MS(m)) = (Map.tryFind(a))

    let add a u (MS(m)) = 
        if Map.containsKey a m 
            then 
                let tmpval = Map.find a m
                MS(Map.add a (u+tmpval) m)
        else MS(Map.add a u m)

    let addSingle a (MS(m)) = add a 1u (MS(m))

    let remove a u (MS(m)) = 
        if Map.containsKey a m 
            then 
                let tmpval = Map.find a m
                if tmpval< u then MS(Map.remove a m)
                else 
                    if Map.containsKey a m 
                        then 
                            let tmpval = Map.find a m
                            MS(Map.add a (u+tmpval) m)
                    else MS(Map.add a u m)
        else MS(m)

    let removeSingle a (MS(m)) = remove a 1u (MS(m))

    let fold f acc (MS(m)) = MS(Map.fold f acc m)

    let foldBack f acc (MS(m)) = MS(Map.foldBack f acc m)

    let map mf (MS(m)) = MS(Map.map mf m)

    let ofList l = List.fold (fun acc element -> addSingle element acc) empty l

    let toList (MS(m)) = (Map.toList m)

    let union (MS(s1)) (MS(s2)) = MS(Map(Seq.concat [ (Map.toSeq s1) ; (Map.toSeq s2) ]))

    let subtract (MS a) (MS b) =
        let bCount key = match Map.tryFind key b with | Some c -> c | None -> 0u
        let positiveCounts, _ = 
            a 
            |> Map.map (fun key value -> value - (bCount key))
            |> Map.partition (fun _ value -> value > 0u)
        MS positiveCounts

    let intersection l = l

