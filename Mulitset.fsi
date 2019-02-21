

module MultiSet

    type MultiSet<'a when 'a : comparison>  = MS of Map<'a, uint32>

    let empty = MS (Map.empty)
    let isEmpty (MS(m)) = Map.isEmpty m
    let size (MS(m)) = Map.count m
    let contains a (MS(m)) = Map.containsKey a m
    let numItems a (MS(m)) = Map.tryFind(a)

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
    