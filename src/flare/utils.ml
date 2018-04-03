let group_by f ll =
    List.fold_left
        (fun acc e ->
             let grp = f e in
                let grp_mems = try Hashtbl.find acc grp with Not_found -> [] in
                Hashtbl.replace acc grp (e :: grp_mems);
                acc)
        (Hashtbl.create 100)
        ll;;

let l = [ [1; 2; 3]; [4; 5; 6] ];;
group_by (fun (x::xs) -> xs) l
