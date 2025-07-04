(* TEST
 flags += "-alert -do_not_spawn_domains -alert -unsafe_multidomain";
 runtime5;
 multidomain;
 { bytecode; }
 { native; }
*)

let () =
  let domains = Array.init 7 (fun i ->
    Domain.spawn (fun () ->
          for j = 1 to 10000000 do () done;
          for j = 1 to 100 do
            Format.printf "21. +. 21. is %f@." (21. +. 21.);
          done
      )
    ) in
  Array.iter Domain.join domains
