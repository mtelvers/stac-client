(* CLI tool: search STAC catalog and print results *)

open Stac_client

let pc_base_url = "https://planetarycomputer.microsoft.com/api/stac/v1"

let () =
  if Array.length Sys.argv < 4 then (
    Printf.eprintf "Usage: %s <collection> <bbox> <datetime> [limit]\n" Sys.argv.(0);
    Printf.eprintf "  bbox: west,south,east,north (e.g. -3.0,53.25,-1.5,54.15)\n";
    Printf.eprintf "  datetime: start/end (e.g. 2024-01-01/2024-12-31)\n";
    exit 1
  );
  let collection = Sys.argv.(1) in
  let bbox = String.split_on_char ',' Sys.argv.(2) |> List.map float_of_string in
  let datetime = Sys.argv.(3) in
  let limit =
    if Array.length Sys.argv > 4 then Some (int_of_string Sys.argv.(4))
    else None
  in
  let params = { collections = [collection]; bbox; datetime; query = None; limit } in
  let t = make () in
  Printf.printf "Searching %s for %s in [%s] during %s...\n%!"
    pc_base_url collection Sys.argv.(2) datetime;
  let items = search t ~base_url:pc_base_url params in
  Printf.printf "Found %d items\n\n" (List.length items);
  let show_items = List.filteri (fun i _ -> i < 5) items in
  List.iteri (fun i item ->
    Printf.printf "  [%d] %s" i item.id;
    (match get_datetime item with
     | Some dt -> Printf.printf "  (%s)" dt
     | None -> ());
    Printf.printf "\n";
    Printf.printf "       assets: %s\n"
      (String.concat ", " (List.map fst item.assets))
  ) show_items;
  if List.length items > 5 then
    Printf.printf "  ... and %d more\n" (List.length items - 5);
  Printf.printf "\nSigning first item...\n%!";
  match items with
  | [] -> Printf.printf "No items to sign.\n"
  | first :: _ ->
    let signed = sign_planetary_computer t first in
    Printf.printf "Signed item: %s\n" signed.id;
    (match signed.assets with
     | (name, asset) :: _ ->
       Printf.printf "  %s: %s\n" name
         (if String.length asset.href > 120 then
            String.sub asset.href 0 120 ^ "..."
          else asset.href)
     | [] -> Printf.printf "  (no assets)\n")
