(* STAC API client with Planetary Computer SAS token signing *)

type asset = {
  href : string;
  type_ : string option; [@key "type"] [@default None]
  title : string option; [@default None]
} [@@deriving yojson { strict = false }]

type item = {
  id : string;
  collection : string option;
  bbox : float list option;
  properties : Yojson.Safe.t;
  assets : (string * asset) list;
}

type search_params = {
  collections : string list;
  bbox : float list;
  datetime : string;
  query : Yojson.Safe.t option;
  limit : int option;
}

type t = {
  client : Cohttp_eio.Client.t;
  token_cache : (string, string * string) Hashtbl.t;
}

let ( let* ) = Result.bind

(* -- JSON helpers -------------------------------------------------------- *)

let assets_of_yojson (json : Yojson.Safe.t) : ((string * asset) list, string) result =
  match json with
  | `Assoc pairs ->
    List.fold_left (fun acc (k, v) ->
      let* xs = acc in
      let* a =
        asset_of_yojson v
        |> Result.map_error (fun e -> "bad asset " ^ k ^ ": " ^ e)
      in
      Ok ((k, a) :: xs)
    ) (Ok []) pairs
    |> Result.map List.rev
  | _ -> Error "assets: expected JSON object"

let assets_to_yojson (assets : (string * asset) list) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, v) -> (k, asset_to_yojson v)) assets)

let parse_bbox l =
  List.fold_left (fun acc v ->
    let* xs = acc in
    match v with
    | `Float f -> Ok (f :: xs)
    | `Int i -> Ok (float_of_int i :: xs)
    | _ -> Error "bbox: expected number"
  ) (Ok []) l
  |> Result.map List.rev

let item_of_yojson (json : Yojson.Safe.t) : (item, string) result =
  match json with
  | `Assoc assoc ->
    let find k = List.assoc_opt k assoc in
    let* id =
      match find "id" with
      | Some (`String s) -> Ok s
      | Some _ -> Error "id: expected string"
      | None -> Error "missing required field: id"
    in
    let collection =
      match find "collection" with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let* bbox =
      match find "bbox" with
      | Some (`List l) -> Result.map Option.some (parse_bbox l)
      | Some _ -> Error "bbox: expected array"
      | None -> Ok None
    in
    let properties =
      match find "properties" with
      | Some v -> v
      | None -> `Assoc []
    in
    let* assets =
      match find "assets" with
      | Some v -> assets_of_yojson v
      | None -> Ok []
    in
    Ok { id; collection; bbox; properties; assets }
  | _ -> Error "item: expected JSON object"

let item_to_yojson (item : item) : Yojson.Safe.t =
  let fields = [
    ("id", `String item.id);
    ("properties", item.properties);
    ("assets", assets_to_yojson item.assets);
  ] in
  let fields = match item.collection with
    | Some c -> ("collection", `String c) :: fields
    | None -> fields
  in
  let fields = match item.bbox with
    | Some b -> ("bbox", `List (List.map (fun f -> `Float f) b)) :: fields
    | None -> fields
  in
  `Assoc fields

(* -- Datetime normalization ---------------------------------------------- *)

let is_bare_date s =
  String.length s = 10 && s.[4] = '-' && s.[7] = '-'

let normalize_datetime dt =
  match String.split_on_char '/' dt with
  | [single] ->
    if is_bare_date single then single ^ "T00:00:00Z" else single
  | [start_; end_] ->
    let s = if is_bare_date start_ then start_ ^ "T00:00:00Z" else start_ in
    let e = if is_bare_date end_ then end_ ^ "T23:59:59Z" else end_ in
    s ^ "/" ^ e
  | _ -> dt

let search_params_to_yojson (p : search_params) : Yojson.Safe.t =
  let fields = [
    ("collections", `List (List.map (fun s -> `String s) p.collections));
    ("bbox", `List (List.map (fun f -> `Float f) p.bbox));
    ("datetime", `String (normalize_datetime p.datetime));
  ] in
  let fields = match p.query with
    | Some q -> ("query", q) :: fields
    | None -> fields
  in
  let fields = match p.limit with
    | Some n -> ("limit", `Int n) :: fields
    | None -> fields
  in
  `Assoc fields

(* -- TLS / HTTP ---------------------------------------------------------- *)

let https_handler ~authenticator =
  let tls_config =
    match Tls.Config.client ~authenticator () with
    | Error (`Msg msg) -> failwith ("TLS config error: " ^ msg)
    | Ok c -> c
  in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

let make (net : _ Eio.Net.t) =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok x -> x
    | Error (`Msg m) -> failwith ("CA certs error: " ^ m)
  in
  let client =
    Cohttp_eio.Client.make ~https:(Some (https_handler ~authenticator)) net
  in
  { client; token_cache = Hashtbl.create 16 }

let read_body body =
  Eio.Buf_read.(parse_exn take_all) body ~max_size:(1024 * 1024 * 10)

let http_get ~sw t uri =
  let resp, body = Cohttp_eio.Client.get ~sw t.client (Uri.of_string uri) in
  (resp, read_body body)

let http_post_json ~sw t uri json_body =
  let headers = Http.Header.of_list [("Content-Type", "application/json")] in
  let body = Cohttp_eio.Body.of_string json_body in
  let resp, resp_body =
    Cohttp_eio.Client.post ~sw t.client ~headers ~body (Uri.of_string uri)
  in
  (resp, read_body resp_body)

(* -- Pagination ---------------------------------------------------------- *)

type next_page = {
  next_href : string;
  next_method : string;
  next_body : Yojson.Safe.t option;
}

let parse_features_from_response body_str =
  let json = Yojson.Safe.from_string body_str in
  match json with
  | `Assoc fields ->
    let features =
      match List.assoc_opt "features" fields with
      | Some (`List items) -> items
      | _ -> []
    in
    let next_link =
      match List.assoc_opt "links" fields with
      | Some (`List links) ->
        List.find_map (fun link ->
          match link with
          | `Assoc lf ->
            (match List.assoc_opt "rel" lf, List.assoc_opt "href" lf with
             | Some (`String "next"), Some (`String href) ->
               let meth = match List.assoc_opt "method" lf with
                 | Some (`String m) -> String.uppercase_ascii m
                 | _ -> "GET"
               in
               let body = List.assoc_opt "body" lf in
               Some { next_href = href; next_method = meth; next_body = body }
             | _ -> None)
          | _ -> None
        ) links
      | _ -> None
    in
    (features, next_link)
  | _ -> ([], None)

let search ~sw t ~base_url ?(max_pages = 1000) (params : search_params) : item list =
  let url = base_url ^ "/search" in
  let json_body = Yojson.Safe.to_string (search_params_to_yojson params) in
  let rec fetch_page ~url ~meth ~body acc pages_remaining =
    if pages_remaining <= 0 then List.rev acc
    else
      let resp, body_str = match meth with
        | "POST" -> http_post_json ~sw t url body
        | _ -> http_get ~sw t url
      in
      if Http.Status.compare resp.status `OK <> 0 then
        failwith (Printf.sprintf "STAC search returned %s: %s"
          (Http.Status.to_string resp.status) body_str);
      let features_json, next_link = parse_features_from_response body_str in
      let items = List.filter_map (fun j ->
        match item_of_yojson j with
        | Ok item -> Some item
        | Error e ->
          Printf.eprintf "Warning: skipping item: %s\n%!" e;
          None
      ) features_json in
      let acc = List.rev_append items acc in
      match next_link with
      | Some np ->
        let next_body = match np.next_body with
          | Some b -> Yojson.Safe.to_string b
          | None -> ""
        in
        fetch_page ~url:np.next_href ~meth:np.next_method ~body:next_body
          acc (pages_remaining - 1)
      | None -> List.rev acc
  in
  fetch_page ~url ~meth:"POST" ~body:json_body [] max_pages

(* -- Planetary Computer SAS signing -------------------------------------- *)

let pc_token_endpoint =
  "https://planetarycomputer.microsoft.com/api/sas/v1/token"

let format_utc time =
  let t = Unix.gmtime time in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (t.Unix.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

let is_token_valid expiry_str =
  let threshold = format_utc (Unix.gettimeofday () +. 60.0) in
  String.compare threshold expiry_str < 0

let get_sas_token ~sw t collection =
  match Hashtbl.find_opt t.token_cache collection with
  | Some (token, expiry) when is_token_valid expiry -> token
  | _ ->
    let url = pc_token_endpoint ^ "/" ^ collection in
    let resp, body_str = http_get ~sw t url in
    if Http.Status.compare resp.status `OK <> 0 then
      failwith (Printf.sprintf "SAS token request failed (%s): %s"
        (Http.Status.to_string resp.status) body_str);
    let json = Yojson.Safe.from_string body_str in
    (match json with
     | `Assoc fields ->
       let token = match List.assoc_opt "token" fields with
         | Some (`String s) -> s
         | _ -> failwith "SAS response: missing 'token'"
       in
       let expiry = match List.assoc_opt "msft:expiry" fields with
         | Some (`String s) -> s
         | _ -> format_utc (Unix.gettimeofday () +. 1800.0)
       in
       Hashtbl.replace t.token_cache collection (token, expiry);
       token
     | _ -> failwith "SAS response: expected JSON object")

let sign_href token href =
  if String.contains href '?' then
    href ^ "&" ^ token
  else
    href ^ "?" ^ token

let sign_planetary_computer ~sw t (item : item) : item =
  let collection = match item.collection with
    | Some c -> c
    | None -> failwith ("Cannot sign item without collection: " ^ item.id)
  in
  let token = get_sas_token ~sw t collection in
  let assets = List.map (fun (k, a) ->
    (k, { a with href = sign_href token a.href })
  ) item.assets in
  { item with assets }

let search_and_sign ~sw t ~base_url ?max_pages params =
  let items = search ~sw t ~base_url ?max_pages params in
  List.map (sign_planetary_computer ~sw t) items

(* -- Property accessors -------------------------------------------------- *)

let get_string_prop item key =
  match item.properties with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String s) -> Some s
     | _ -> None)
  | _ -> None

let get_float_prop item key =
  match item.properties with
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`Float f) -> Some f
     | Some (`Int i) -> Some (float_of_int i)
     | _ -> None)
  | _ -> None

let get_datetime item = get_string_prop item "datetime"
