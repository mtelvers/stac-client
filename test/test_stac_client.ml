open Stac_client

(* -- normalize_datetime -------------------------------------------------- *)

let test_bare_date () =
  Alcotest.(check string) "bare date gets start-of-day"
    "2024-01-01T00:00:00Z"
    (normalize_datetime "2024-01-01")

let test_full_timestamp_unchanged () =
  Alcotest.(check string) "already has time"
    "2024-01-01T12:00:00Z"
    (normalize_datetime "2024-01-01T12:00:00Z")

let test_bare_date_range () =
  Alcotest.(check string) "bare range"
    "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z"
    (normalize_datetime "2024-01-01/2024-12-31")

let test_mixed_range () =
  Alcotest.(check string) "mixed: bare start, full end"
    "2024-01-01T00:00:00Z/2024-12-31T15:30:00Z"
    (normalize_datetime "2024-01-01/2024-12-31T15:30:00Z")

let test_full_range_unchanged () =
  Alcotest.(check string) "full range unchanged"
    "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z"
    (normalize_datetime "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z")

(* -- item_of_yojson ------------------------------------------------------ *)

let test_item_minimal () =
  let json = `Assoc [("id", `String "x")] in
  match item_of_yojson json with
  | Error e -> Alcotest.fail e
  | Ok item ->
    Alcotest.(check string) "id" "x" item.id;
    Alcotest.(check (option string)) "collection" None item.collection;
    Alcotest.(check (option (list (float 0.001)))) "bbox" None item.bbox;
    Alcotest.(check int) "assets" 0 (List.length item.assets)

let test_item_full () =
  let json = `Assoc [
    ("id", `String "S2A_MSIL2A");
    ("collection", `String "sentinel-2-l2a");
    ("bbox", `List [`Float (-3.0); `Float 53.0; `Int (-1); `Float 54.0]);
    ("properties", `Assoc [("datetime", `String "2024-06-15T10:30:00Z")]);
    ("assets", `Assoc [
      ("B02", `Assoc [("href", `String "https://example.com/B02.tif")]);
    ]);
  ] in
  match item_of_yojson json with
  | Error e -> Alcotest.fail e
  | Ok item ->
    Alcotest.(check string) "id" "S2A_MSIL2A" item.id;
    Alcotest.(check (option string)) "collection"
      (Some "sentinel-2-l2a") item.collection;
    Alcotest.(check (option (list (float 0.001)))) "bbox"
      (Some [-3.0; 53.0; -1.0; 54.0]) item.bbox;
    Alcotest.(check (option string)) "datetime"
      (Some "2024-06-15T10:30:00Z") (get_datetime item);
    Alcotest.(check int) "asset count" 1 (List.length item.assets);
    let _, asset = List.hd item.assets in
    Alcotest.(check string) "asset href"
      "https://example.com/B02.tif" asset.href

let test_item_error_not_object () =
  match item_of_yojson (`String "oops") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-object"

let test_item_error_missing_id () =
  match item_of_yojson (`Assoc [("properties", `Assoc [])]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing id"

let test_item_error_bad_bbox () =
  let json = `Assoc [
    ("id", `String "x");
    ("bbox", `List [`String "bad"]);
  ] in
  match item_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-numeric bbox"

(* -- item roundtrip ------------------------------------------------------ *)

let test_item_roundtrip () =
  let item = {
    id = "test-item";
    collection = Some "sentinel-2";
    bbox = Some [-3.0; 53.0; -1.0; 54.0];
    properties = `Assoc [("datetime", `String "2024-06-15T10:30:00Z")];
    assets = [
      ("B02", { href = "https://example.com/B02.tif";
                type_ = Some "image/tiff"; title = Some "Blue" });
      ("B03", { href = "https://example.com/B03.tif";
                type_ = None; title = None });
    ];
  } in
  let json = item_to_yojson item in
  match item_of_yojson json with
  | Error e -> Alcotest.fail ("roundtrip failed: " ^ e)
  | Ok item' ->
    Alcotest.(check string) "id" item.id item'.id;
    Alcotest.(check (option string)) "collection"
      item.collection item'.collection;
    Alcotest.(check (option (list (float 0.001)))) "bbox"
      item.bbox item'.bbox;
    Alcotest.(check int) "asset count"
      (List.length item.assets) (List.length item'.assets);
    List.iter2 (fun (k1, a1) (k2, a2) ->
      Alcotest.(check string) "asset key" k1 k2;
      Alcotest.(check string) "asset href" a1.href a2.href;
      Alcotest.(check (option string)) "asset type" a1.type_ a2.type_;
      Alcotest.(check (option string)) "asset title" a1.title a2.title
    ) item.assets item'.assets

(* -- search_params_to_yojson --------------------------------------------- *)

let test_search_params_basic () =
  let params = {
    collections = ["sentinel-2-l2a"];
    bbox = [-3.0; 53.0; -1.0; 54.0];
    datetime = "2024-01-01/2024-12-31";
    query = None;
    limit = Some 10;
  } in
  let json = search_params_to_yojson params in
  match json with
  | `Assoc fields ->
    (* datetime should be normalized *)
    (match List.assoc_opt "datetime" fields with
     | Some (`String dt) ->
       Alcotest.(check string) "datetime normalized"
         "2024-01-01T00:00:00Z/2024-12-31T23:59:59Z" dt
     | _ -> Alcotest.fail "missing datetime");
    (* limit present *)
    (match List.assoc_opt "limit" fields with
     | Some (`Int n) -> Alcotest.(check int) "limit" 10 n
     | _ -> Alcotest.fail "missing limit")
  | _ -> Alcotest.fail "expected object"

let test_search_params_no_optional () =
  let params = {
    collections = ["cop-dem-glo-30"];
    bbox = [0.0; 0.0; 1.0; 1.0];
    datetime = "2024-06-15T00:00:00Z";
    query = None;
    limit = None;
  } in
  let json = search_params_to_yojson params in
  match json with
  | `Assoc fields ->
    Alcotest.(check bool) "no limit" true
      (Option.is_none (List.assoc_opt "limit" fields));
    Alcotest.(check bool) "no query" true
      (Option.is_none (List.assoc_opt "query" fields))
  | _ -> Alcotest.fail "expected object"

(* -- property accessors -------------------------------------------------- *)

let sample_item = {
  id = "test";
  collection = None;
  bbox = None;
  properties = `Assoc [
    ("datetime", `String "2024-06-15T10:30:00Z");
    ("eo:cloud_cover", `Float 12.5);
    ("eo:cloud_cover_int", `Int 12);
    ("platform", `String "Sentinel-2A");
  ];
  assets = [];
}

let test_get_string_prop () =
  Alcotest.(check (option string)) "present"
    (Some "Sentinel-2A") (get_string_prop sample_item "platform");
  Alcotest.(check (option string)) "missing"
    None (get_string_prop sample_item "nope");
  Alcotest.(check (option string)) "wrong type"
    None (get_string_prop sample_item "eo:cloud_cover")

let test_get_float_prop () =
  Alcotest.(check (option (float 0.001))) "float"
    (Some 12.5) (get_float_prop sample_item "eo:cloud_cover");
  Alcotest.(check (option (float 0.001))) "int coerced"
    (Some 12.0) (get_float_prop sample_item "eo:cloud_cover_int");
  Alcotest.(check (option (float 0.001))) "missing"
    None (get_float_prop sample_item "nope")

let test_get_datetime () =
  Alcotest.(check (option string)) "datetime"
    (Some "2024-06-15T10:30:00Z") (get_datetime sample_item)

(* -- test runner --------------------------------------------------------- *)

let () =
  Alcotest.run "stac_client" [
    "normalize_datetime", [
      Alcotest.test_case "bare date" `Quick test_bare_date;
      Alcotest.test_case "full timestamp" `Quick test_full_timestamp_unchanged;
      Alcotest.test_case "bare range" `Quick test_bare_date_range;
      Alcotest.test_case "mixed range" `Quick test_mixed_range;
      Alcotest.test_case "full range" `Quick test_full_range_unchanged;
    ];
    "item_of_yojson", [
      Alcotest.test_case "minimal" `Quick test_item_minimal;
      Alcotest.test_case "full" `Quick test_item_full;
      Alcotest.test_case "not object" `Quick test_item_error_not_object;
      Alcotest.test_case "missing id" `Quick test_item_error_missing_id;
      Alcotest.test_case "bad bbox" `Quick test_item_error_bad_bbox;
    ];
    "item roundtrip", [
      Alcotest.test_case "encode then decode" `Quick test_item_roundtrip;
    ];
    "search_params_to_yojson", [
      Alcotest.test_case "with limit" `Quick test_search_params_basic;
      Alcotest.test_case "no optionals" `Quick test_search_params_no_optional;
    ];
    "property accessors", [
      Alcotest.test_case "get_string_prop" `Quick test_get_string_prop;
      Alcotest.test_case "get_float_prop" `Quick test_get_float_prop;
      Alcotest.test_case "get_datetime" `Quick test_get_datetime;
    ];
  ]
