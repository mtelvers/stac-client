# stac-client

An OCaml client for the [STAC](https://stacspec.org/) (SpatioTemporal Asset
Catalog) API, with built-in support for
[Microsoft Planetary Computer](https://planetarycomputer.microsoft.com/)
SAS token signing.

Uses [Eio](https://github.com/ocaml-multicore/eio) for direct-style I/O and
[cohttp-eio](https://github.com/mirage/ocaml-cohttp) for HTTPS.

## Building

Requires OCaml >= 5.1.

```sh
opam install . --deps-only
dune build
```

## Usage

### CLI

```sh
dune exec bin/search.exe -- <collection> <bbox> <datetime> [limit]
```

For example, to search for Sentinel-2 Level-2A imagery over northern England
in June 2024:

```sh
dune exec bin/search.exe -- sentinel-2-l2a "-3.0,53.25,-1.5,54.15" "2024-06-01/2024-06-30" 10
```

Or Copernicus DEM tiles covering part of southeast England:

```sh
dune exec bin/search.exe -- cop-dem-glo-30 "0.0,51.0,1.0,52.0" "2021-01-01/2021-12-31"
```

### Library

```ocaml
open Stac_client

let () =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  let t = make env#net in
  Eio.Switch.run @@ fun sw ->
  let params = {
    collections = ["sentinel-2-l2a"];
    bbox = [-3.0; 53.25; -1.5; 54.15];
    datetime = "2024-06-01/2024-06-30";
    query = None;
    limit = Some 10;
  } in
  let base_url = "https://planetarycomputer.microsoft.com/api/stac/v1" in
  let items = search_and_sign ~sw t ~base_url params in
  List.iter (fun item ->
    Printf.printf "%s\n" item.id
  ) items
```

## Running tests

```sh
dune runtest
```

## License

MIT
