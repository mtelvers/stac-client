(** STAC API client with Planetary Computer SAS token signing. *)

(** {1 Types} *)

type asset = {
  href : string;
  type_ : string option;
  title : string option;
}

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

(** {1 Client} *)

type t
(** A STAC client encapsulating an HTTPS-capable HTTP client and a SAS token
    cache. *)

val make : _ Eio.Net.t -> t
(** [make net] creates a client using system CA certificates for TLS. *)

val http_get : sw:Eio.Switch.t -> t -> string -> Http.Response.t * string
(** [http_get ~sw t url] performs an HTTP GET request using the TLS-configured
    client. Returns the response and body string. *)

val search :
  sw:Eio.Switch.t ->
  t ->
  base_url:string ->
  ?max_pages:int ->
  search_params ->
  item list
(** [search ~sw t ~base_url params] issues a STAC search and follows
    pagination links automatically. [max_pages] (default 1000) bounds the
    number of pages fetched. *)

val sign_planetary_computer : sw:Eio.Switch.t -> t -> item -> item
(** Sign asset URLs with a Microsoft Planetary Computer SAS token. The item
    must have a [collection] field. *)

val search_and_sign :
  sw:Eio.Switch.t ->
  t ->
  base_url:string ->
  ?max_pages:int ->
  search_params ->
  item list
(** Convenience: {!search} then {!sign_planetary_computer} on every item. *)

(** {1 Item properties} *)

val get_string_prop : item -> string -> string option
val get_float_prop : item -> string -> float option
val get_datetime : item -> string option

(** {1 JSON conversion} *)

val item_of_yojson : Yojson.Safe.t -> (item, string) result
val item_to_yojson : item -> Yojson.Safe.t
val search_params_to_yojson : search_params -> Yojson.Safe.t
val normalize_datetime : string -> string
