(* POSIX *)

type posix

val posix_to_millis : posix -> int

val millis_to_posix : int -> posix

val now : (posix, unit) BsOakCore.Task.t

(* TIME ZONES *)

type zone

type zone_name =
  | Name of string
  | Offset of int

val custom_zone : int -> (int * int) list -> zone

val utc : zone

val here : (zone, unit) BsOakCore.Task.t

(* DATES *)

val to_year : zone -> posix -> int

type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

val to_month : zone -> posix -> month

val to_day : zone -> posix -> int

type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

val to_weekday : zone -> posix -> weekday

val to_hour : zone -> posix -> int

val to_minute : zone -> posix -> int

val to_second : zone -> posix -> int

val to_millis : zone -> posix -> int

(* SUBSCRIPTIONS *)

val every : float -> (posix -> 'msg) -> 'msg BsOakCore.Sub.t
