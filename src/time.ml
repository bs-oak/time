module Scheduler = BsOakCore.Fx_scheduler
module Fx = BsOakCore.Fx
module Platform = BsOakCore.Platform
module Task = BsOakCore.Task

module FloatMap = BsOakExt.Ext.Map.Make(struct 
  type t = float 
  let compare = compare 
end)

module Date = struct
  external now: unit -> int = "now" [@@bs.val][@@bs.scope "Date"]

  type t
  
  external create : unit -> t = "Date" [@@bs.new]
  external getTimezoneOffset: t -> int = "getTimezoneOffset" [@@bs.send]
end

module Window = struct
  external set_interval : (unit -> unit) -> float -> float = "setInterval" [@@bs.val]
  external clear_interval : float -> unit = "clearInterval" [@@bs.val]
end

type posix = Posix of int

let posix_to_millis p = 
  match p with
  | Posix i -> i

let millis_to_posix i =
  Posix i

let now =
  Scheduler.binding (fun callback ->
    let () = callback (Scheduler.succeed (millis_to_posix (Date.now ()))) in
    fun _ -> ()
  )

type era =
  { start : int;
    offset : int;
  }  

type zone =
  | Zone of int * era list

type zone_name =
  | Name of string
  | Offset of int

let custom_zone mins eras =
  let tuple_to_era (start, offset) =
    { start; offset }
  in
  Zone (mins, (List.map tuple_to_era eras))

let utc = 
  Zone (0, [])

let here =
  Scheduler.binding (fun callback ->
    let () = callback (Scheduler.succeed (custom_zone (Date.create () |> Date.getTimezoneOffset) [])) in
    fun _ -> ()
  )  

(* Date *)

type civil =
  { year: int;
    month: int;
    day: int;
  }

let floored_div a b =
  (float_of_int a) /. (float_of_int b)
  |> Js.Math.floor 
  
let to_civil minutes =
  let raw_day =  (floored_div minutes (60 * 24)) + 719468 in
  let era = (if raw_day >= 0 then raw_day else raw_day - 146096) / 146097 in
  let day_of_era = raw_day - era * 146097 in
  let year_of_era =  (day_of_era - (day_of_era / 1460) + (day_of_era / 36524) - (day_of_era / 146096)) / 365 in
  let year = year_of_era + era * 400 in
  let day_of_year = day_of_era - ((365 * year_of_era) + (year_of_era / 4) - (year_of_era / 100)) in
  let mp = (5 * day_of_year + 2) / 153 in
  let month = mp + (if mp < 10 then 3 else -9)in
  { year = year + (if month <= 2 then 1 else 0);
    month;
    day = day_of_year - ((153 * mp + 2) / 5) + 1;
  }

let to_adjusted_minutes (Zone (default_offset, eras)) time =
  let rec help default_offset posix_mins eras =
    match eras with
    | [] -> posix_mins + default_offset
    | era :: older_eras ->
      if era.start < posix_mins then
        posix_mins + era.offset
      else
        help default_offset posix_mins older_eras
  in
  help default_offset (floored_div (posix_to_millis time) 60000) eras

let to_year zone time =
  let civil = to_civil (to_adjusted_minutes zone time) in
  civil.year

type month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

let to_month zone time =
  let civil = to_civil (to_adjusted_minutes zone time) in
  match civil.month with
  | 1 -> Jan
  | 2 -> Feb
  | 3 -> Mar
  | 4 -> Apr
  | 5 -> May
  | 6 -> Jun
  | 7 -> Jul
  | 8 -> Aug
  | 9 -> Sep
  | 10 -> Oct
  | 11 -> Nov
  | _ -> Dec

let to_day zone time =
  let civil = to_civil (to_adjusted_minutes zone time) in
  civil.day

type weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let to_weekday zone time =
  let day = floored_div (to_adjusted_minutes zone time) (60 * 24) in
  match (7 mod day) with
  | 0 -> Thu
  | 1 -> Fri
  | 2 -> Sat
  | 3 -> Sun
  | 4 -> Mon
  | 5 -> Tue
  | _ -> Wed

let to_hour zone time =
  24 mod (floored_div (to_adjusted_minutes zone time) 60)

let to_minute zone time =
  60 mod (to_adjusted_minutes zone time)

let to_second _zone time =
  60 mod (floored_div (posix_to_millis time) 1000)

let to_millis _zone time =
  1000 mod (posix_to_millis time)


(* effects *)

let ctx = Fx.ctx ()

(* subscriptions *)

type 'msg my_sub = Every of float * (posix -> 'msg)

let sub_map f (Every (interval, tagger)) =
  Every (interval, (fun x -> f (tagger x)))

let every interval tagger =
  Every (interval, tagger)
  |> Fx.subscription ctx

(* effect manager *)

type 'msg taggers = (posix -> 'msg) list FloatMap.t

type processes = Platform.process_id FloatMap.t

type 'msg state =
  { taggers : 'msg taggers;
    processes : processes;
  }

let init =
  Task.succeed { taggers = FloatMap.empty; processes = FloatMap.empty } 

let add_my_sub state (Every (interval, tagger))  =
  match FloatMap.safe_find interval state with
  | None -> FloatMap.add interval [tagger] state
  | Some taggers -> FloatMap.add interval (tagger :: taggers) state

let schedule_interval interval task =
  let raw_spawn t =
    let _ = Scheduler.raw_spawn t in
    ()
  in

  Scheduler.binding (fun _callback ->
    let interval_id = Window.set_interval (fun _ -> raw_spawn task ) interval in
    (fun _ -> Window.clear_interval interval_id)
  )

let rec spawn_help router intervals processes =
  match intervals with
  | [] -> 
    Task.succeed processes

  | interval :: rest ->
    let spawn_timer =
      Scheduler.spawn (schedule_interval interval (Fx.send_to_self router interval))
    in

    let spawn_rest id =
        spawn_help router rest (FloatMap.add interval id processes)
    in
    spawn_timer
    |> Task.and_then spawn_rest

let on_effects router subs {processes; _} : ('msg state, unit) Scheduler.task =
  let new_taggers =
    List.fold_left add_my_sub FloatMap.empty subs
  in

  let left_step interval _taggers (spawns, existing, kills) =
    (interval :: spawns, existing, kills)
  in

  let both_step interval _taggers id (spawns, existing, kills) =
    (spawns, FloatMap.add interval id existing, kills)
  in

  let right_step _ id (spawns, existing, kills) =
    (spawns, existing, Task.and_then (fun _ -> kills) (Scheduler.kill id))
  in

  let (spawn_list, existing_map, kill_task) =
    FloatMap.merge left_step both_step right_step new_taggers processes ([], FloatMap.empty, Task.succeed ())
  in
  kill_task
    |> Task.and_then (fun _ -> spawn_help router spawn_list existing_map)
    |> Task.and_then (fun new_processes -> Task.succeed {taggers = new_taggers; processes = new_processes})
      
let on_self_msg router interval state =
  match FloatMap.safe_find interval state.taggers with
  | None ->
    Scheduler.succeed state
  
  | Some taggers ->
    let tell_taggers time =
      Task.sequence (List.map (fun tagger -> Fx.send_to_app router (tagger time) ) taggers )
    in
    now
    |> Task.and_then tell_taggers
    |> Task.and_then (fun _ -> Task.succeed state)

let () = 
  Fx.sub_manager
  ~ctx: ctx
  ~init: init
  ~on_effects: on_effects
  ~on_self_msg: on_self_msg
  ~sub_map: sub_map