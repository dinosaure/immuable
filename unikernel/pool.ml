module Atomic = struct
  include Atomic

  let[@inline] fenceless_get (a : 'a Atomic.t) =
    !(Sys.opaque_identity (Obj.magic a : 'a ref))

  let[@inline] fenceless_set (a : 'a Atomic.t) v =
    (Obj.magic a : 'a ref) := v
end

module Ws = struct
  type 'a t =
    { rd : int Atomic.t
    ; wr : int Atomic.t
    ; rd_cached : int ref
    ; mutable arr : 'a ref array }

  let realloc arr rd wr size new_size =
    let _arr' = Array.make new_size (Obj.magic ()) in
    assert false

  let push t v =
    let cell = ref v in
    let wr_seen = Atomic.fenceless_get t.wr in
    let rd_cached = !(t.rd_cached) in
    let arr = t.arr in
    let size = wr_seen - rd_cached in
    let capacity = Array.length arr in
    if size < capacity || let rd = Atomic.get t.rd in t.rd_cached := rd; rd != rd_cached
    then begin
      Array.set arr (wr_seen land (capacity - 1)) cell;
      Atomic.incr t.wr
    end else begin
      let arr = realloc arr rd_cached wr_seen capacity (capacity lsl 1) in
      Array.set arr (wr_seen land (Array.length arr - 1)) cell;
      t.arr <- arr;
      Atomic.incr t.wr
    end

  let min_capacity = 16

  exception Empty

  let pop t =
    let wr = Atomic.fetch_and_add t.wr (-1) - 1 in
    let rd = Atomic.fenceless_get t.rd in
    let size = wr - rd in
    if 0 < size then begin
      let arr = t.arr in
      let capacity = Array.length arr in
      let out = Array.get arr (wr land (capacity - 1)) in
      let res = !out in
      out := Obj.magic ();
      if size + size + size <= capacity - min_capacity then
        t.arr <- realloc arr rd wr capacity (capacity lsr 1);
      res
    end else if wr = rd then begin
      t.rd_cached := rd + 1;
      let got = Atomic.compare_and_set t.wr wr (wr + 1) in
      Atomic.fenceless_set t.rd (rd + 1);
      if got then begin
        let arr = t.arr in
        let out = Array.get arr (rd + 1) in
        let res = !out in
        out := Obj.magic ();
        res
      end else raise Empty
    end else begin
      Atomic.fenceless_set t.wr (wr + 1);
      raise Empty
    end

  let rec steal t backoff =
    let rd = Atomic.fenceless_get t.rd in
    let wr = Atomic.get t.wr in
    let size = wr - rd in
    if 0 < size then
      let arr = t.arr in
      let out = Array.get arr (rd land (Array.length arr - 1)) in
      if Atomic.compare_and_set t.rd rd (rd + 1) then begin
        let res = !out in
        out := Obj.magic ();
        res
      end
      else steal t (Miou.Backoff.once backoff)
    else raise Empty
end

module CM = struct
  type t =
    { mutex : Miou.Mutex.t
    ; condition : Miou.Condition.t }
end

module Chan = struct
  type 'a t =
    { buffer_size : int option
    ; contents : 'a contents Atomic.t }
  and 'a contents =
    | Empty of { recvs : ('a option ref * CM.t) Q.t }
    | Not_empty of { sends : ('a * flag ref * CM.t) Q.t; msgs: 'a Q.t }
  and flag =
    | Waiting
    | Notified
end

type 'a t =
  { create : unit -> 'a }

let create_member t =


let validate_and_return t v =
  try
    if t.validate v then v
    else begin
      t.dispose t v;
      create_member t
    end
  with exn ->
    let () = t.dispose t v in
    replace_disposed t;
    reraise exn

let acquire t =
  match Miou.Queue.dequeue t.pending with
  | exception Miou.Queue.Empty ->
  | v -> validate_and_return t v

let use t fn =
  acquire t 
