type pthread_t = unit ptr   (* ToDo: system dependent *)
val pthread_create =
    _import "pthread_create"
    : (pthread_t ref, unit ptr, unit ptr -> unit ptr, unit ptr) -> int

val pthread_join =
    _import "pthread_join"
    : (pthread_t, unit ptr ref) -> int

fun th_create f =
  let
      val ret = ref _NULL
      val err = pthread_create (ret, _NULL, f, _NULL)
      val ref th = ret
  in
      if err = 0 then () else raise Fail "pthread_create";
      th
  end

fun th_join t =
  let 
      val ret = ref _NULL
      val _ = pthread_join (t, ret)
      val ref v = ret
  in
      v
  end

(* trivial loop waisting time with no mem alloc *)
fun cons_many 0 = _NULL
  | cons_many n = ([ n ]; cons_many (n - 1))
    
(* make m tasks each making n cons cells *)
fun make_tasks n 0 = _NULL
  | make_tasks n 1 = cons_many n
  | make_tasks n m = 
    let
	val tx = th_create (fn _ => make_tasks n (m div 2))
	val y  = make_tasks n (m - m div 2)
    in
	th_join tx; y
    end

val sml_dump_alloc_time = _import "sml_dump_alloc_time" : () -> ()
val sml_reset_alloc_time = _import "sml_reset_alloc_time" : () -> ()
val get_tsc = _import "get_tsc" : () -> int64

(* measure rep times "make m tasks each making n cons cells" *)
fun do_it n m 0 = 0
  | do_it n m rep = 
    let
	val _ = sml_reset_alloc_time ()
	val t0 = get_tsc() (* Time.now () *)
	val x = make_tasks n m
	val t1 = get_tsc() (* Time.now () *)
	(* val _ = (print (Time.fmt 6 (Time.- (t1, t0))); print "\n") *)
	val _ = (print (Int64.toString (t1 - t0)); print "\n")
    in
	do_it n m (rep - 1)
    end
    
fun parse_int_nth args n default =
  if n < List.length args then
      case Int.fromString (List.nth (args,n)) of
	  SOME x => x
	| NONE => default
  else
      default
    
fun main args =
  let
      val n = parse_int_nth args 0 10000000
      val m = parse_int_nth args 1 40
      val r = parse_int_nth args 2 5
  in
      print ("app=cons\n");
      print ("conses=" ^ (Int.toString n) ^ "\n");
      print ("tasks="  ^ (Int.toString m) ^ "\n");
      print ("repeat=" ^ (Int.toString r) ^ "\n");
      do_it n m r;
      sml_dump_alloc_time ()
  end

(* usage:
   MYTH_NUM_WORKERS=M_TASKS ./cons_many N_CONS_CELLS_PER_TASK M_TASKS REPEAT_TIMES
 *)
val _ = main (CommandLine.arguments())
