(* trivial loop waisting time with no mem alloc *)
fun cons_many 0 = 0
  | cons_many n = ([ n ]; cons_many (n - 1))
    
(* make m tasks each making n cons cells *)
fun make_tasks n 0 = 0
  | make_tasks n 1 = cons_many n
  | make_tasks n m = 
    let
	val tx = Myth.Thread.create (fn () => make_tasks n (m div 2))
	val y  = make_tasks n (m - m div 2)
    in
	(Myth.Thread.join tx) + y
    end

val sml_dump_alloc_time = _import "sml_dump_alloc_time" : () -> ()

(* measure rep times "make m tasks each making n cons cells" *)
fun do_it n m 0 = 0
  | do_it n m rep = 
    let
	val t0 = Time.now ()
	val x = make_tasks n m
	val t1 = Time.now ()
    in
	print (Time.fmt 6 (Time.- (t1, t0))); print "\n";
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
