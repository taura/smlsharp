(* trivial loop waisting time with no mem alloc *)

fun inc a i n =
  if i = n then
      0
  else
      let
          val _ = Array.update (a, 0, (Array.sub (a, 0)) + 1)
      in
          inc a (i + 1) n
      end
    
(* make m tasks each making n cons cells *)
fun make_tasks n 0 = 0
  | make_tasks n 1 =
    let
        val a = Array.array (1, 0)
    in
        inc a 0 n
    end
  | make_tasks n m = 
    let
	val tx = Myth.Thread.create (fn () => make_tasks n (m div 2))
	val y  = make_tasks n (m - m div 2)
        val _ = Myth.Thread.join tx
    in
        y
    end
        
(* measure rep times "make m tasks each making n cons cells" *)
fun do_it n m 0 = 0
  | do_it n m rep = 
    let
	val t0 = Time.now ()
	val x = make_tasks n m
	val t1 = Time.now ()
    in
	print ((Time.fmt 6 (Time.- (t1, t0))) ^ "\n");
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
      val n = parse_int_nth args 0 1000000
      val m = parse_int_nth args 1 40
      val r = parse_int_nth args 2 5
  in
      print (Int.toString n); print "\n";
      print (Int.toString m); print "\n";
      print (Int.toString r); print "\n";
      do_it n m r
  end

(* usage:
   MYTH_NUM_WORKERS=M_TASKS ./cons_many N_CONS_CELLS_PER_TASK M_TASKS REPEAT_TIMES
 *)
val _ = main (CommandLine.arguments())
