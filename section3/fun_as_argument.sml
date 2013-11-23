(*function as argruments*)
fun increment_n_times_lame (n, x) =
	if n = 0
	then x
	else 1 + increment_n_times_lame(n-1, x)

fun double_n_times_lame (n, x) =
	if n = 0
	then x
	else 2 * double_n_times_lame(n-1, x)

fun nth_tail_lame(n, xs) = 
	if n = 0
	then xs
	else tl (nth_tail_lame(n-1, xs))

fun n_times(f, n, x) =
	if n = 0
	then x
	else f (n_times(f, n-1, x))

fun increment x = x+1
fun double x = x+x

val x1 = n_times(double, 4, 7)
fun addition(n, x) = n_times(increment, n, x)

