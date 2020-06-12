// https://atcoder.jp/contests/abc151/submissions/9508836?lang=ja

object Main extends App {
	val sc = new java.util.Scanner(System.in)
	val pw = new java.io.PrintWriter(System.out)

	val h, w = sc.nextInt
	val s = Array.fill(h)(sc.next.toArray) map (_ map (_ == '.'))

	def bfs(sx: Int, sy: Int): Int = {
		val a = Array.tabulate(h)(s(_).clone) map (_ map (if (_) 0 else -1))

		def inner(seq : Seq[(Int, Int)]): Int = seq match {
			case Seq() => a.flatten.max
			case (x, y) :: tail => {
				val ds = Seq( (x-1, y), (x+1, y), (x, y-1), (x, y+1) ).filter { case (i, j) =>
					(i, j) != (sx, sy) && 0 <= i && i < w && 0 <= j && j < h && a(j)(i) == 0
				}
				ds foreach {case (q, p) => a(p)(q) = a(y)(x) + 1}
				inner(tail ++ ds)
			}
		}

		inner(Seq((sx, sy)))
	}

	pw.println((for (x <- 0 until w; y <- 0 until h if s(y)(x)) yield bfs(x, y)).max)
	pw.flush()
}
