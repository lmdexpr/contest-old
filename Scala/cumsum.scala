// https://atcoder.jp/contests/abc154/submissions/10000261

object Main extends App {
	val sc = new java.util.Scanner(System.in)
	val pw = new java.io.PrintWriter(System.out)

	val n, k = sc.nextInt
	val p = Seq.fill(n)(sc.nextInt).map(pi => (1 + pi).toDouble / 2.0)
	val q = p.scanLeft(0.0)(_ + _)

	pw.println((0 to n-k).foldLeft(0.0){ case (res, i) =>
		Math.max(q(k + i) - q(i), res)
	})
	pw.flush()
}
