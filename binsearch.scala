// https://atcoder.jp/contests/abc146/submissions/9509699?lang=ja

object Main extends App {
	val sc = new java.util.Scanner(System.in)
	val a, b, x = sc.nextLong

	def ok(n: Long): Boolean = a * n + b * (if (n == 0) 0 else n.toString.length) > x

	def binsearch(l: Long, r: Long): Long = {
		val m = (l + r) / 2
		if (ok(m)) binsearch(l, m) else if (r - m < 2) m else binsearch(m, r)
	}

	println(binsearch(0, 1000000001))
}
