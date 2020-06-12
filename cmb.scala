// https://atcoder.jp/contests/abc145/submissions/9588265?lang=ja

object Main extends App {
	val sc = new java.util.Scanner(System.in)
	val pw = new java.io.PrintWriter(System.out)

	val x, y = sc.nextInt

	val M = BigInt(1000000007)
	val nm = (x+y) / 3

	def cmb(n: Int, k: Int): BigInt =
		(BigInt(1) to BigInt(k)).foldLeft(BigInt(1))((z, i) => (n - k + i) * i.modInverse(M) * z % M)

	pw.println(if (((x+y) % 3 != 0) || (2*y < x || 2*x < y)) 0 else cmb(nm, x - nm).toInt)
	pw.flush()
}
