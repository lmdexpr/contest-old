// https://atcoder.jp/contests/abc156/submissions/10293783

object Main extends App {
	val sc = new java.util.Scanner(System.in)
	val pw = new java.io.PrintWriter(System.out)

	val n, a, b = sc.nextInt

	val M = BigInt(1000000007)

	def cmb(k: Int): BigInt =
		(BigInt(1) to BigInt(k)).foldLeft(BigInt(1))((z, i) => (n - k + i) * i.modInverse(M) * z % M)

	def pow2(k: Int): BigInt =
		if (k == 0) 1
		else if (k % 2 == 0) {
			val r = pow2(k/2)
			r * r % M
		} else BigInt(2) * pow2(k-1) % M

	val r = (pow2(n) - BigInt(1) - cmb(a) - cmb(b)) % M

	pw.println(r + (if(r < 0) M else 0))
	pw.flush()
}
