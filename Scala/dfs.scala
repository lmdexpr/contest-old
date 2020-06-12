// https://atcoder.jp/contests/abc165/tasks/abc165_c
// 通ってないぞ

object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	@scala.annotation.tailrec
	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start < end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val n, m = Scanner.nextInt

	case class Query(a: Int, b: Int, c: Int, d: Int)
	val queries = Seq.fill(Scanner.nextInt)(Query(Scanner.nextInt-1, Scanner.nextInt-1, Scanner.nextInt, Scanner.nextInt))

	@inline def score(x: Array[Int]): Int = queries.collect{ case Query(a, b, c, d) if(x(b) - x(a) == c) => d }.sum

	def dfs(x: Array[Int], digit: Int, maxDig: Int): Int =
		if (digit < 0) score(x)
		else Math.max(score(x), (2 to maxDig).map{ di => x(digit) = di; dfs(x.clone, digit - 1, di) }.max)

	pw.println( dfs(Array.fill(n)(1), n-1, m) )
	pw.flush()
}

object Scanner {
	private val buf = new Array[Byte](1024); private var ptr = 0; private var len = 0

	@inline private def isPrintableChar(c: Int): Boolean = 33 <= c && c <= 126
	@inline private def hasNextByte(): Boolean = if (ptr >= len) { ptr = 0; len = System.in.read(buf); len > 0 } else true
	@inline private def hasNext(): Boolean = {
		while (hasNextByte() && !isPrintableChar(buf(ptr))) ptr += 1
		hasNextByte()
	}
	@inline private def readByte(): Byte = if (hasNextByte()) { val res = buf(ptr); ptr += 1; res } else -1

	def next(): String = {
		if(!hasNext()) ???
		val sb = new StringBuilder; var b = readByte()
		while (isPrintableChar(b)) { sb.append(b.toChar); b = readByte() }
		sb.toString
	}

	def nextInt(): Int = {
		val n = nextLong()
		if (n < Int.MinValue || Int.MaxValue < n) ???
		n.toInt
	}

	def nextLong(): Long = {
		if(!hasNext()) ???
		var minus = false; var b = readByte()
		if (b == '-') { minus = true; b = readByte() }

		@scala.annotation.tailrec
		def go (b: Byte, n: Long = 0): Long = if ('0' <= b && b <= '9') go(readByte(), n * 10 + b - '0') else if (minus) -n else n

		go(b)
	}

	def nextDouble(): Double = next().toDouble
}
