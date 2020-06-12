// https://atcoder.jp/contests/abc157/submissions/10474224

object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	@scala.annotation.tailrec
	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start <= end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val n, m, k = Scanner.nextInt

	val friend, blocked = Array.fill(n+1)(0)
	val uf = UnionFind(1 to n)

	Seq.fill(m)((Scanner.nextInt, Scanner.nextInt)) foreach { case (a, b) =>
		uf.unite(a, b)
		friend(a) = friend(a) + 1
		friend(b) = friend(b) + 1
	}

	Seq.fill(k)((Scanner.nextInt, Scanner.nextInt)) foreach { case (c, d) =>
		if (uf.same(c,d)) {
			blocked(c) = blocked(c) + 1
			blocked(d) = blocked(d) + 1
		}
	}

	loop(1, n) { i =>
		pw.print( (uf.size(i) - 1 - friend(i) - blocked(i)).toString + " " )
	}
	pw.flush()
}

case class UnionFind[T](seq: Seq[T]) {
	import scala.collection.mutable.Map
	private[this] val par: Map[T, T] = Map[T,T](seq.map(elem => elem -> elem): _*)
	private[this] val sz: Map[T, Int] = Map[T, Int](seq.map(elem => elem -> 1): _*)

	@scala.annotation.tailrec
	private[this] def find(t: T, acc: Seq[T] = Nil): T = {
		val p = par(t)
		if (p == t) {acc foreach { par(_) = t }; t} else find(p, p +: acc)
	}

	def unite(a: T, b: T): Unit = {
		val pa = find(a)
		val pb = find(b)

		if (pa == pb) return

		if (sz(pa) <= sz(pb)) {
			par(pa) = pb
			sz(pb)  = sz(pa) + sz(pb)
		} else {
			par(pb) = pa
			sz(pa)  = sz(pa) + sz(pb)
		}
	}

	def same(a: T, b: T) = find(a) == find(b)

	def size(a: T) = sz(find(a))
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
