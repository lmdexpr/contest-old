object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	@scala.annotation.tailrec
	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start < end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val n = Scanner.nextInt
	val k = Scanner.nextLong
	val a = Array.fill(n)(Scanner.nextInt - 1)

	val l = Array.fill(n)(true)
	var p = 0
	var closed = 0
	while (l(p)) {
		closed += 1
		l(p) = false
		p = a(p)
	}

	var ans = 0
	var branch = 0
	while(ans != p) {
		branch += 1
		ans = a(ans)
	}
	closed -= branch

	loop(0, ((k-branch) % closed).toInt) { _ => ans = a(ans) }

	pw.println(ans+1)
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
