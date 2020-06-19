object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	@scala.annotation.tailrec
	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start < end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val s = Scanner.next.map(_ == 'R')
	val n = s.size
	val a = Array.fill(n)(0)

	var acc = 0
	var bcc = 0

	loop(0, n) { i =>
		if (s(i)) {
			if (s(i+1)) acc += 1
			else {
				a(i)   += (acc / 2) + 1
				a(i+1) += (acc / 2.0).ceil.toInt
				acc = 0
			}
		}
		if (!s(n-1-i)) {
			if (!s(n-2-i)) bcc += 1
			else {
				a(n-1-i) += (bcc / 2) + 1
				a(n-2-i) += (bcc / 2.0).ceil.toInt
				bcc = 0
			}
		}
	}

	loop(0, n) { i => pw.print(s"${a(i)} ") }
	pw.println()
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
