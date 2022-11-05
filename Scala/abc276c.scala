object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start < end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val n = Scanner.nextInt
  
  val p = List.fill(n)(Scanner.nextInt).reverse

  def unsortedCount(i: Int = 0, pred: Int = 101): List[Int] => Int = {
    case Nil => i
    case h :: t => if (h > pred) i+1 else unsortedCount(i+1, h)(t)
  }

  val (a, b) = p.splitAt(unsortedCount()(p))

  val (s, l) = a.partition(_ <= a.last)

  val (ss, thr) = (s.init.sorted, s.last)
  val (sss, t)  = (ss.init, ss.last)

  val ans = b.reverse ++ List(t) ++ (thr :: sss ++ l).sorted.reverse

  println(ans.mkString(" "))
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
