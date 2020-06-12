object Main extends App {
	val pw = new java.io.PrintWriter(System.out)

	@scala.annotation.tailrec
	def loop(start: Int, end: Int)(f: Int => Unit): Unit =
		if (start < end) {
			f(start)
			loop(start + 1, end)(f)
		}

	val n	   = Scanner.nextInt
	val x, y = Scanner.nextInt + 200
	val maze = Array.fill(401)(Array.fill(401)(true))
	maze(200)(200) = false

	val N = Math.max(x, y)

	loop(0, n) { _ =>
		val a, b = Scanner.nextInt + 200
		maze(b)(a) = false
	}

	val answers = scala.collection.mutable.ListBuffer.empty[Int]
	val depth = Array.tabulate(401)(maze(_).clone) map (_ map (if (_) 0 else -1))

	def bfs(seq : Seq[(Int, Int)]): Unit = seq match {
		case (p, q) :: tail => {
			if (p == x && q == y) answers += depth(q)(p)+1
			maze(q)(p) = false

			val ds = Seq((p+1, q+1), (p, q+1), (p-1, q+1), (p+1, q), (p-1, q), (p, q-1)).filter { case (i, j) =>
				0 <= i && i <= N && 0 <= j && j <= N && depth(j)(i) == 0 && maze(j)(i)
			}

			ds foreach { case (i, j) => depth(j)(i) = depth(q)(p) + 1 }

			bfs(tail ++ ds)
		}
		case _ => ()
	}

	bfs(Seq((200, 200)))

	pw.println(if (answers.isEmpty) -1 else answers.min)
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
