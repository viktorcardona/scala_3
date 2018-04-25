import barneshut.Quad
import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = new Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = nw.centerX + (ne.centerX - nw.centerX)/2 //??
    val centerY: Float = nw.centerY + (sw.centerY - nw.centerY)/2  //??
    val size: Float = nw.size + ne.size //???
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass //???
    val massX: Float = if (allEmpty) centerX else massXX //???
    val massY: Float = if (allEmpty) centerY else massYY //???
    val total: Int = nw.total + ne.total + sw.total + se.total

    def allEmpty = (nw.total==0 && ne.total==0 && sw.total==0 && se.total==0)
       //massX = (    m_B * x_B      +     m_C * x_C      +      m_D * x_D     +     m_E * x_E)       / mass

    //def massXX = (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass
    def massXX = (nw.mass * nw.massX + ne.mass * ne.massX + sw.mass * sw.massX + se.mass * se.massX) / mass

       //massY = (    m_B * y_B      +     m_C * y_C      +     m_D * y_D      +     m_E * y_E)       / mass
    def massYY = (nw.mass * nw.massY + ne.mass * ne.massY + sw.mass * sw.massY + se.mass * se.massY) / mass


    def insert(b: Body): Fork =
      quad2change(b) match {
        case "nw" => new Fork(insert(b, nw), ne, sw, se)
        case "ne" => new Fork(nw, insert(b, ne), sw, se)
        case "sw" => new Fork(nw, ne, insert(b, sw), se)
        case "se" => new Fork(nw, ne, sw, insert(b, se))
      }

    def quad2change(b: Body): String = {
      def isBodyInsideQuad(quad: Quad) = {
        val sizeHalf = quad.size/2
        val minX = quad.centerX - sizeHalf
        val maxX = quad.centerX + sizeHalf
        val minY = quad.centerY - sizeHalf
        val maxY = quad.centerY + sizeHalf
        b.x >= minX && b.x <= maxX && b.y >= minY && b.y <= maxY
      }
      //nw: Quad, ne: Quad, sw: Quad, se
      if (isBodyInsideQuad(nw)) "nw"
      else if (isBodyInsideQuad(ne)) "ne"
      else if (isBodyInsideQuad(sw)) "sw"
      else "se"
    }

    def insert(b: Body, quad: Quad) = quad.insert(b)

  }

  /*
  If the size of a Leaf is greater than a predefined constant minimumSize,
    inserting an additonal body into that Leaf quadtree creates a
    Fork quadtree with empty children, and adds all the bodies into
    that Fork (including the new body).
    Otherwise, inserting creates another Leaf with all the existing bodies and the new one.
  */
  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    //val (mass, massX, massY) = (??? : Float, ??? : Float, ??? : Float)
    val (mass, massX, massY) = (massValue, massXValue, massYValue)
    //val (mass, massX, massY) = (bodies.toStream.map(b => b.mass).sum : Float, bodies.toStream.map(b => b.mass*b.x).sum / mass : Float, bodies.toStream.map(b => b.mass*b.y).sum / mass : Float)

    private def massValue:Float = bodies.toStream.map(b => b.mass).sum

    //massX = (    m_B * x_B      +     m_C * x_C      +      m_D * x_D     +     m_E * x_E)       / mass
    //private def massXValue:Float = (bodies.toStream.map(b => b.mass*b.x).sum) / mass
    private def massXValue:Float = (bodies.toStream.map(b => b.mass*b.x).sum) / massValue

    //massY = (    m_B * y_B      +     m_C * y_C      +     m_D * y_D      +     m_E * y_E)       / mass
    private def massYValue:Float = (bodies.toStream.map(b => b.mass*b.y).sum) / massValue

    val total: Int = bodies.length // ???
    //def insert(b: Body): Quad = ???
    def insert(b: Body): Quad = if (amI2big) buildFork(b) else new Leaf(centerX, centerY, size, bodies++Seq(b))

    def amI2big = (size > minimumSize)

    private def buildFork(b: Body): Fork = {
      val halfSize = size/2
      val quarterSize = size/4
      def buildEmptyNw = new Empty(centerX - quarterSize, centerY - quarterSize, halfSize)
      def buildEmptyNe = new Empty(centerX + quarterSize, centerY - quarterSize, halfSize)
      def buildEmptySw = new Empty(centerX - quarterSize, centerY + quarterSize, halfSize)
      def buildEmptySe = new Empty(centerX + quarterSize, centerY + quarterSize, halfSize)
      var newFork = new Fork(buildEmptyNw, buildEmptyNe, buildEmptySw, buildEmptySe)

      for (i <- 0 until bodies.length) newFork = newFork.insert(bodies(i))
      newFork = newFork.insert(b)
      newFork
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def isSufficientlyFarAway(quad: Quad, body: Body): Boolean = {
        //the distance dist between the center of mass and the particle
        val dist = distance(quad.massX, quad.massY, body.x, body.y)
        quad.size / dist < theta
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          if (isSufficientlyFarAway(quad, this)) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            // or recursion is needed
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    /*
    This method should use the body position,
      boundaries and sectorPrecision to determine the
      sector into which the body should go into,
      and add the body into the corresponding ConcBuffer object.
    if the Body lies outside of the Boundaries,
      it should be considered to be located at the closest point
      within the Boundaries for the purpose of finding which
      ConcBuffer should hold the body.
    */
    def +=(b: Body): SectorMatrix = {
      def isBodyInsideBoundaries():Boolean =
          b.x >= boundaries.minX && b.x <= boundaries.maxX &&
          b.y >= boundaries.minY && b.y <= boundaries.maxY

      def getSector(xx: Float, yy: Float):Int = {
        /////////
        ///ojo que reste uno a xx y a yy !!!!!!!!!!
        /////////
        val sector = (math.abs(xx - 1)/sectorSize).toInt + ((math.abs(yy - 1)/sectorSize).toInt*sectorPrecision)
        if (sector < 0 || sector >= matrix.length) {
          println("...Wrong value for sector: " + sector + " matrix.length: " + matrix.length + "  xx: " + xx + " yy: " + yy)
          if (sector < matrix.length/2) 0 else matrix.length - 1
        } else sector
      }

      def getClosestSector(): Int = {
        def getClosestX(): Float = {
          if (b.x >= boundaries.minX && b.x <= boundaries.maxX)  b.x
          else if (b.x < boundaries.minX) boundaries.minX
          else boundaries.maxX
        }
        def getClosestY(): Float = {
          if (b.y >= boundaries.minY && b.y <= boundaries.maxY)  b.y
          else if (b.y < boundaries.minY) boundaries.minY
          else boundaries.maxY
        }
        getSector(getClosestX(), getClosestY())
      }

      val sectorValue = (if (isBodyInsideBoundaries()) getSector(b.x, b.y) else getClosestSector()) //- 1
      println("--------------------------------------")
      println("sectorPrecision:"+sectorPrecision+", sectorSize:"+sectorSize +", boundaries:"+boundaries+" size: "+boundaries.size+", sectorValue:"+sectorValue+", b.x: "+b.x+ ", b.y: "+b.y)
      matrix(sectorValue) = matrix(sectorValue).+=(b)
      println("--------------------------------------")
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      var newSectorMatrix = new SectorMatrix(this.boundaries, this.sectorPrecision)
      for (i <- 0 until this.matrix.length) newSectorMatrix.matrix(i) = this.matrix(i).combine(that.matrix(i))
      newSectorMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
