package streams

import common._

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     inside the terrain)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
 */
trait StringParserTerrain extends GameDef {

  final val charStartPos = 'S'
  final val charFinalPos = 'T'
  final val charValidPos = 'o'
  final val charInValidPos = '-'
  final val setValidPos = Set(charStartPos,charFinalPos,charValidPos)

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This method returns terrain function that represents the terrain
   * in `levelVector`. The vector contains parsed version of the `level`
   * string. For example, the following level
   *
   *   val level =
   *     """ST
   *       |oo
   *       |oo""".stripMargin
   *
   * is represented as
   *
   *   Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
   *
   * The resulting function should return `true` if the position `pos` is
   * a valid position (not a '-' character) inside the terrain described
   * by `levelVector`.
   */
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    (pos: Pos) =>
      (0 until levelVector.length contains pos.row) &&
        (0 until levelVector(pos.row).length contains pos.col) &&
        (setValidPos contains levelVector(pos.row)(pos.col))

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = levelVector match {
    case Vector() => throw new IllegalArgumentException("empty.levelVector")
    case vector: Vector[Vector[Char]] =>
      val foundedPos: Seq[Pos] =
      ((0 until vector.length) flatMap {
        row => {
          (0 until vector(row).length).withFilter(col => vector(row)(col) == c)
            .map(col => Pos(row,col))
        }
      })
      if(foundedPos.isEmpty) throw new ArrayIndexOutOfBoundsException("pos.not.found")
      else foundedPos(0)
  }

  private lazy val vector: Vector[Vector[Char]] =
    Vector(level.split("\n").map(str => Vector(str: _*)): _*)

  lazy val terrain: Terrain = terrainFunction(vector)
  lazy val startPos: Pos = findChar(charStartPos, vector)
  lazy val goal: Pos = findChar(charFinalPos, vector)

}

