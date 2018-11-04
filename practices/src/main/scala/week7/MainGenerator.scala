package week7

trait Generator[+T]{
  self =>
  def generate: T

  // Necesito que los Generators tengan map y flatMap. Sino no funcionan los for-compresion
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }

  override def toString: String = generate.toString
}

object MainGenerator extends App{

  val integers = new Generator[Int]{
    val rand = new java.util.Random
    override def generate: Int = rand.nextInt()
  }

  val booleans = new Generator[Boolean] {
    override def generate: Boolean = integers.generate > 0
  }

  val pairsInt = new Generator[(Int,Int)] {
    override def generate: (Int, Int) = (integers.generate,integers.generate)
  }

  /*
  // Other version
  def pairs[T,U](t:Generator[T], u:Generator[U]): Generator[(T,U)] =
    for{
      x <-t
      y <-u
    } yield (x, y)
  */

  def pairs[T,U](t:Generator[T], u:Generator[U]): Generator[(T,U)] = new Generator[(T, U)] {
    override def generate: (T, U) = (t.generate,u.generate)
  }



  // Necesito que los Generators tengan map y flatMap. Sino no funcionan los for-compresion
  println("Random integer:" +
    (for{
      i <- integers
    } yield i)
  )

  println("Random boolean:" +
    (for{
      i <- booleans
    } yield i)
  )

  println("Random pairs:" +
    (for{
      i <- pairs(integers,integers)
    } yield i)
  )


  def singleGenerator[T](x:T) = new Generator[T] {
    override def generate: T = x
  }

  def choose(low: Int, high: Int): Generator[Int] =
    for(x <- integers) yield Math.abs(low + x % (high - low))

  def oneOfGenerator[T](xs: T*): Generator[T] =
    for( idx <- choose(0,xs.length)) yield xs(idx)

  println("Random oneOfGenerator:" +
    (for{
      i <- oneOfGenerator('A','B','C','D')
    } yield i).toString
  )


  def nonEmptyList = for{
    head <- integers
    tail <- listIntGenerator
  } yield head :: tail

  def listIntGenerator: Generator[List[Int]] =
    for{
      isEmpty <- booleans
      list <- if(isEmpty) singleGenerator(List.empty) else nonEmptyList
    } yield list

  println("Random list of Int:" +
    (for{
      list <- listIntGenerator
    } yield list)
  )

  // Exercise: TreeGenerator

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def innerTree: Generator[Inner] = for{
    left <- treeGenerator
    right <- treeGenerator
  } yield Inner(left,right)

  def leafTree: Generator[Leaf] = for{
    leaf <- integers
  } yield Leaf(leaf)

  def treeGenerator: Generator[Tree] = for{
    isLeaf <- booleans
    tree <- if (isLeaf) leafTree else innerTree
  } yield tree

  println("Random tree:" +
    (for{
      tree <- treeGenerator
    } yield tree)
  )

  // Necesito que los Generators tengan withFilter.
  // Sino los filtros en el for-compresion no funcionan
  /*println("Random tree only Leafs:" +
    (for{
      Leaf(leafs) <- treeGenerator
    } yield leafs)
  )*/

  /*
    TESTS
   */
  def test[T](g: Generator[T], numTimes: Int = 100)
    (testFunc: T => Boolean): Unit ={
    for (_ <- 0 until numTimes){
      val value = g.generate
      assert(testFunc(value), "test failed for value "+ value)
    }
    println("Passed test "+numTimes+" times")
  }

  test(pairs(listIntGenerator,listIntGenerator)){
    case (xs,ys) => (xs ++ ys).length >= xs.length
  }

}
