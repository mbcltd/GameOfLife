//
// Problem Description [http://codingdojo.org/cgi-bin/index.pl?KataGameOfLife]
//
// This Kata is about calculating the next generation of Conway's game of life, given any starting position. See http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life for background.
//
// You start with a two dimensional grid of cells, where each cell is either alive or dead. In this version of the problem, the grid is finite, and no life can exist off the edges. When calculating the next generation of the grid, follow these rules:
//
//   1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
//   2. Any live cell with more than three live neighbours dies, as if by overcrowding.
//   3. Any live cell with two or three live neighbours lives on to the next generation.
//   4. Any dead cell with exactly three live neighbours becomes a live cell.
// You should write a program that can accept an arbitrary grid of cells, and will output a similar grid showing the next generation.
//


object GameOfLife extends App {
  val input =
    ".........\n"+
    "..*......\n"+
    ".***.....\n"+
    "..*......\n"+
    ".........\n"

  val grid = Grid(input)


  val grids = grid.next(3)

  grids.foreach(println)


}

object Grid {
  // Parse a string into a Grid
  def apply(s:String):Grid = {
    val rows = s.split('\n')
    val widestRow = rows.map( _.length ).max

    val liveLocations = for {
      row <- rows.zipWithIndex
      cells <- row._1.toCharArray.zipWithIndex
      if cells._1 == '*'
    } yield Location(cells._2, row._2)

    Grid( liveLocations, widestRow, rows.size )
  }
}

case class Grid(liveLocations:Array[Location], width:Int, height:Int) {
  def isLive(l:Location) = liveLocations.contains(l)

  def liveCount(l:Location) = if (isLive(l)) 1 else 0

  def neighbourCount(l:Location):Int = l.neighbours.map( liveCount ).sum

  def rowToString(y:Int):String = (0 to width).map( x => if ( isLive(Location(x,y)) ) '*' else '.' ).mkString

  def nextGenerationAlive(l:Location) = if (isLive(l)) neighbourCount(l) > 1 && neighbourCount(l) < 4 else neighbourCount(l) == 3

  def nextLiveLocations = for {
    x <- 0 to width
    y <- 0 to height
    if nextGenerationAlive(Location(x,y))
  } yield Location(x,y)

  def next = Grid(nextLiveLocations.toArray,width,height)

  def next(count:Int):List[Grid] = if (count == 0) Nil else this.next :: this.next.next( count -1 )

  override def toString = (0 to height).map( rowToString ).mkString("\n")
}

case class Location(x:Int,y:Int) {
  def neighbourCells = for { i <- -1 to 1; j <- -1 to 1; if !(i == 0 && j == 0) } yield (i,j)
  def neighbours = neighbourCells.map( o => Location(x+o._1,y+o._2) )
}