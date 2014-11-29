package boardgame

object Boardgame {

  
  val tileProps:Map[TileType,Double] = Map(AIR -> .1,DIRT -> .3, ROCK -> .2 , IRON -> .1, COAL -> .1, GOLD -> .05, SPECIAL -> .05)
  private val tileTypeSeq = tileProps.keys.toIndexedSeq
  private val tileMultinom = new cltools.distributions.Multinom(tileTypeSeq.map((t:TileType) => tileProps.get(t).get))
  
  def sampleTile = {
    tileTypeSeq(tileMultinom.sample).instance
  }
  //assert(tileProps.toSet.map((x:(TileType,Double)) => x._2).sum == 1.0)
  
  def main(args:Array[String]) = {
    val b = new Board
    b.init((0,0),12)
    println(b.print)
    for(i <- 0 until 20) {
      b.dig(5,i)
      println("--\n"+b.print)
    }
  }
  
}

class Board {
  
  var tiles = Map.empty[(Int,Int),Tile]
  
  def max = {
    (tiles.keys.map(_._1).max,tiles.keys.map(_._2).max)
  }
  
  def min = {
    (tiles.keys.map(_._1).min,tiles.keys.map(_._2).min)
  }
  
  def init(upperLeft:(Int,Int),size:Int) = {
    for(x <- upperLeft._1 to (upperLeft._1+size);y <- upperLeft._2 to (upperLeft._2+size)) {
      tiles = tiles + ((x,y) -> Boardgame.sampleTile)
    }
  }
  
  def print = {
    val mn = min
    val mx = max
    val colT = for(y <- mn._2 to mx._2) yield {
      val rowT = for(x <- mn._1 to mx._1) yield {
        getTile((x,y)).name
      }
      rowT.mkString("")
    }
    colT.mkString("\n")
  }
  
  def getTile(xy:(Int,Int)):Tile = {
    tiles.getOrElse(xy,UNKNOWN.instance)
  }
  
  def dig(xy:(Int,Int)) = {
    val prevType = getTile(xy)
    updateTile(xy,AIR.instance)
    for(x <- xy._1-1 to xy._1+1;y <- xy._2-1 to xy._2+1) {
      if((x,y) != xy && getTile((x,y)).tType == UNKNOWN) {
        updateTile((x,y),Boardgame.sampleTile)
      }
    }
    prevType
  }
  
  def updateTile(xy:(Int,Int),tt:Tile) = {
    tiles = tiles.updated(xy, tt)
  }
}

class Tile(val tType:TileType,val copies:Set[TileType]) {
  def name = tType.name
}

trait TileType {
  def name:String
  def instance:Tile = new Tile(this,Set.empty)
}

case object AIR extends TileType {val name = " "};
case object DIRT extends TileType {val name = "."};
case object ROCK extends TileType {val name = ","};
case object IRON extends TileType {val name = "i"};
case object COAL extends TileType {val name = "c"};
case object GOLD extends TileType {val name = "G"};
case object UNKNOWN extends TileType {val name = "?"};
case object SPECIAL extends TileType {val name = "S"};