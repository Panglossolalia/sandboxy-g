package mapgen

object Room {
  
  var nextRoomType:RoomType = GenericRoom
  
  def fromOrigin(x:Int,y:Int,w:Int,h:Int):Room = {
    new Room((x,x+w),(y,y+h))
  }
  
  def fromPoints(p0:(Int,Int),p1:(Int,Int)) = {
    new Room((math.min(p0._1,p1._1),math.max(p0._1,p1._1)),(math.min(p0._2,p1._2),math.max(p0._2,p1._2)))
  }
  
  def overlaps(r0:Room,r1:Room):Boolean = {
    val overlapX = rc(r1.rangeX._1,r0.rangeX) || rc(r1.rangeX._2,r0.rangeX) || rc(r0.rangeX._1,r1.rangeX) || rc(r0.rangeX._2,r1.rangeX)
    val overlapY = rc(r1.rangeY._1,r0.rangeY) || rc(r1.rangeY._2,r0.rangeY) || rc(r0.rangeY._1,r1.rangeY) || rc(r0.rangeY._2,r1.rangeY)
    overlapX && overlapY
  }
  
  private def rc(x:Int,range:(Int,Int)):Boolean = {
    return (x >= range._1  && x <= range._2)
  }
  
  def expand(r:Room,m:Int):Room = {
    new Room((r.rangeX._1-m,r.rangeX._2+m),(r.rangeY._1-m,r.rangeY._2+m))
  }
  
}

class Room(val rangeX:(Int,Int),val rangeY:(Int,Int),val roomType:RoomType=Room.nextRoomType) {
  assert(rangeX._2 >= rangeX._1)
  assert(rangeY._2 >= rangeY._1)
  
  def overlaps(r:Room) = {
    Room.overlaps(this,r)
  }
  
  // Inflates one room. Used to establish that two rooms are adjacent, or separated by a fixed space.
  def marginOverlap(r:Room,margin:Int):Boolean = {
    Room.overlaps(Room.expand(this,margin),Room.expand(r,0))
  }
  
  def start = {
    (rangeX._1,rangeY._1)
  }
  
  def size = {
    // size is inclusive of the end
    (rangeX._2-rangeX._1+1,rangeY._2-rangeY._1+1)
  }
  
  def adjoiningRoom(wall:Int,originDepth:Int,endDepth:Int,originLateral:Int,endLateral:Int) = {
    assert(originDepth <= endDepth)
    val rSz=this.size
    val endBuffer=1;
    val originBuffer=1
    val rOut:Room = wall match {
      case 0 => { // North
        new Room((rangeX._1+originLateral,rangeX._1+(rSz._1-endLateral-1)),(rangeY._2+originDepth+originBuffer,rangeY._2+endDepth+endBuffer))
      }
      case 1 => { // East
        new Room((rangeX._2+originDepth+originBuffer,rangeX._2+endDepth+endBuffer),(rangeY._1+originLateral,rangeY._1+(rSz._2-endLateral-1)))
      }
      case 2 => { // South
        Room.fromPoints((rangeX._1+originLateral,rangeY._1-originDepth-originBuffer),(rangeX._1+(rSz._1-endLateral-1),rangeY._1-endDepth-endBuffer))
      }
      case 3 => { // West 
        Room.fromPoints((rangeX._1-originDepth-originBuffer,rangeY._1+originLateral),(rangeX._1-endDepth-endBuffer,rangeY._1+(rSz._2-endLateral-1)))
      }
    }
    rOut
  }
  
  def adjoiningRoomWithDoor(wall:Int,endDepth:Int,originLateral:Int,endLateral:Int):(Room,Door) = {
    assert(endDepth >= 1)
    val rSz=this.size
    val originDepth=1
    
    val direction = if(wall <2) 1 else -1
    val endOffset = direction*(endDepth+1)
    val startOffset = direction*(originDepth+1)
    
    val rOut = wall match {
      case 0 => { // North
         Room.fromPoints((rangeX._1+originLateral,           rangeY._2+startOffset),
                          (rangeX._1+(rSz._1-endLateral-1),   rangeY._2+endOffset))
            //new Door(rangeX._1+math.max(0,originLateral),rangeY._2+1))
      }
      case 1 => { // East
         Room.fromPoints((rangeX._2+startOffset,rangeY._1+originLateral),
                          (rangeX._2+endOffset,      rangeY._1+(rSz._2-endLateral-1)))
            //new Door(rangeX._2+1,rangeY._1+math.max(0,originLateral)))
      }
      case 2 => { // South
        Room.fromPoints((rangeX._1+originLateral,            rangeY._1+startOffset),
                         (rangeX._1+(rSz._1-endLateral-1),    rangeY._1+endOffset))
            //new Door(rangeX._1+math.max(0,originLateral),rangeY._1-1))
      }
      case 3 => { // West 
        Room.fromPoints((rangeX._1+startOffset, rangeY._1+originLateral),
        		         (rangeX._1+endOffset,       rangeY._1+(rSz._2-endLateral-1)))
            //new Door(rangeX._1-1,rangeY._1+math.max(0,originLateral)))
      }
    }
    (rOut,connectingDoor(this,rOut,wall,1))//dOut)
  }
  

  // if offset is 1, door is at the positive extreme coordinate.
  // if offset is -1, door is at the negative extreme coordinate.
  // Increasing offset magnitudes bring position toward center from extreme.
  def connectingDoor(r0:Room,r1:Room,wall:Int,offset:Int):Door = {
	assert(offset != 0) // 1 is flush against side, because the sign is useful
    val direction = if(wall <2) 1 else -1
    val isVertical = wall % 2 == 0
    
    if(!isVertical) {
      val lims = (math.max(r0.rangeY._1,r1.rangeY._1),math.min(r0.rangeY._2,r1.rangeY._2))
      val xPos = if(direction==1) (r0.rangeX._2+1) else (r0.rangeX._1-1) 
      new Door(xPos,if(offset>0) (lims._1+offset-1) else (lims._2+offset+1))
    } else {
      val lims = (math.max(r0.rangeX._1,r1.rangeX._1),math.min(r0.rangeX._2,r1.rangeX._2))
      val yPos = if(direction==1) (r0.rangeY._2+1) else (r0.rangeY._1-1)
      new Door(if(offset>0) (lims._1+offset-1) else (lims._2+offset+1),yPos)
    }
  }
}
  

case class Door(x:Int,y:Int)


