package mapgen

import javax.swing._
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom.Rectangle2D
import java.awt.Rectangle
import cltools.distributions._

object FloorPlans {
  
  def main(args:Array[String]):Unit = {
      testRandAdjoinDoors
  }
  
  def testVis = {
    val f = new FloorPlan
    f.addRoom(new Room((0,0),(1,1)))
    f.addRoom(new Room((0,0),(2,2)))
    f.addRoom(new Room((0,0),(3,3)))
    f.addRoom(new Room((1,1),(3,3)))
    println(f.rooms.size)
    f.showImage
  }
  
  def testAdjoin = {
    val f = new FloorPlan
    val ro = new Room((1,20),(1,20))
    f.addRoom(ro)
    val rn = ro.adjoiningRoom(0, 0, 1,  0, 0);
    f.addRoom(rn)
    f.addRoom(ro.adjoiningRoom(1, 0, 1, 0, 0));
    f.addRoom(ro.adjoiningRoom(2, 0, 1, 0, 0));
    f.addRoom(ro.adjoiningRoom(3, 0, 1, 0, 0));
    f.showImage
  }
  
  def testRandAdjoin = {
    def propose(f:FloorPlan) = {
      var sanity = 1000
      var found  = false
      var propRef = f.rooms(SimpleDist.rUniD(f.nRooms))
      var propSide = SimpleDist.rUniD(4)
      var largeEnough = if(propSide % 2 == 1) (propRef.size._2>2) else (propRef.size._1 > 2)
      
      while(!largeEnough && sanity > 0) {
        propRef = f.rooms(SimpleDist.rUniD(f.nRooms))
        propSide = SimpleDist.rUniD(4)
        largeEnough = if(propSide % 2 == 1) (propRef.size._2>2) else (propRef.size._1 > 2)
        sanity = sanity-1
      }
      if(sanity==0) throw new RuntimeException("Couldn't find a suitable reference room")
      val p = propRef.adjoiningRoom(propSide,1,2+SimpleDist.rUniD(20),SimpleDist.rUniD(3)-1,SimpleDist.rUniD(3)-1)
      p
    }
    
    def accept(r:Room,f:FloorPlan):Boolean = {
      !f.abutsExistingRoom(r,1)
    }
    
    val f = new FloorPlan
    f.addRoom(new Room((-5,5),(-5,5)))
    for(i <- 0 until 50000) {
      val p = propose(f)
      if(accept(p,f)) {
        f.addRoom(p)
      }
    }
    f.showImage
  }
  
  def testRandAdjoinDoors = {
    def propose(f:FloorPlan) = {
      var sanity = 1000
      var found  = false
      var propRef = f.rooms(SimpleDist.rUniD(f.nRooms))
      var propSide = SimpleDist.rUniD(4)
      var largeEnough = if(propSide % 2 == 1) (propRef.size._2>2) else (propRef.size._1 > 2)
      
      while(!largeEnough && sanity > 0) {
        propRef = f.rooms(SimpleDist.rUniD(f.nRooms))
        propSide = SimpleDist.rUniD(4)
        largeEnough = if(propSide % 2 == 1) (propRef.size._2>2) else (propRef.size._1 > 2)
        sanity = sanity-1
      }
      if(sanity==0) throw new RuntimeException("Couldn't find a suitable reference room")
      // Not rhyme or reason to types right now
      Room.nextRoomType = if(math.random < .2) Corridor else GenericRoom
      val (p,d) = propRef.adjoiningRoomWithDoor(propSide,2+SimpleDist.rUniD(20),SimpleDist.rUniD(3)-1,SimpleDist.rUniD(3)-1)
      (p,d)
    }
    
    def accept(r:Room,f:FloorPlan):Boolean = {
      !f.abutsExistingRoom(r,1)
    }
    
    val f = new FloorPlan
    f.addRoom(new Room((-5,5),(-5,5)))
    for(i <- 0 until 50000) {
      val (p,d) = propose(f)
      if(accept(p,f)) {
        f.addRoom(p)
        f.addDoor(d)
      }
    }
    f.showImage
  }
  
  def randomPlan = {
    val f = new FloorPlan
    var sanity=4500000
    var i = 0
    val nRooms = 500;
    val maxIdx = 200
    val minSz  = 2
    val maxSz  = 30
    
    f.addRoom(randomRoom(maxIdx,minSz,maxSz))
    while(sanity > 0 && i < nRooms) {
      sanity = sanity-1
       val r = randomRoom(maxIdx,minSz,maxSz)
       if(f.abutsExistingRoom(r,2) && !f.abutsExistingRoom(r,1)) {
         f.addRoom(r)
         i = i+1
       } else {
         // nothing
       }
    }
    println("sanity=%d, rooms created=%d".format(sanity,i))
    f
  }
  
  def randomRoom(maxIdx:Int,minSz:Int,maxSz:Int):Room = {
	   val sizeUb = maxSz-minSz;
	   val candidateStart = ((math.random*maxIdx).intValue(),(math.random*maxIdx).intValue())
       val candidateSize = ((math.random*sizeUb).intValue()+minSz,(math.random*sizeUb).intValue()+minSz)
       Room.fromOrigin(candidateStart._1, candidateStart._2, candidateSize._1, candidateSize._2)
  }
  
  /*
  def adjoiningRoom(ref:Room,wall:Int,originDepth:Int,endDepth:Int,originLateral:Int,endLateral:Int) = {
    assert(originDepth <= endDepth)
    val rSz=ref.size
    val endBuffer=1;
    val originBuffer=1
    val rOut:Room = wall match {
      case 0 => { // North
        new Room((ref.rangeX._1+originLateral,ref.rangeX._1+(rSz._1-endLateral-1)),(ref.rangeY._2+originDepth+originBuffer,ref.rangeY._2+endDepth+endBuffer))
      }
      case 1 => { // East
        new Room((ref.rangeX._2+originDepth+originBuffer,ref.rangeX._2+endDepth+endBuffer),(ref.rangeY._1+originLateral,ref.rangeY._1+(rSz._2-endLateral-1)))
      }
      case 2 => { // South
        Room.fromPoints((ref.rangeX._1+originLateral,ref.rangeY._1-originDepth-originBuffer),(ref.rangeX._1+(rSz._1-endLateral-1),ref.rangeY._1-endDepth-endBuffer))
      }
      case 3 => { // West 
        Room.fromPoints((ref.rangeX._1-originDepth-originBuffer,ref.rangeY._1+originLateral),(ref.rangeX._1-endDepth-endBuffer,ref.rangeY._1+(rSz._2-endLateral-1)))
      }
    }
    rOut
  }
* 
*/
}

class FloorPlan() {

  var rooms=Vector.empty[Room]
  var doors=Vector.empty[Door]
  
  def nRooms = rooms.size
  
  def addRoom(r:Room):Boolean = {
    if(canAdd(r)) {
    	rooms = rooms :+ r
    	true
    } else false
  }
  
  def addDoor(d:Door):Boolean = {
    doors = doors :+ d
    true
  }
    
  def canAdd(r0:Room):Boolean = {
    for(r1 <- rooms) {
      if(r0.overlaps(r1)) return false
    }
    return true
  }
  
  
  def toImage = {
    val mins = rooms.foldLeft((Int.MaxValue,Int.MaxValue))(
        (p0:(Int,Int),r:Room) => (math.min(p0._1,r.rangeX._1),math.min(p0._2,r.rangeY._1)))
    val maxes= rooms.foldLeft((Int.MinValue,Int.MinValue))(
        (p0:(Int,Int),r:Room) => (math.max(p0._1,r.rangeX._2+1),math.max(p0._2,r.rangeY._2+1)))
    val span = ((maxes._1-mins._1,maxes._2-mins._2))
    val newIm = new BufferedImage(span._1+2,span._2+2,BufferedImage.TYPE_INT_RGB)
    val g2 = newIm.createGraphics()
    g2.setColor(Color.WHITE)
    g2.fillRect(0, 0,newIm.getWidth(), newIm.getHeight())
    //g2.setColor(Color.RED)
    val offset = 1
    for(r <- rooms) {
      drawRoom(r,g2,mins,maxes)
    }
    for(d <- doors) {
      drawDoor(d,g2,mins,maxes)
    }
    newIm
  }
  
  def drawRoom(r:Room,g2:Graphics2D,mins:(Int,Int),maxes:(Int,Int)) = {
    val st = r.start
    val sz = r.size
    val roomRect = new Rectangle(st._1-mins._1, 
          maxes._2-st._2-sz._2,
          sz._1,
          sz._2)
    val roomColor = r.roomType match {
      case Corridor => Color.ORANGE
      case _ => Color.RED
    }
    g2.setColor(roomColor)
    g2.fill(roomRect)
  }
  
  def drawDoor(d:Door,g2:Graphics2D,mins:(Int,Int),maxes:(Int,Int)) = {
    val st = (d.x,d.y)
    val doorRect = new Rectangle(st._1-mins._1, 
          maxes._2-st._2-1,
          1,
          1)
    g2.setColor(Color.BLUE)
    g2.fill(doorRect)
  }
  
  def abutsExistingRoom(r0:Room,m:Int):Boolean = {
    for(r <- rooms) {
      if(r0.marginOverlap(r, m)) return true
    }
    return false
  }
  
  def showImage() = {
     val bim = toImage
     JOptionPane.showMessageDialog(null, new JLabel(new ImageIcon(bim)));
  }
    
}

