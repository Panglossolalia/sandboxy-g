package autodraw

import java.awt.{ Graphics2D, Color, Stroke, BasicStroke}
import java.awt.Dimension
import java.awt.Point
import java.awt.geom.{ Rectangle2D, Ellipse2D }
import scala.swing.event._
import scala.swing.MainFrame
import scala.swing.Button
import scala.swing.SimpleSwingApplication
import scala.swing.Component
import scala.swing.event.MousePressed
import scala.swing.Panel
import org.apache.commons.math3.distribution._
import cltools.distributions._
import collection.mutable.{ HashMap, MultiMap, Set, PriorityQueue }
import scala.language.implicitConversions

/**
 * Some target examples to approximate:
 *  - The github octocat: https://octodex.github.com/
 *  - Hello kitty and other sanrio characters: https://en.wikipedia.org/wiki/List_of_Sanrio_characters
 *  - Van Beater character: https://www.google.com/search?q=van+beater&source=lnms&tbm=isch
 *  - turntable.fm avatars: https://www.google.com/search?q=turntable.fm+avatars&source=lnms&tbm=isch
 */
object Critter {
  
  val hCenter = 250.0

  def main(args: Array[String]): Unit = {
    import Color._

    val mainFrame = new MainFrame {
      contents = new CritterPanel(octocat)
    }
    mainFrame.pack
    mainFrame.visible = true
  }

  def octocat = {
    import Color._
    val headCenter = (250.0, 250.0)
    val pupilColor = new Color(172, 92, 81)
    val skinColor = new Color(244, 202, 177)
    val eyeDim = (69.0 / 2, 100.0 / 2)
    val pupilProp = .667
    val eyeSep = 186.0
    // actual octocat's eyes are slightly crossed
    val eyes2 = new NEPair(headCenter._2, eyeSep, eyeDim,Vector((WHITE, 1.0), (pupilColor, pupilProp)))
    // mouth is less than 180 degrees in actual octocat
    val mouth = new Mouth((headCenter._1,headCenter._2+66.7),(40,40),pupilColor)
    val nose = new Nose((headCenter._1,headCenter._2+43),(8,8),pupilColor)
    val head = new RrHead(headCenter._2+20,(360,200),skinColor) // size/shape are placeholders
    new Critter(Vector(head,mouth,nose,eyes2))
  }

}

class Critter(parts: IndexedSeq[CritterPart]) {

  def paintMe(g2: Graphics2D) = {
    for (cp <- parts) {
      cp.paintMe(g2)
    }
  }
}

trait CritterPart {
  implicit def i2d(d: Double) = { d.toInt }

  def paintMe(g2: Graphics2D)
  def center: (Double, Double)
  def size: (Double, Double)
}

class Eye(val center: (Double, Double), radii: (Double, Double)) extends CritterPart {
  def paintMe(g2: Graphics2D) = {
    g2.setColor(Color.white)
    g2.fillOval(center._1 - radii._1, center._2 - radii._2, radii._1*2, radii._2*2)
  }
  val size = (radii._1 * 2, radii._2 * 2)
}

class Nose(val center: (Double, Double), radii: (Double, Double), color: Color) extends CritterPart {
  def paintMe(g2: Graphics2D) = {
    g2.setColor(color)
    g2.fillOval(center._1 - radii._1, center._2 - radii._2, radii._1*2, radii._2*2)
  }
  val size = (radii._1 * 2, radii._2 * 2)
}

// Generally the outermost radius should be 1.0
class NestedEye(center: (Double, Double), radii: (Double, Double), colorRads: IndexedSeq[(Color, Double)]) extends Eye(center, radii) {
  override def paintMe(g2: Graphics2D) = {
    for (cr <- colorRads) {
      g2.setColor(cr._1)
      val currRad = (radii._1 * cr._2, radii._2 * cr._2)
      g2.fillOval(center._1 - currRad._1, center._2 - currRad._2, currRad._1*2, currRad._2*2)
    }
  }
}

class NEPair(y:Double, separation: Double, radii: (Double, Double), colorRads: IndexedSeq[(Color, Double)]) extends CritterPart {
  val eyes = Vector.tabulate(2)(i => {
    val sepDelta = separation* (i.toDouble - 1.0/2) 
    new NestedEye((Critter.hCenter+sepDelta,y),radii,colorRads)
  })
  def paintMe(g2: Graphics2D) = {
    eyes.foreach(e => e.paintMe(g2))
  }
  def center = (Critter.hCenter,y)
  def size = ((radii._1+separation)*2,radii._2*2)
}

class RrHead(y:Double, val size:(Double,Double), color: Color) extends CritterPart {

  
  def paintMe(g2: Graphics2D) = {
    g2.setColor(color)
    g2.fillRoundRect(Critter.hCenter-size._1/2, y-size._2/2, size._1, size._2, size._1/3, size._2/3)
  }
  
  def center = (Critter.hCenter,y)
  
}


// Note that the center is the cetner of the hypothetical circle, so for a smiley mouth it's actually the top
class Mouth(val center: (Double, Double), val size: (Double, Double), color: Color) extends CritterPart {

  def paintMe(g2: Graphics2D) = {
    val os = g2.getStroke
    g2.setStroke(new BasicStroke(7,1,1))
    g2.setColor(color)
    g2.drawArc(center._1 - (size._1 / 2), center._2 - (size._2 / 2), size._1, size._2, 180, 180)
    g2.setStroke(os)
  }

}

class CritterPanel(critter: Critter) extends scala.swing.Panel {
  preferredSize = new Dimension(500, 500);
  override def paintComponent(g: Graphics2D) = {
    super.paintComponent(g)
    critter.paintMe(g.asInstanceOf[Graphics2D])
  }
}